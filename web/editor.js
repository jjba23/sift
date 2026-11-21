/* sift --- the browser editor
 *
 * SPDX-License-Identifier: MIT
 * Copyright © Josep Bigorra <jjbigorra@gmail.com>
 *
 * The whole of the editor's knowledge about sift is this:
 *
 *   const [sift] = await Scheme.load_main("sift.wasm");
 *   const answer = JSON.parse(text(sift.call(JSON.stringify(request))[0]));
 *
 * one procedure, one JSON document in, one JSON document out.  Nothing here
 * knows how a `cond` clause is indented or what SIFT012 means; it asks, and
 * draws the answer.  That is the point of the port: the browser gets the same
 * formatter and the same 93 rules the command line has, not a
 * reimplementation of them.
 *
 * A Scheme string comes back *reflected* -- strings are mutable in Scheme, so
 * Hoot hands over an object rather than a JavaScript string -- and `text'
 * below is the one place that knows how to read it.
 */

// ---------------------------------------------------------------- the colour
//
// `highlight.js' is a module of its own, and it is loaded *dynamically* on
// purpose: a static `import' that fails takes the whole module with it, and
// then nothing on this page works -- no boot, no buttons, no findings, a
// status bar saying "loading" until the reader gives up.  That is one 404 away
// at all times: a deploy replaces the directory this file is served from, and
// a browser that asks for the two files in the wrong instant gets the old
// `editor.js' and no `highlight.js' at all.
//
// So the editor is written to work without it.  Until it arrives -- or if it
// never does -- the buffer is drawn as plain text, and when it loads the next
// keystroke colours it.  A page whose formatter works and whose colour is
// missing is a page; a page whose colour is missing and which therefore does
// nothing at all is the bug this arrangement exists to prevent.

let tokenize = null;

const highlighterUrl = new URL("highlight.js" + new URL(import.meta.url).search,
                               import.meta.url).href;

import(highlighterUrl)
  .then((module) => {
    tokenize = module.tokenize;
    drawHighlight();
  })
  .catch((error) => {
    // Saying nothing is the point: the editor works, and the reader has lost
    // colour rather than the tool.
    console.warn("sift: the highlighter did not load; the buffer is uncoloured", error);
  });

/** The buffer as HTML: tokens when the highlighter is here, text when it is not. */
function coloured(text, dialectName = dialect.value) {
  if (!tokenize) return escapeHtml(text);
  try {
    return tokenize(text, dialectName)
      .map((token) => (token.kind
        ? `<span class="t-${token.kind}">${escapeHtml(token.text)}</span>`
        : escapeHtml(token.text)))
      .join("");
  } catch (error) {
    // A tokenizer that throws is a tokenizer to stop using, not a page to lose.
    console.warn("sift: the highlighter failed; the buffer is uncoloured", error);
    tokenize = null;
    return escapeHtml(text);
  }
}

const escapeHtml = (text) =>
  String(text).replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");

// ---------------------------------------------------------------- the bridge

let sift = null;

const text = (value) =>
  typeof value === "string"
    ? value
    : value && value.reflector && typeof value.reflector.string_value === "function"
      ? value.reflector.string_value(value)
      : String(value);

/** Send one request to the wasm and return the parsed answer. */
function ask(request) {
  if (!sift) throw new Error("sift has not finished starting in this browser");
  const [answer] = sift.call(JSON.stringify(request));
  return JSON.parse(text(answer));
}

/** The answer, or an exception with sift's own message. */
function askOrThrow(request) {
  const answer = ask(request);
  if (!answer.ok) throw new Error(answer.error.message);
  return answer.result;
}


// ---------------------------------------------------------------- questions
//
// The four things an editor asks about a *place* -- what is this, where is it
// defined, what could I type, what does it take -- are answered by the same
// code the language server in sift answers with.  The answers carry LSP ranges
// (a line and a character, both zero based) because that is what they are, so
// the two conversions below are the whole of the translation.

function offsetOfLineCharacter(text, line, character) {
  const rows = text.split("\n");
  let offset = 0;
  for (let n = 0; n < line && n < rows.length; n += 1) offset += rows[n].length + 1;
  return Math.min(text.length, offset + character);
}

const caret = () => source.selectionStart;

let contextTimer = 0;
let completionItems = [];
let completionIndex = 0;
let completionPrefix = "";

/** Show what the symbol at the caret is, or the signature of the call around it. */
function describeCaret() {
  if (!sift || source.selectionStart !== source.selectionEnd) return;
  const offset = caret();
  let text = "";
  try {
    const hover = ask({ op: "hover", source: source.value, dialect: dialect.value, offset });
    if (hover.ok && hover.result.hover) {
      const contents = hover.result.hover.contents;
      text = typeof contents === "string" ? contents : contents && contents.value ? contents.value : "";
      text = text.split("\n").filter((line) => !line.startsWith("```")).join(" ");
    } else {
      const signature = ask({ op: "signature", source: source.value, dialect: dialect.value, offset });
      if (signature.ok && signature.result.signature) {
        const active = signature.result.signature;
        const label = active.label || (active.signatures && active.signatures[0] && active.signatures[0].label) || "";
        text = label;
      }
    }
  } catch (error) {
    text = "";
  }
  contextBar.textContent = text;
  contextBar.hidden = text === "";
}

function describeCaretSoon() {
  clearTimeout(contextTimer);
  contextTimer = setTimeout(describeCaret, 400);
}

/** Jump to the definition of the symbol at the caret, inside this buffer. */
function jumpToDefinition() {
  if (!sift) return;
  try {
    const answer = ask({
      op: "definition", source: source.value, dialect: dialect.value, offset: caret(),
    });
    const definitions = answer.ok ? answer.result.definitions : [];
    if (definitions.length === 0) {
      status.textContent = "no definition in this buffer";
      return;
    }
    const at = offsetOfLineCharacter(source.value, definitions[0].range.start.line,
                                     definitions[0].range.start.character);
    selectRange(at, at);
    status.textContent = `jumped to the definition at ${definitions[0].range.start.line + 1}:${definitions[0].range.start.character + 1}`;
    status.classList.remove("bad");
    describeCaret();
  } catch (error) {
    status.textContent = error.message;
    status.classList.add("bad");
  }
}

/** Offer the names the buffer defines that start with what is being typed. */
function complete() {
  if (!sift) return;
  const at = caret();
  const before = source.value.slice(0, at);
  const match = before.match(/[^\s()\[\]"';]+$/);
  completionPrefix = match ? match[0] : "";
  try {
    const answer = ask({
      op: "completion", source: source.value, dialect: dialect.value, offset: at,
    });
    const items = answer.ok ? answer.result.items : [];
    completionItems = items
      .filter((item) => item.label.startsWith(completionPrefix))
      .slice(0, 12);
    completionIndex = 0;
    drawCompletions();
  } catch (error) {
    status.textContent = error.message;
    status.classList.add("bad");
  }
}

function drawCompletions() {
  if (completionItems.length === 0) {
    completions.hidden = true;
    completions.innerHTML = "";
    return;
  }
  completions.innerHTML = completionItems
    .map((item, index) => `
      <li role="option" data-index="${index}" class="${index === completionIndex ? "active" : ""}">
        <span class="label">${escape2(item.label)}</span>
        <span class="kind">${completionKind(item.kind)}</span>
      </li>`)
    .join("");
  completions.hidden = false;
}

function completionKind(kind) {
  return { 3: "function", 12: "function", 13: "variable", 8: "macro", 6: "field" }[kind] || "name";
}

function acceptCompletion() {
  const item = completionItems[completionIndex];
  if (!item) return;
  const at = caret();
  source.setSelectionRange(at - completionPrefix.length, at);
  source.setRangeText(item.label, at - completionPrefix.length, at, "end");
  hideCompletions();
  drawHighlight();
  drawLines();
  lintSoon();
  describeCaretSoon();
}

function hideCompletions() {
  completionItems = [];
  completions.hidden = true;
  completions.innerHTML = "";
}

/** Select a range, scroll it into view and focus the buffer. */
function selectRange(from, to) {
  const rows = source.value.split("\n");
  let line = 1;
  let seen = 0;
  for (let n = 0; n < rows.length; n += 1) {
    if (seen + rows[n].length + 1 > from) { line = n + 1; break; }
    seen += rows[n].length + 1;
    line = n + 2;
  }
  source.focus();
  source.setSelectionRange(from, to);
  const height = source.scrollHeight / Math.max(1, rows.length);
  source.scrollTop = Math.max(0, (line - 3) * height);
  drawHighlight();
  drawLines();
}

// ---------------------------------------------------------------- the panel
//
// One panel at a time: what sift found in the buffer, the outline of the buffer,
// or the rules of the build.  It is one function because the three lists are
// three ways of saying "here is what I found", and a page where each pair of
// show/hide calls was written on its own is a page that ends up showing two of
// them at once -- an outline sitting on top of a list of rules, which is what a
// reader saw when the rules were opened and then the outline.

const panels = {
  findings: { head: document.getElementById("findings-head"),
              list: document.getElementById("finding-list"), button: null },
  outline: { head: document.getElementById("outline-head"),
             list: document.getElementById("outline-list"),
             button: document.getElementById("outline") },
  rules: { head: document.getElementById("rules-head"),
           list: document.getElementById("rule-list"),
           button: document.getElementById("rules") },
};

let visiblePanel = "findings";

/** Show @var{name} and hide the rest. */
function showPanel(name) {
  visiblePanel = name;
  for (const [key, panel] of Object.entries(panels)) {
    const shown = key === name;
    panel.head.hidden = !shown;
    panel.list.hidden = !shown;
    if (panel.button) panel.button.setAttribute("aria-expanded", shown ? "true" : "false");
  }
}

/** True when @var{name} should now open: it was not the panel on screen. */
function openingPanel(name) {
  if (visiblePanel === name) {
    showPanel("findings");
    return false;
  }
  return true;
}

// ---------------------------------------------------------------- the outline

async function showOutline() {
  if (!openingPanel("outline")) return;
  // Asked for before the panel is shown, so a failure leaves the findings where
  // they were and puts the reason in the status bar rather than in an empty panel.
  const result = askOrThrow({ op: "symbols", source: source.value, dialect: dialect.value });
  const symbols = result.symbols;
  document.getElementById("outline-count").textContent = String(symbols.length);
  outlineList.innerHTML = symbols.map((symbol) => `
      <li class="outline-row" data-line="${symbol.selectionRange.start.line}"
          data-character="${symbol.selectionRange.start.character}">
        <code class="rule">${symbol.kind}</code>
        <span class="name">${escape2(symbol.name)}</span>
        <span class="where">${symbol.selectionRange.start.line + 1}</span>
        ${symbol.detail
          ? `<p class="message"><code class="detail">${coloured(symbol.detail)}</code></p>`
          : ""}
      </li>`).join("");
  showPanel("outline");
}

// ---------------------------------------------------------------- the buffer

const source = document.getElementById("source");
const highlight = document.getElementById("highlight");
const lines = document.getElementById("lines");
const findings = document.getElementById("finding-list");
const ruleList = document.getElementById("rule-list");
const outlineList = document.getElementById("outline-list");
const contextBar = document.getElementById("context");
const completions = document.getElementById("completions");
const count = document.getElementById("count");
const status = document.getElementById("status");
const dialect = document.getElementById("dialect");
const style = document.getElementById("style");
const severity = document.getElementById("severity");

let currentFindings = [];

const request = (op, extra) => ({
  op,
  source: source.value,
  dialect: dialect.value,
  style: style.value,
  severity: severity.value,
  label: "buffer." + (dialect.value === "elisp" ? "el"
    : dialect.value === "cl" ? "lisp"
    : dialect.value === "racket" ? "rkt"
    : "scm"),
  ...extra,
});

// ------------------------------------------------------- highlighting

/* The colour itself lives in `highlight.js', which is a tokenizer that knows the
 * four dialects and nothing else -- it does not decide what a form means, and it
 * has no opinion about indentation, because sift has an opinion about that and
 * it is in the wasm.  Here is the whole of the arrangement: the same text goes
 * into a `<pre>' under the textarea, and the two scroll together. */

function drawHighlight() {
  highlight.innerHTML = coloured(source.value);
  highlight.scrollTop = source.scrollTop;
  highlight.scrollLeft = source.scrollLeft;
}

function drawLines() {
  const total = source.value.split("\n").length;
  const flagged = new Set(currentFindings.map((finding) => finding.line));
  let out = "";
  for (let n = 1; n <= total; n += 1) {
    out += `<div class="line${flagged.has(n) ? " flagged" : ""}">${n}</div>`;
  }
  lines.innerHTML = out;
  lines.scrollTop = source.scrollTop;
}

// ------------------------------------------------------- findings

const RANK = { error: 0, warning: 1, info: 2 };

function showFindings(list) {
  currentFindings = list;
  count.textContent = list.length === 0 ? "clean" : String(list.length);
  findings.innerHTML = list
    .slice()
    .sort((a, b) => a.line - b.line || a.column - b.column || RANK[a.severity] - RANK[b.severity])
    .map(
      (finding) => `
      <li class="finding sev-${finding.severity}" data-line="${finding.line}" data-column="${finding.column}">
        <div class="finding-head">
          <code class="rule">${escape2(finding.code || "reader")}</code>
          <span class="name">${escape2(finding.rule)}</span>
          <span class="where">${finding.line}:${finding.column}</span>
        </div>
        <p class="message">${escape2(finding.message)}</p>
      </li>`,
    )
    .join("");
  drawLines();
}

function escape2(value) {
  return String(value).replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
}

function jumpTo(line, column) {
  const rows = source.value.split("\n");
  let offset = 0;
  for (let n = 0; n < line - 1 && n < rows.length; n += 1) offset += rows[n].length + 1;
  offset += Math.max(0, column - 1);
  source.focus();
  source.setSelectionRange(offset, offset);
  // Put the line at the top of the view.
  const height = source.scrollHeight / Math.max(1, rows.length);
  source.scrollTop = Math.max(0, (line - 3) * height);
  drawHighlight();
  drawLines();
}

// ------------------------------------------------------- the operations

/** The counts of a lint, as one sentence, with how long it took. */
function countsSentence(counts, started) {
  return `${counts.error} errors, ${counts.warning} warnings, ${counts.info} notes`
    + ` — ${Math.round(performance.now() - started)} ms`;
}

/** Lint the buffer, and say what @var{note} did first when there is a note.
 *
 * The note is what a *button* did -- "formatted in 12 ms", "already formatted,
 * nothing to change" -- and it is composed here rather than assigned before the
 * call because the lint has a sentence of its own, and the two used to fight:
 * the format message was written and then overwritten, so a reader who pressed
 * Format on an already-formatted buffer saw the counts change by nothing and had
 * no way to tell a no-op from a broken button.
 */
function lint(note = "") {
  try {
    const started = performance.now();
    const result = askOrThrow(request("lint"));
    showFindings(result.warnings);
    const counts = countsSentence(result.counts, started);
    status.textContent = note ? `${note} — ${counts}` : counts;
    status.classList.remove("bad");
  } catch (error) {
    status.textContent = error.message;
    status.classList.add("bad");
  }
}

let timer = 0;
function lintSoon() {
  clearTimeout(timer);
  timer = setTimeout(lint, 300);
}

function replaceBuffer(text) {
  if (text === source.value) return;
  const at = source.selectionStart;
  source.value = text;
  const clamped = Math.min(at, text.length);
  source.setSelectionRange(clamped, clamped);
  drawHighlight();
  drawLines();
}

function format() {
  try {
    const started = performance.now();
    const result = askOrThrow(request("format"));
    replaceBuffer(result.source);
    showFindings(result.warnings);
    status.classList.remove("bad");
    lint(result.changed
      ? `formatted in ${Math.round(performance.now() - started)} ms`
      : "already formatted, so nothing changed");
  } catch (error) {
    status.textContent = error.message;
    status.classList.add("bad");
  }
}

/** Extract the expression at the caret, and put the rewritten buffer back. */
function extract(to) {
  try {
    const started = performance.now();
    const result = askOrThrow({
      op: "extract",
      source: source.value,
      dialect: dialect.value,
      offset: caret(),
      to,
    });
    replaceBuffer(result.source);
    status.classList.remove("bad");
    lint(result.changed
      ? `${result.title} in ${Math.round(performance.now() - started)} ms`
      : "nothing to extract here");
  } catch (error) {
    status.textContent = error.message;
    status.classList.add("bad");
  }
}

function fix() {
  try {
    const started = performance.now();
    const result = askOrThrow(request("fix"));
    replaceBuffer(result.source);
    showFindings(result.warnings);
    const left = result.warnings.length;
    status.textContent =
      result.applied === 0
        ? `nothing to fix — ${left} finding${left === 1 ? "" : "s"} left`
        : `${result.applied} fix${result.applied === 1 ? "" : "es"} applied in ${Math.round(performance.now() - started)} ms`
          + ` — ${left} finding${left === 1 ? "" : "s"} left`;
    status.classList.remove("bad");
  } catch (error) {
    status.textContent = error.message;
    status.classList.add("bad");
  }
}

async function showRules() {
  if (!openingPanel("rules")) return;
  const result = askOrThrow({ op: "checkers" });
  document.getElementById("rule-count").textContent = String(result.checkers.length);
  ruleList.innerHTML = result.checkers
    .map((checker) => {
      // A rule that applies to one dialect is written in that dialect, so its
      // example is coloured by it: an Emacs Lisp rule whose example is coloured
      // as Scheme is the same mistake as opening a Racket buffer on a Scheme
      // example, one panel further in.
      const exampleDialect = (checker.dialects && checker.dialects[0]) || "scheme";
      const dialects = (checker.dialects || []).join(" ");
      // `expected' is the rule's fix as the catalogue writes it: sometimes the
      // text the finding becomes, sometimes a sentence about the change
      // ("delete `car` from the `only` clause").  So it is shown as prose rather
      // than coloured as code -- a sentence coloured like a form would be the
      // page claiming to have read something it has not.
      const fix = checker.expected
        ? `<p class="message fix"><span class="label">fix</span> ${escape2(checker.expected)}</p>`
        : "";
      return `
      <li class="rule-row">
        <div class="rule-head">
          <code class="rule">${escape2(checker.code)}</code>
          <span class="name">${escape2(checker.name)}</span>
          <span class="dialects">${escape2(dialects)}</span>
          <span class="sev sev-${checker.severity}">${escape2(checker.severity)}</span>
        </div>
        <p class="message">${escape2(checker.description)}</p>
        <pre class="example" data-label="a finding">${coloured(checker.example, exampleDialect)}</pre>
        ${fix}
      </li>`;
    })
    .join("");
  showPanel("rules");
}

// ------------------------------------------------------- the example
//
// The page opens on an example, and the example is the dialect's: a buffer
// written in Scheme beside a selector that says Racket teaches the wrong
// language.  The text comes from sift -- `{"op":"sample"}' -- rather than from
// a copy here, so there is one set of examples, and `tests/web.scm' is what
// keeps each of them formatted and worth looking at.

let example = "";

/** Put the current dialect's example in the buffer. */
function loadExample() {
  if (!sift) return;
  try {
    example = askOrThrow({ op: "sample", dialect: dialect.value }).source;
    source.value = example;
    source.setSelectionRange(0, 0);
    source.scrollTop = 0;
    hideCompletions();
    contextBar.hidden = true;
    drawHighlight();
    drawLines();
    lint();
  } catch (error) {
    status.textContent = error.message;
    status.classList.add("bad");
  }
}

/** True while the buffer still holds an example, or nothing at all. */
function untouched() {
  return source.value === example || source.value.trim() === "";
}

// ------------------------------------------------------- the theme
//
// Two themes, and three ways to choose one: `?theme=' in the URL, then the
// choice a reader made last time, then what the system says.  The first wins,
// because a link that names a theme is a link someone meant to send.  Every
// colour is a variable in `editor.css', so a theme is one attribute on the root
// element and the page is otherwise the same page.

const THEME_KEY = "sift-theme";
const themeButton = document.getElementById("theme");

function systemTheme() {
  return window.matchMedia && window.matchMedia("(prefers-color-scheme: light)").matches
    ? "light"
    : "dark";
}

/** The remembered theme, or null when this tab cannot remember one. */
function rememberedTheme() {
  try {
    return localStorage.getItem(THEME_KEY);
  } catch (error) {
    return null;
  }
}

function rememberTheme(theme) {
  try {
    localStorage.setItem(THEME_KEY, theme);
  } catch (error) {
    // remembered for this tab only, which is what having no storage means
  }
}

/** Make THEME the one on screen, and say which one the button switches to. */
function showTheme(theme) {
  const next = theme === "light" ? "dark" : "light";
  document.documentElement.setAttribute("data-theme", theme);
  themeButton.textContent = next === "light" ? "☀" : "☾";
  themeButton.title = `switch to the ${next} theme`;
  themeButton.setAttribute("aria-label", `switch to the ${next} theme`);
  themeButton.setAttribute("aria-pressed", theme === "light" ? "true" : "false");
}

themeButton.addEventListener("click", () => {
  const current = document.documentElement.getAttribute("data-theme");
  const next = current === "light" ? "dark" : "light";
  showTheme(next);
  rememberTheme(next);
});

/** The theme to open on: the link, the memory, the system, in that order. */
function preferredTheme() {
  const named = new URLSearchParams(location.search).get("theme");
  if (named === "light" || named === "dark") return named;
  const remembered = rememberedTheme();
  if (remembered === "light" || remembered === "dark") return remembered;
  return systemTheme();
}

// ------------------------------------------------------- wiring

showTheme(preferredTheme());

// A link can name a language as well as a theme, which is how one example is
// shared with someone else: `?dialect=racket'.  One that names nothing sift
// reads is ignored rather than refused -- the selector still works.
const linkedDialect = new URLSearchParams(location.search).get("dialect");
if (linkedDialect && Array.from(dialect.options).some((option) => option.value === linkedDialect)) {
  dialect.value = linkedDialect;
}

source.addEventListener("input", () => {
  drawHighlight();
  drawLines();
  lintSoon();
});
source.addEventListener("scroll", () => {
  highlight.scrollTop = source.scrollTop;
  highlight.scrollLeft = source.scrollLeft;
  lines.scrollTop = source.scrollTop;
});
source.addEventListener("keydown", (event) => {
  if (event.key === "Tab") {
    event.preventDefault();
    source.setRangeText("  ", source.selectionStart, source.selectionEnd, "end");
    drawHighlight();
    lintSoon();
  }
  if (event.ctrlKey && event.shiftKey && event.key.toLowerCase() === "f") {
    event.preventDefault();
    format();
  }
  if (event.ctrlKey && event.shiftKey && event.key.toLowerCase() === "x") {
    event.preventDefault();
    fix();
  }
  if (event.ctrlKey && event.shiftKey && event.key.toLowerCase() === "e") {
    event.preventDefault();
    extract("let");
  }
  if (event.ctrlKey && event.shiftKey && event.key.toLowerCase() === "r") {
    event.preventDefault();
    extract("procedure");
  }
  if (event.key === "F12") {
    event.preventDefault();
    jumpToDefinition();
  }
  if (event.ctrlKey && event.key === " ") {
    event.preventDefault();
    complete();
  }
  if (!completions.hidden) {
    if (event.key === "ArrowDown" || event.key === "ArrowUp") {
      event.preventDefault();
      const step = event.key === "ArrowDown" ? 1 : -1;
      completionIndex = (completionIndex + step + completionItems.length) % completionItems.length;
      drawCompletions();
    } else if (event.key === "Enter" || event.key === "Tab") {
      event.preventDefault();
      acceptCompletion();
    } else if (event.key === "Escape") {
      event.preventDefault();
      hideCompletions();
    }
  }
});

source.addEventListener("click", (event) => {
  if (event.ctrlKey || event.metaKey) jumpToDefinition();
});

for (const select of [style, severity]) select.addEventListener("change", lint);
// A dialect is a different language, so it is a different example -- unless the
// buffer holds someone's work, which is never thrown away for a click.
dialect.addEventListener("change", () => (untouched() ? loadExample() : lint()));
// A dialect is also a way of colouring the buffer, so switching one repaints it
// even when the text stays: an Emacs Lisp `?a' is a character where a Scheme
// `?a' is a name, and the page should not keep saying otherwise.
dialect.addEventListener("change", drawHighlight);
document.getElementById("example").addEventListener("click", loadExample);
document.getElementById("format").addEventListener("click", format);
document.getElementById("fix").addEventListener("click", fix);
document.getElementById("rules").addEventListener("click", () => showRules().catch((error) => {
  status.textContent = error.message;
  status.classList.add("bad");
}));
document.getElementById("outline").addEventListener("click", () => showOutline().catch((error) => {
  status.textContent = error.message;
  status.classList.add("bad");
}));

outlineList.addEventListener("click", (event) => {
  const row = event.target.closest(".outline-row");
  if (!row) return;
  const at = offsetOfLineCharacter(source.value, Number(row.dataset.line),
                                   Number(row.dataset.character));
  selectRange(at, at);
  describeCaret();
});

completions.addEventListener("mousedown", (event) => {
  const item = event.target.closest("li[data-index]");
  if (!item) return;
  event.preventDefault();
  completionIndex = Number(item.dataset.index);
  acceptCompletion();
});

source.addEventListener("click", () => {
  describeCaretSoon();
});
source.addEventListener("keyup", (event) => {
  if (["ArrowUp", "ArrowDown", "Enter", "Escape"].includes(event.key) && !completions.hidden) return;
  clearTimeout(timer);
  timer = setTimeout(lint, 300);
  describeCaretSoon();
});
source.addEventListener("blur", () => setTimeout(hideCompletions, 150));

findings.addEventListener("click", (event) => {
  const item = event.target.closest(".finding");
  if (item) jumpTo(Number(item.dataset.line), Number(item.dataset.column));
});

// ------------------------------------------------------- boot

// The page does not say where sift.wasm is; this module's own URL does.  The
// editor is served from a directory and not from the root of a site --
// https://jointhefreeworld.org/sift/ is a directory beside a blog -- so
// nothing here may name a path from `/`.  `import.meta.url` is the URL this
// module was loaded from, which makes "sift.wasm beside me" the answer wherever
// the editor is mounted.
//
// Its query string is the build stamp that web/build.sh wrote into index.html,
// carried on to the wasm: these files are served with a long cache, and a URL
// that changes when the build does is what makes a deploy visible without
// asking a reader to reload with the cache held down.  `reflect.wasm' and
// `wtf8.wasm' are Hoot's and are named by Hoot relative to a directory, so the
// directory is what is handed over -- without a trailing slash, because Hoot
// joins the two with one.
const moduleUrl = new URL(import.meta.url);
const wasmUrl = new URL("sift.wasm" + moduleUrl.search, moduleUrl).href;
const reflectDir = new URL(".", moduleUrl).href.replace(/\/$/, "");

(async () => {
  // A boot that never finishes is the failure a reader cannot diagnose: the
  // status bar keeps saying "loading" and every button does nothing.  This says
  // so after ten seconds -- a browser without the WebAssembly this needs, a
  // deploy caught between two files -- and leaves the buffer readable, because
  // an editor that can only be read is still better than a page that looks
  // broken.
  const watchdog = setTimeout(() => {
    if (sift) return;
    status.textContent =
      "sift is still starting after ten seconds — this browser may not support the"
      + " WebAssembly it needs, and the editor works for reading until it does";
    status.classList.add("bad");
  }, 10000);
  try {
    const started = performance.now();
    [sift] = await Scheme.load_main(wasmUrl, { reflect_wasm_dir: reflectDir });
    const version = askOrThrow({ op: "version" }).version;
    status.classList.remove("bad");
    status.textContent = `sift ${version} — loaded in ${Math.round(performance.now() - started)} ms`;
    loadExample();
  } catch (error) {
    status.textContent = `sift could not start: ${error.message}`;
    status.classList.add("bad");
    source.readOnly = true;
  } finally {
    clearTimeout(watchdog);
  }
})();
