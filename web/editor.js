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

// ---------------------------------------------------------------- the outline

async function showOutline() {
  const head = document.getElementById("outline-head");
  const button = document.getElementById("outline");
  const findingsHead = document.getElementById("findings-head");
  const findingList = document.getElementById("finding-list");
  const rulesHead = document.getElementById("rules-head");
  if (!outlineList.hidden) {
    outlineList.hidden = true;
    head.hidden = true;
    findingsHead.hidden = false;
    findingList.hidden = false;
    button.setAttribute("aria-expanded", "false");
    return;
  }
  const result = askOrThrow({ op: "symbols", source: source.value, dialect: dialect.value });
  const symbols = result.symbols;
  document.getElementById("outline-count").textContent = String(symbols.length);
  outlineList.innerHTML = symbols.map((symbol, index) => `
      <li class="outline-row" data-line="${symbol.selectionRange.start.line}"
          data-character="${symbol.selectionRange.start.character}">
        <code class="rule">${symbol.kind}</code>
        <span class="name">${escape2(symbol.name)}</span>
        <span class="where">${symbol.selectionRange.start.line + 1}</span>
        ${symbol.detail ? `<p class="message">${escape2(symbol.detail)}</p>` : ""}
      </li>`).join("");
  outlineList.hidden = false;
  head.hidden = false;
  findingsHead.hidden = true;
  findingList.hidden = true;
  rulesHead.hidden = true;
  button.setAttribute("aria-expanded", "true");
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

/* A Scheme reader, near enough for colour: comments, strings, character
 * literals, delimiters, numbers and the quote prefixes.  It is a tokenizer and
 * not a formatter -- sift has one of those, and it is in the wasm -- so it
 * only has to agree with the text, never with the indentation. */
const DELIMITERS = new Set(["(", ")", "[", "]", "{", "}"]);
const PREFIXES = new Set(["'", "`"]);

function escape(character) {
  return character === "&" ? "&amp;" : character === "<" ? "&lt;" : character === ">" ? "&gt;" : character;
}

function highlightScheme(code) {
  let out = "";
  let i = 0;
  const push = (kind, text) => {
    out += kind ? `<span class="t-${kind}">${Array.from(text, escape).join("")}</span>` : Array.from(text, escape).join("");
  };
  while (i < code.length) {
    const character = code[i];
    const next = code[i + 1];

    if (character === ";") {
      const end = code.indexOf("\n", i);
      const stop = end === -1 ? code.length : end;
      push("comment", code.slice(i, stop));
      i = stop;
    } else if (character === "#" && next === "|") {
      const end = code.indexOf("|#", i + 2);
      const stop = end === -1 ? code.length : end + 2;
      push("comment", code.slice(i, stop));
      i = stop;
    } else if (character === '"') {
      let j = i + 1;
      while (j < code.length) {
        if (code[j] === "\\") { j += 2; continue; }
        if (code[j] === '"') { j += 1; break; }
        j += 1;
      }
      push("string", code.slice(i, j));
      i = j;
    } else if (character === "#" && next === "\\") {
      let j = i + 2;
      while (j < code.length && !/[\s()\[\]";]/.test(code[j])) j += 1;
      push("char", code.slice(i, j));
      i = j;
    } else if (DELIMITERS.has(character)) {
      push("paren", character);
      i += 1;
    } else if (PREFIXES.has(character) || (character === "," && next === "@") || character === ",") {
      const width = character === "," && next === "@" ? 2 : 1;
      push("quote", code.slice(i, i + width));
      i += width;
    } else if (/\s/.test(character)) {
      let j = i;
      while (j < code.length && /\s/.test(code[j])) j += 1;
      push(null, code.slice(i, j));
      i = j;
    } else {
      let j = i;
      while (j < code.length && !/[\s()\[\]";]/.test(code[j])) j += 1;
      const word = code.slice(i, j);
      if (/^-?(\d+\.?\d*|\.\d+)([eE][-+]?\d+)?$/.test(word)) push("number", word);
      else if (word.startsWith(":") || word.startsWith("#:")) push("keyword", word);
      else push("symbol", word);
      i = j;
    }
  }
  return out;
}

function drawHighlight() {
  highlight.innerHTML = highlightScheme(source.value);
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

function lint() {
  try {
    const started = performance.now();
    const result = askOrThrow(request("lint"));
    showFindings(result.warnings);
    const counts = result.counts;
    status.textContent =
      `${counts.error} errors, ${counts.warning} warnings, ${counts.info} notes` +
      ` — ${Math.round(performance.now() - started)} ms`;
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
    status.textContent = result.changed
      ? `formatted in ${Math.round(performance.now() - started)} ms`
      : "already formatted";
    status.classList.remove("bad");
    lint();
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
    status.textContent = result.changed
      ? `${result.title} in ${Math.round(performance.now() - started)} ms`
      : "nothing to extract here";
    status.classList.remove("bad");
    lint();
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
    status.textContent =
      result.applied === 0
        ? "nothing to fix"
        : `${result.applied} fix${result.applied === 1 ? "" : "es"} applied in ${Math.round(performance.now() - started)} ms`;
    status.classList.remove("bad");
  } catch (error) {
    status.textContent = error.message;
    status.classList.add("bad");
  }
}

async function showRules() {
  const head = document.getElementById("rules-head");
  const findingsHead = document.getElementById("findings-head");
  const findingList = document.getElementById("finding-list");
  const button = document.getElementById("rules");
  if (!ruleList.hidden) {
    ruleList.hidden = true;
    head.hidden = true;
    findingsHead.hidden = false;
    findingList.hidden = false;
    button.setAttribute("aria-expanded", "false");
    return;
  }
  const result = askOrThrow({ op: "checkers" });
  document.getElementById("rule-count").textContent = String(result.checkers.length);
  ruleList.innerHTML = result.checkers
    .map(
      (checker) => `
      <li class="rule-row">
        <div class="rule-head">
          <code class="rule">${escape2(checker.code)}</code>
          <span class="name">${escape2(checker.name)}</span>
          <span class="sev sev-${checker.severity}">${escape2(checker.severity)}</span>
        </div>
        <p class="message">${escape2(checker.description)}</p>
        <pre class="example">${escape2(checker.example)}</pre>
      </li>`,
    )
    .join("");
  ruleList.hidden = false;
  head.hidden = false;
  findingsHead.hidden = true;
  findingList.hidden = true;
  button.setAttribute("aria-expanded", "true");
}

// ------------------------------------------------------- wiring

const sample = `(define (classify n)
  (cond ((< n 0) 'negative)
        ((= n 0) 'zero)
        (else 'positive)))

(define (report n)
  (if (ready?)
      (proceed)))
`;

source.value = sample;

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

for (const select of [dialect, style, severity]) select.addEventListener("change", lint);
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
  try {
    const started = performance.now();
    [sift] = await Scheme.load_main(wasmUrl, { reflect_wasm_dir: reflectDir });
    const version = askOrThrow({ op: "version" }).version;
    status.textContent = `sift ${version} — loaded in ${Math.round(performance.now() - started)} ms`;
    drawHighlight();
    drawLines();
    lint();
  } catch (error) {
    status.textContent = `sift could not start: ${error.message}`;
    status.classList.add("bad");
    source.readOnly = true;
  }
})();
