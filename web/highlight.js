/* sift --- syntax colour for the browser editor
 *
 * SPDX-License-Identifier: MIT
 * Copyright © Josep Bigorra <jjbigorra@gmail.com>
 *
 * Why this is JavaScript and not the reader in the wasm: colour is redrawn on
 * every keystroke, and a read of the buffer costs a JSON round trip into a
 * Scheme reader that allocates four nodes for every character -- 780 ms on six
 * kilobytes, which is a keystroke nobody would type twice.  What the wasm is
 * asked for is the *answers* -- the findings, the fixes, the formatting -- and
 * those are the things sift is the only implementation of.  This is a tokenizer
 * and not a reader: it classifies text, it never decides what the text means,
 * and it never disagrees with the reader about indentation because it has no
 * opinion about indentation.
 *
 * Two things it must never do, and `tests/web.scm` runs it under node to say
 * so: lose a character (the tokens of a buffer, concatenated, are the buffer)
 * and mistake a datum for prose.  `#;(a b)` comments out one datum, not the
 * rest of the line; `#| a #| b |# c |#` nests; `?\(` is a character and not an
 * unclosed list; `#\(` likewise; and a `;` inside a string is a semicolon.
 *
 * A dialect is not a decoration here.  Emacs Lisp writes a character as `?a`
 * and a keyword as `:key`; Common Lisp does the same and spells a float `1.5d0`;
 * Scheme and Racket write `#\a` and `#:key`; Racket opens with `#lang`, which is
 * a declaration and not a symbol.  Colouring those alike would be colouring a
 * reader's language wrong, which is the one thing this page is for.
 */

/* Every kind of token the highlighter colours, and therefore every `--t-<kind>`
 * colour `editor.css` has to declare.  `tests/web.scm' fails when the two lists
 * disagree: a token with no colour is text the reader cannot see, and a colour
 * nothing produces is a rule that has been dead since it was written. */
export const tokenKinds = [
  "comment",
  "string",
  "char",
  "number",
  "keyword",
  "boolean",
  "symbol",
  "paren",
  "quote",
];

// What ends an atom: whitespace, a delimiter, a string, a comment, a quote
// prefix or a comma.  Anything else is part of the name being read.
const ATOM_END = /[\s()[\]{}"'`,;]/;

const DELIMITERS = "()[]{}";
const OPENERS = "([{";
const CLOSERS = ")]}";
const QUOTES = "'`";

const isSpace = (character) => /\s/.test(character);
const isDigit = (character) => character >= "0" && character <= "9";
const isAlphaNumeric = (character) => /[A-Za-z0-9]/.test(character);

// An integer, a ratio, a decimal with an optional exponent, and the exponent
// letters Common Lisp adds to the last two.  `#x1f' and `#e1.5' are numbers by
// their radix or their exactness, which is why they are tested separately.
const RATIO = /^[+-]?\d+\/\d+$/;
const DECIMAL = /^[+-]?(?:\d+\.?\d*|\.\d+)(?:[eE][+-]?\d+)?(?:[dDfFlLsS](?:[+-]?\d+)?)?$/;
const RADIX = /^#[xXoObBdDeEiI][0-9A-Za-z.+\/-]+$/;
const INFINITY = /^[+-](?:inf|nan)\.0$/;

const isNumber = (word) =>
  RATIO.test(word) || DECIMAL.test(word) || RADIX.test(word) || INFINITY.test(word);

/** The kind of an atom, given the dialect it was written in. */
function classify(word, dialect) {
  if (isNumber(word)) return "number";
  if (word === "#lang" || word === "#langs") return "keyword";
  if (word === "#t" || word === "#f" || word === "#true" || word === "#false") return "boolean";
  if (word.startsWith("#:") && (dialect === "scheme" || dialect === "racket")) return "keyword";
  if (word.startsWith(":") && (dialect === "elisp" || dialect === "cl")) return "keyword";
  if ((dialect === "elisp" || dialect === "cl") && (word === "nil" || word === "t")) {
    return "boolean";
  }
  return "symbol";
}

// ---------------------------------------------------------------- the readers
//
// One per shape of token, each answering the index just past what it read.  A
// shape that never closes -- a string whose end is off the screen, an
// unterminated block comment -- answers the end of the buffer, which is what an
// editor shows: the rest of the text is what the reader is inside of.

function readLineComment(code, start) {
  const end = code.indexOf("\n", start);
  return end === -1 ? code.length : end;
}

function readBlockComment(code, start) {
  let depth = 0;
  let i = start;
  while (i < code.length) {
    if (code[i] === "#" && code[i + 1] === "|") {
      depth += 1;
      i += 2;
    } else if (code[i] === "|" && code[i + 1] === "#") {
      depth -= 1;
      i += 2;
      if (depth === 0) return i;
    } else {
      i += 1;
    }
  }
  return code.length;
}

function readString(code, start) {
  let i = start + 1;
  while (i < code.length) {
    if (code[i] === "\\") {
      i += 2;
      continue;
    }
    if (code[i] === '"') return i + 1;
    i += 1;
  }
  return code.length;
}

/** A character literal: `#\a' in three dialects, `?a' in Emacs Lisp. */
function readCharLiteral(code, start, dialect) {
  let i = start + 1;
  if (code[start] === "#") i = start + 2;
  if (i >= code.length) return code.length;
  if (dialect === "elisp") {
    // `?\C-x' is a name with modifiers in it; `?\(', `?\ ' and `?\;' are one
    // character wearing a backslash, so the run may be empty -- and when it is,
    // the character after the backslash is the one to take.
    if (code[i] === "\\") {
      i += 1;
      while (i < code.length && !ATOM_END.test(code[i])) i += 1;
      if (i < code.length && i === start + 2) i += 1;
      return i;
    }
    return i + 1;
  }
  // Scheme, Racket and Common Lisp: a named character is a run of letters and
  // digits, and anything else is exactly one character.
  if (isAlphaNumeric(code[i])) {
    while (i < code.length && /[A-Za-z0-9-]/.test(code[i])) i += 1;
    return i;
  }
  return i + 1;
}

/** Return the index just past the list whose opening delimiter is at START. */
function readList(code, start, dialect) {
  let depth = 0;
  let i = start;
  while (i < code.length) {
    const character = code[i];
    if (character === '"') {
      i = readString(code, i);
    } else if (character === "#" && code[i + 1] === "|") {
      i = readBlockComment(code, i);
    } else if (character === "#" && code[i + 1] === "\\") {
      i = readCharLiteral(code, i, dialect);
    } else if (dialect === "elisp" && character === "?" && isAtomStart(code, i)) {
      i = readCharLiteral(code, i, dialect);
    } else if (OPENERS.includes(character)) {
      depth += 1;
      i += 1;
    } else if (CLOSERS.includes(character)) {
      depth -= 1;
      i += 1;
      if (depth === 0) return i;
    } else {
      i += 1;
    }
  }
  return code.length;
}

/** True when the character at START begins an atom rather than continuing one. */
function isAtomStart(code, start) {
  return start === 0 || ATOM_END.test(code[start - 1]);
}

/** Return the index just past the one datum that starts at START, or is after it. */
function readDatum(code, start, dialect) {
  let i = start;
  while (i < code.length && isSpace(code[i])) i += 1;
  if (i >= code.length) return code.length;
  const character = code[i];
  const next = code[i + 1];
  if (character === '"') return readString(code, i);
  if (character === "#" && next === "|") return readBlockComment(code, i);
  if (character === "#" && next === "\\") return readCharLiteral(code, i, dialect);
  if (character === "#" && next === ";") return readDatum(code, readDatum(code, i + 2, dialect), dialect);
  if (dialect === "elisp" && character === "?" && isAtomStart(code, i)) {
    return readCharLiteral(code, i, dialect);
  }
  if (QUOTES.includes(character)) return readDatum(code, i + 1, dialect);
  if (character === ",") return readDatum(code, i + (next === "@" ? 2 : 1), dialect);
  if (character === "#" && "'`,".includes(next)) {
    return readDatum(code, i + (code[i + 2] === "@" ? 3 : 2), dialect);
  }
  if (OPENERS.includes(character)) return readList(code, i, dialect);
  if (character === "#" && OPENERS.includes(next)) return readList(code, i + 1, dialect);
  if (character === "#" && /[uU]/.test(next) && code[i + 2] === "8" && code[i + 3] === "(") {
    return readList(code, i + 3, dialect);
  }
  let j = i;
  while (j < code.length && !ATOM_END.test(code[j])) j += 1;
  return Math.max(j, i + 1);
}

// ---------------------------------------------------------------- the tokens

/** Return CODE as a list of `{kind, text}`, for the DIALECT it is written in.
 *
 * A token's kind is one of `tokenKinds`, or `null` for the text between them --
 * the whitespace an overlay has to reproduce exactly and has no colour for.
 * Concatenated, the tokens are the buffer, character for character.
 */
export function tokenize(code, dialect = "scheme") {
  const tokens = [];
  const push = (kind, text) => {
    if (text) tokens.push({ kind, text });
  };
  let i = 0;
  while (i < code.length) {
    const character = code[i];
    const next = code[i + 1];

    if (isSpace(character)) {
      let j = i + 1;
      while (j < code.length && isSpace(code[j])) j += 1;
      push(null, code.slice(i, j));
      i = j;
    } else if (character === ";" || (i === 0 && character === "#" && next === "!")) {
      // A `#!' line is a shebang, which is a comment about the file's first
      // line rather than a token of the language in it.
      const end = readLineComment(code, i);
      push("comment", code.slice(i, end));
      i = end;
    } else if (character === "#" && next === "|") {
      const end = readBlockComment(code, i);
      push("comment", code.slice(i, end));
      i = end;
    } else if (character === "#" && next === ";") {
      // `#;' comments out the datum after it, not the rest of the line.
      const end = readDatum(code, i + 2, dialect);
      push("comment", code.slice(i, end));
      i = end;
    } else if (character === '"') {
      const end = readString(code, i);
      push("string", code.slice(i, end));
      i = end;
    } else if (character === "#" && next === "\\") {
      const end = readCharLiteral(code, i, dialect);
      push("char", code.slice(i, end));
      i = end;
    } else if (dialect === "elisp" && character === "?" && isAtomStart(code, i)) {
      const end = readCharLiteral(code, i, dialect);
      push("char", code.slice(i, end));
      i = end;
    } else if (character === "#" && OPENERS.includes(next)) {
      push("paren", code.slice(i, i + 2));
      i += 2;
    } else if (character === "#" && /[uU]/.test(next) && code[i + 2] === "8" && code[i + 3] === "(") {
      push("paren", code.slice(i, i + 4));
      i += 4;
    } else if (QUOTES.includes(character)) {
      push("quote", character);
      i += 1;
    } else if (character === ",") {
      const width = next === "@" ? 2 : 1;
      push("quote", code.slice(i, i + width));
      i += width;
    } else if (character === "#" && "'`,".includes(next)) {
      const width = code[i + 2] === "@" ? 3 : 2;
      push("quote", code.slice(i, i + width));
      i += width;
    } else if (character === "#" && (next === "+" || next === "-" || next === "." || next === "&")) {
      // A feature conditional, a read-time form or a box: reader syntax that
      // introduces what follows rather than being part of it.
      push("quote", code.slice(i, i + 2));
      i += 2;
    } else if (DELIMITERS.includes(character)) {
      push("paren", character);
      i += 1;
    } else {
      let j = i;
      while (j < code.length && !ATOM_END.test(code[j])) j += 1;
      if (j === i) j = i + 1;
      const word = code.slice(i, j);
      push(classify(word, dialect), word);
      i = j;
    }
  }
  return tokens;
}

const escapeHtml = (text) =>
  text.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");

/** Return CODE as HTML: one `span' per coloured token, the spacing as it was.
 *
 * The answer is what goes inside the highlight layer, so it must be the text of
 * the buffer and nothing else -- an entity for a bracket's own `<', and no
 * whitespace added anywhere, because the layer sits exactly under the textarea.
 */
export function highlight(code, dialect = "scheme") {
  let out = "";
  for (const token of tokenize(code, dialect)) {
    out += token.kind
      ? `<span class="t-${token.kind}">${escapeHtml(token.text)}</span>`
      : escapeHtml(token.text);
  }
  return out;
}
