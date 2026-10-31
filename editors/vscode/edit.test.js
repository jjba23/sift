// sift --- tests for the extension's arithmetic
//
// SPDX-License-Identifier: MIT
// Copyright © Josep Bigorra <jjbigorra@gmail.com>
//
// Run with `node editors/vscode/edit.test.js` (or `maak editors-check`).  No
// framework: Node's own `assert` is the assertion library, which is why this
// file needs nothing installed.

'use strict';

const assert = require('assert');
const { positionAt, minimalEdit, dialectFor } = require('./edit.js');

let failures = 0;
let run = 0;

function test(name, body) {
  run += 1;
  try {
    body();
    console.log(`  ok    ${name}`);
  } catch (error) {
    failures += 1;
    console.log(`  FAIL  ${name}`);
    console.log(`        ${error.message.split('\n').join('\n        ')}`);
  }
}

// --- positions ---------------------------------------------------------

test('the first character is line 0, column 0', () => {
  assert.deepStrictEqual(positionAt('abc', 0), { line: 0, character: 0 });
});

test('a position on the first line is its offset', () => {
  assert.deepStrictEqual(positionAt('abc\ndef', 2), { line: 0, character: 2 });
});

test('a position after a newline starts the next line', () => {
  assert.deepStrictEqual(positionAt('abc\ndef', 4), { line: 1, character: 0 });
  assert.deepStrictEqual(positionAt('abc\ndef', 6), { line: 1, character: 2 });
});

test('the end of the text is the end of the last line', () => {
  assert.deepStrictEqual(positionAt('abc\ndef\n', 8), { line: 2, character: 0 });
});

test('an offset past the end is clamped to the end', () => {
  assert.deepStrictEqual(positionAt('abc', 99), { line: 0, character: 3 });
});

test('a negative offset is clamped to the start', () => {
  assert.deepStrictEqual(positionAt('abc', -5), { line: 0, character: 0 });
});

test('a multi-line string counts its lines', () => {
  const text = '(a)\n(b)\n(c)\n';
  assert.deepStrictEqual(positionAt(text, text.length), { line: 3, character: 0 });
});

// --- minimal edits -----------------------------------------------------

test('two identical texts produce an empty edit', () => {
  const edit = minimalEdit('(if a b)\n', '(if a b)\n');
  assert.strictEqual(edit.text, '');
  assert.deepStrictEqual(edit.start, edit.end);
});

test('a changed word replaces the word, not the file', () => {
  const original = '(if (ready?) (go))\n';
  const updated = '(when (ready?) (go))\n';
  const edit = minimalEdit(original, updated);
  assert.deepStrictEqual(edit.start, { line: 0, character: 1 });
  assert.deepStrictEqual(edit.end, { line: 0, character: 3 });
  assert.strictEqual(edit.text, 'when');
});

test('trailing whitespace is removed by an edit at its own column', () => {
  const original = '(define (f x)   \n  x)\n';
  const updated = '(define (f x)\n  x)\n';
  const edit = minimalEdit(original, updated);
  assert.deepStrictEqual(edit.start, { line: 0, character: 13 });
  assert.deepStrictEqual(edit.end, { line: 0, character: 16 });
  assert.strictEqual(edit.text, '');
});

test('a change on a later line keeps the lines before it', () => {
  const original = '(a)\n(if b c)\n(d)\n';
  const updated = '(a)\n(when b c)\n(d)\n';
  const edit = minimalEdit(original, updated);
  assert.deepStrictEqual(edit.start, { line: 1, character: 1 });
  assert.deepStrictEqual(edit.end, { line: 1, character: 3 });
  assert.strictEqual(edit.text, 'when');
});

test('an insertion is one empty range and the text to add', () => {
  const edit = minimalEdit('(a)\n', '(a)\n(b)\n');
  assert.deepStrictEqual(edit.start, { line: 1, character: 0 });
  assert.deepStrictEqual(edit.end, { line: 1, character: 0 });
  assert.strictEqual(edit.text, '(b)\n');
});

test('a deletion is one range and no text', () => {
  const edit = minimalEdit('(a)\n(b)\n', '(a)\n');
  assert.strictEqual(edit.text, '');
  assert.deepStrictEqual(edit.start, { line: 1, character: 0 });
  assert.deepStrictEqual(edit.end, { line: 2, character: 0 });
});

test('applying the edit to the original gives the updated text', () => {
  const pairs = [
    ['(if a b)\n', '(when a b)\n'],
    ['(a)\n(b)\n', '(a)\n'],
    ['(a)\n', '(a)\n(b)\n(if c d)\n'],
    ['\n\n', ''],
  ];
  for (const [original, updated] of pairs) {
    const edit = minimalEdit(original, updated);
    const startOffset = offsetOf(original, edit.start);
    const endOffset = offsetOf(original, edit.end);
    const applied = original.slice(0, startOffset) + edit.text + original.slice(endOffset);
    assert.strictEqual(applied, updated, `${JSON.stringify(original)} -> ${JSON.stringify(updated)}`);
  }
});

/** Return the offset of a position, for checking an edit by applying it. */
function offsetOf(text, position) {
  const lines = text.split('\n');
  let offset = 0;
  for (let line = 0; line < position.line; line += 1) {
    offset += lines[line].length + 1;
  }
  return offset + position.character;
}

// --- dialects ----------------------------------------------------------

test('the extension of a file names its dialect', () => {
  assert.strictEqual(dialectFor('a.scm'), 'scheme');
  assert.strictEqual(dialectFor('a.sld'), 'scheme');
  assert.strictEqual(dialectFor('a.el'), 'elisp');
  assert.strictEqual(dialectFor('a.lisp'), 'cl');
  assert.strictEqual(dialectFor('a.asd'), 'cl');
});

test('a file with no extension falls back to the setting', () => {
  assert.strictEqual(dialectFor('Makefile', 'elisp'), 'elisp');
  assert.strictEqual(dialectFor('Makefile'), 'scheme');
});

test('a name with dots keeps its last extension', () => {
  assert.strictEqual(dialectFor('my.module.el'), 'elisp');
});

console.log(`\n${run - failures}/${run} passed`);
process.exit(failures === 0 ? 0 : 1);
