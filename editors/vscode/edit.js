// sift --- the two pieces of arithmetic the extension does
//
// SPDX-License-Identifier: MIT
//
// Kept out of extension.js, and out of the `vscode' module, so that they can be
// tested by running Node: an extension is hard to test and a pure function is
// not, and these are the only parts of the extension where a mistake is
// invisible — an off-by-one in a range is a fix applied to the wrong line.

'use strict';

/**
 * Return the position of `offset` in `text`, counted the way the editor
 * counts: lines from zero, characters from zero.
 */
function positionAt(text, offset) {
  let clamped = Math.max(0, Math.min(offset, text.length));
  const before = text.slice(0, clamped);
  const lines = before.split('\n');
  return { line: lines.length - 1, character: lines[lines.length - 1].length };
}

/**
 * Return the smallest replacement that turns `original` into `updated`.
 *
 * The common prefix and the common suffix are trimmed, so a fix that changes
 * one word replaces one word: the whole document is never rewritten for a
 * change the author can see, and an editor keeps the cursor and the undo
 * history of the lines that did not change.
 */
function minimalEdit(original, updated) {
  let start = 0;
  const shortest = Math.min(original.length, updated.length);
  while (start < shortest && original[start] === updated[start]) {
    start += 1;
  }
  let end = original.length;
  let endUpdated = updated.length;
  while (end > start && endUpdated > start &&
         original[end - 1] === updated[endUpdated - 1]) {
    end -= 1;
    endUpdated -= 1;
  }
  return {
    start: positionAt(original, start),
    end: positionAt(original, end),
    text: updated.slice(start, endUpdated),
  };
}

/**
 * Return the dialect for a file name, or `fallback` when the name says
 * nothing.  The extension's `sift.dialect` setting is the first word; this is
 * what answers when it is `auto`.
 */
function dialectFor(fileName, fallback) {
  const dialects = {
    scm: 'scheme', ss: 'scheme', sld: 'scheme', sls: 'scheme',
    el: 'elisp',
    lisp: 'cl', cl: 'cl', asd: 'cl',
  };
  const extension = String(fileName).split('.').pop().toLowerCase();
  return dialects[extension] || fallback || 'scheme';
}

module.exports = { positionAt, minimalEdit, dialectFor };
