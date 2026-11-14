// sift --- the VS Code side of it
//
// SPDX-License-Identifier: MIT
// Copyright © Josep Bigorra <jjbigorra@gmail.com>
//
// The extension is a pipe and nothing else: every answer comes from the
// executable, which is what keeps the editor and the pipeline saying the same
// thing.  There is no build step and no dependency -- the file you are reading
// is the file that runs -- so `vsce package' on a checkout produces the
// extension with nothing installed first.
//
// What it gives you:
//
//   diagnostics   sift lint --format=json, on open, on change or on save
//   quickfixes    sift fix --checker=<the rule that fired>, as a code action
//   formatting    sift format, as "Format Document"
//
// One deliberate difference from the Emacs and Neovim integrations: those use
// `sift lsp', the language server, which reports the fix for the exact span it
// belongs to.  This extension runs the command, because VS Code has no
// language-client without a dependency to bundle -- so a quickfix here runs
// `sift fix' for that one rule and rewrites the smallest range that differs.
// The result is the same edit.

'use strict';

const vscode = require('vscode');
const { execFile } = require('child_process');
const { minimalEdit, dialectFor } = require('./edit');

// The extensions this extension answers for.  The dialect of one is in
// `edit.js`, next to the arithmetic, so that it can be tested without VS Code.
const DIALECTS = ['scm', 'ss', 'sld', 'sls', 'rkt', 'rktd', 'rktl', 'scrbl',
  'el', 'lisp', 'cl', 'asd'];

/** The document filter every provider here is registered with. */
const SELECTOR = { scheme: 'file', pattern: '**/*.{scm,ss,sld,sls,rkt,rktd,rktl,scrbl,el,lisp,cl,asd}' };

/** The diagnostics of each open document, by URI. */
const collections = new Map();

/** A pending lint per document, so that typing does not start a queue. */
const timers = new Map();

function configuration() {
  return vscode.workspace.getConfiguration('sift');
}

function dialectOf(document) {
  const chosen = configuration().get('dialect');
  if (chosen && chosen !== 'auto') {
    return chosen;
  }
  return dialectFor(document.fileName, 'scheme');
}

/**
 * Run sift with `arguments`, feeding it `source` on standard input.
 *
 * Answers `{ status, stdout, stderr }`; a status above 1 is a usage error and
 * not a finding, and the caller says so instead of showing it as one.
 */
function run(arguments_, source) {
  const executable = configuration().get('executable');
  return new Promise((resolve) => {
    const child = execFile(
      executable,
      arguments_,
      { maxBuffer: 32 * 1024 * 1024 },
      (error, stdout, stderr) => {
        const status = error ? (typeof error.code === 'number' ? error.code : 2) : 0;
        resolve({ status, stdout, stderr });
      }
    );
    if (child.stdin) {
      child.stdin.end(source);
    }
  });
}

function diagnosticsFor(document) {
  const dialect = dialectOf(document);
  const label = document.isUntitled ? '<untitled>' : document.fileName;
  return run(
    ['lint', '--stdin', '--format=json', `--severity=${configuration().get('severity')}`,
      `--dialect=${dialect}`, `--label=${label}`],
    document.getText()
  ).then((result) => {
    if (result.status > 1) {
      return [];
    }
    let findings;
    try {
      findings = JSON.parse(result.stdout);
    } catch (error) {
      return [];
    }
    return findings.map((finding) => {
      const range = new vscode.Range(
        finding.line - 1, finding.column - 1, finding.line - 1, finding.column
      );
      const severity = {
        error: vscode.DiagnosticSeverity.Error,
        warning: vscode.DiagnosticSeverity.Warning,
        info: vscode.DiagnosticSeverity.Information,
      }[finding.severity] || vscode.DiagnosticSeverity.Information;
      const diagnostic = new vscode.Diagnostic(
        range, finding.message, severity
      );
      diagnostic.code = finding.code;
      diagnostic.source = 'sift';
      // The rule's name is what a quickfix needs: it is the argument
      // `sift fix --checker' takes.
      diagnostic.data = { checker: finding.checker, code: finding.code };
      return diagnostic;
    });
  });
}

function lint(document) {
  if (!document || document.languageId === 'Log') {
    return;
  }
  const key = document.uri.toString();
  let collection = collections.get(key);
  if (!collection) {
    collection = vscode.languages.createDiagnosticCollection('sift');
    collections.set(key, collection);
  }
  return diagnosticsFor(document).then((diagnostics) => {
    collection.set(document.uri, diagnostics);
  });
}

function lintLater(document) {
  if (configuration().get('lintOn') === 'change') {
    const key = document.uri.toString();
    clearTimeout(timers.get(key));
    timers.set(key, setTimeout(() => lint(document), 400));
  }
}

/**
 * Return the edit that turns `original` into `updated`, as a VS Code range and
 * a replacement: the arithmetic is in `edit.js`, and this is the one line that
 * turns a plain position into the editor's own type.
 */
function editFor(original, updated) {
  const edit = minimalEdit(original, updated);
  return {
    range: new vscode.Range(
      new vscode.Position(edit.start.line, edit.start.character),
      new vscode.Position(edit.end.line, edit.end.character)
    ),
    text: edit.text,
  };
}

/** Apply `sift fix` for one rule, and answer the edit that would do it. */
function fixEdit(document, checker) {
  const dialect = dialectOf(document);
  const label = document.isUntitled ? '<untitled>' : document.fileName;
  return run(
    ['fix', '--stdin', `--dialect=${dialect}`, `--label=${label}`,
      `--checker=${checker}`],
    document.getText()
  ).then((result) => {
    if (result.status > 1 || !result.stdout) {
      return null;
    }
    return editFor(document.getText(), result.stdout);
  });
}

function activate(context) {
  const register = (disposable) => context.subscriptions.push(disposable);

  function matches(document) {
    const extension = document.fileName.split('.').pop().toLowerCase();
    return DIALECTS.includes(extension);
  }

  function lintDocument(document) {
    if (document && matches(document)) {
      lint(document);
    }
  }

  for (const document of vscode.workspace.textDocuments) {
    lintDocument(document);
  }

  register(vscode.workspace.onDidOpenTextDocument(lintDocument));
  register(vscode.workspace.onDidChangeTextDocument((event) => {
    if (matches(event.document)) {
      lintLater(event.document);
    }
  }));
  register(vscode.workspace.onDidSaveTextDocument(lintDocument));
  register(vscode.workspace.onDidCloseTextDocument((document) => {
    const key = document.uri.toString();
    const collection = collections.get(key);
    if (collection) {
      collection.dispose();
      collections.delete(key);
    }
  }));

  register(vscode.commands.registerCommand('sift.lint', () =>
    lint(vscode.window.activeTextEditor && vscode.window.activeTextEditor.document)));

  register(vscode.commands.registerCommand('sift.fix', async () => {
    const editor = vscode.window.activeTextEditor;
    if (!editor) {
      return;
    }
    const result = await run(
      ['fix', '--stdin', `--dialect=${dialectOf(editor.document)}`,
        `--label=${editor.document.fileName}`],
      editor.document.getText()
    );
    if (result.status > 1) {
      vscode.window.showErrorMessage(`sift: ${result.stderr.trim()}`);
      return;
    }
    const edit = editFor(editor.document.getText(), result.stdout);
    await editor.edit((builder) => builder.replace(edit.range, edit.text));
    lint(editor.document);
  }));

  register(vscode.commands.registerCommand('sift.explain', async () => {
    const editor = vscode.window.activeTextEditor;
    if (!editor) {
      return;
    }
    const line = editor.selection.active.line;
    const collection = collections.get(editor.document.uri.toString());
    const diagnostic = collection &&
      collection.get(editor.document.uri).find((candidate) => candidate.range.start.line === line);
    if (!diagnostic || !diagnostic.data) {
      vscode.window.showInformationMessage('sift: no finding on this line.');
      return;
    }
    const result = await run(['explain', diagnostic.data.checker], '');
    const document = await vscode.workspace.openTextDocument({
      content: result.stdout,
      language: 'text',
    });
    await vscode.window.showTextDocument(document, { preview: true });
  }));

  register(vscode.languages.registerCodeActionsProvider(SELECTOR, {
    provideCodeActions(document, range, context) {
      const actions = [];
      for (const diagnostic of context.diagnostics) {
        if (!diagnostic.data || !diagnostic.data.checker) {
          continue;
        }
        const action = new vscode.CodeAction(
          `sift: fix ${diagnostic.data.code} [${diagnostic.data.checker}]`,
          vscode.CodeActionKind.QuickFix
        );
        action.diagnostics = [diagnostic];
        action.isPreferred = true;
        actions.push(action);
      }
      if (actions.length === 0) {
        return [];
      }
      return Promise.all(actions.map((action) =>
        fixEdit(document, action.diagnostics[0].data.checker).then((edit) => {
          if (!edit) {
            return null;
          }
          action.edit = new vscode.WorkspaceEdit();
          action.edit.replace(document.uri, edit.range, edit.text);
          return action;
        })
      )).then((resolved) => resolved.filter(Boolean));
    },
  }, { providedCodeActionKinds: [vscode.CodeActionKind.QuickFix] }));

  register(vscode.languages.registerDocumentFormattingEditProvider(SELECTOR, {
    provideDocumentFormattingEdits(document) {
      return run(
        ['format', '--stdin', `--dialect=${dialectOf(document)}`,
          `--label=${document.fileName}`],
        document.getText()
      ).then((result) => {
        if (result.status !== 0) {
          if (result.stderr) {
            vscode.window.showErrorMessage(`sift: ${result.stderr.trim()}`);
          }
          return [];
        }
        if (result.stdout === document.getText()) {
          return [];
        }
        const whole = new vscode.Range(
          new vscode.Position(0, 0),
          document.positionAt(document.getText().length)
        );
        return [vscode.TextEdit.replace(whole, result.stdout)];
      });
    },
  }));

  register(vscode.workspace.onWillSaveTextDocument((event) => {
    if (configuration().get('formatOnSave') && matches(event.document)) {
      event.waitUntil(
        vscode.commands.executeCommand('editor.action.formatDocument')
      );
    }
  }));
}

function deactivate() {
  for (const collection of collections.values()) {
    collection.dispose();
  }
  collections.clear();
  for (const timer of timers.values()) {
    clearTimeout(timer);
  }
  timers.clear();
}

module.exports = { activate, deactivate };
