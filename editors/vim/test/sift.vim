" sift --- tests for the Vim plugin, run in batch
"
" SPDX-License-Identifier: MIT
"
"   vim -es -u NORC -N -S editors/vim/test/sift.vim
"
" The exit status is the answer: 0 when every assertion held, 1 when one did
" not.  `maak editors-check' runs it, and the CI job runs `maak editors-check'.
"
" Everything here goes through the executable, the way the plugin does: a test
" that called the autoload functions with a stubbed sift would test the stub.

" The plugin is found by this file's own path rather than by the invocation:
" a runtimepath entry that is relative stops meaning anything as soon as a test
" changes directory, and an autoload that disappears mid-run looks like a
" plugin that does not work.
let s:root = expand('<sfile>:p:h:h:h:h')
execute 'set runtimepath^=' . fnameescape(s:root . '/editors/vim')
runtime! plugin/sift.vim

" The checkout's own executable, by its documented setting: the test should
" not depend on which sift happens to be installed on the machine running it.
let g:sift_executable = s:root . '/bin/sift'

set nomore
set noswapfile

let s:failures = []
let s:passed = 0

function! s:check(condition, name) abort
  if a:condition
    let s:passed += 1
  else
    call add(s:failures, a:name)
  endif
endfunction

function! s:write(name, lines) abort
  call writefile(a:lines, join([tempname(), a:name], '-'))
endfunction

" The sift a plugin run finds is the one on the PATH, and the configuration it
" reads is the one in the directory it is run from.  A scratch directory keeps
" this test's answer from depending on the project it is run inside.
let s:scratch = tempname()
call mkdir(s:scratch, 'p')
execute 'cd ' . fnameescape(s:scratch)

" The buffer every test starts from: a fixable `if`, trailing whitespace, and a
" function whose second argument is not used.
let s:source = [
      \ '(define (f x y)   ',
      \ '  (if (ready?)',
      \ '      (go)))',
      \ ]

" --- :SiftLint fills the quickfix list from the JSON ----------------

enew!
call setline(1, s:source)
set filetype=scheme
SiftLint

let s:items = getqflist()
call s:check(len(s:items) >= 3, 'SiftLint finds at least three findings')
call s:check(!empty(filter(copy(s:items),
      \ 'v:val.user_data ==# "if-without-else"')),
      \ 'a finding carries the checker name a fix needs')
call s:check(!empty(filter(copy(s:items), 'v:val.type ==# "W"')),
      \ 'a warning is a warning in the list')
call s:check(!empty(filter(copy(s:items),
      \ 'v:val.text =~# "\\[SIFT"')),
      \ 'a finding is named by its rule code')

" A clean buffer empties the list rather than keeping the old one.
enew!
call setline(1, ['(define (f x)', '  (+ x 1))'])
set filetype=scheme
SiftLint
call s:check(empty(getqflist()), 'a clean buffer has an empty list')

" --- :SiftFormat rewrites the buffer -------------------------------

enew!
call setline(1, ['(define (f x)', '(+ x 1))'])
set filetype=scheme
SiftFormat
call s:check(getline(1, '$') == ['(define (f x)', '  (+ x 1))'],
      \ 'SiftFormat indents the body')

" --- :SiftFix applies the fixes --------------------------------------

enew!
call setline(1, s:source)
set filetype=scheme
SiftFix
call s:check(index(getline(1, '$'), '(define (f x y)') >= 0,
      \ 'SiftFix removes the trailing whitespace')
call s:check(!empty(filter(getline(1, '$'), 'v:val =~# "when"')),
      \ 'SiftFix turns the if into a when')

" --- a buffer that does not parse is a message, not a crash ---------

enew!
call setline(1, ['(define (f x)'])
set filetype=scheme
try
  SiftLint
  call s:check(1, 'a broken buffer is linted')
catch
  call s:check(0, 'a broken buffer is linted: ' . v:exception)
endtry
call s:check(!empty(getqflist()),
      \ 'a broken buffer still has findings')

" --- :SiftExplain shows the rule, in a buffer of its own ------------

enew!
call setline(1, ['(define (f x)', '  (if (ready?)', '      (go))'])
set filetype=scheme
SiftLint
cclose
call cursor(2, 4)
SiftExplain
call s:check(getline(1) ==# 'CODE', 'SiftExplain opens the rule rather than writing in the file')
call s:check(index(getline(1, '$'), 'SIFT012') >= 0,
      \ 'SiftExplain names the rule that fired')
call s:check(!&modifiable, 'and the page it opens is not for editing')

" --- the compiler plugin ---------------------------------------------

enew!
call setline(1, s:source)
set filetype=scheme
compiler sift
call s:check(&makeprg ==# 'sift ci', 'the compiler sets makeprg')
call s:check(&errorformat =~# 'warning', 'and an errorformat that parses a finding')

" --- the answer ------------------------------------------------------

if !empty(s:failures)
  call writefile(s:failures + [printf('%d/%d checks passed', s:passed,
        \ s:passed + len(s:failures))], '/dev/stderr')
  qall!
endif

call writefile([printf('%d/%d checks passed', s:passed, s:passed)], '/dev/stdout')
qall!
