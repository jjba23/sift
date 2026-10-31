" sift --- a :compiler plugin for Vim
"
" SPDX-License-Identifier: MIT
" Copyright © Josep Bigorra <jjbigorra@gmail.com>
"
" `:compiler sift' then `:make` runs `sift ci` over the current file and fills
" the quickfix list from its output.  The errorformat below is the one-line
" report sift prints -- `file:line:column: severity: [checker] message' -- and
" nothing else matches, so the list holds sift's findings and not whatever else
" ended up in the buffer.
"
" `sift ci` asks both questions, so a file that is not formatted is reported
" too; `let b:sift_makeprg = 'check'` before `:make` asks only that one.

if exists('current_compiler')
  finish
endif
let current_compiler = 'sift'

let s:save_cpo = &cpo
set cpo&vim

let s:command = get(b:, 'sift_makeprg', 'ci')

execute 'CompilerSet makeprg=' . escape('sift ' . s:command, ' \')

CompilerSet errorformat=
      \%f:%l:%c:\ error:\ [%*[^]]]\ %m,
      \%f:%l:%c:\ warning:\ [%*[^]]]\ %m,
      \%f:%l:%c:\ info:\ [%*[^]]]\ %m,
      \%-G%.%#

let &cpo = s:save_cpo
unlet s:save_cpo
