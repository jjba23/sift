" sift --- linting, fixing and formatting from Vim
"
" SPDX-License-Identifier: MIT
" Copyright © Josep Bigorra <jjbigorra@gmail.com>
"
" Copy this directory to ~/.vim (or point a plugin manager at it) and the
" commands below appear for Scheme, Racket, Emacs Lisp and Common Lisp
" buffers.
"
"   :SiftLint      fill the quickfix list with sift's findings
"   :SiftFormat    format the buffer, keeping the cursor where it was
"   :SiftFix       apply every fix sift has for the buffer
"   :SiftExplain   print what the rule on this line wants
"
" Neovim users have a language server instead: see neovim.lua in this
" directory, which gives diagnostics, code actions and formatting through
" `sift lsp'.

if exists('g:loaded_sift')
  finish
endif
let g:loaded_sift = 1

let s:save_cpo = &cpo
set cpo&vim

augroup sift
  autocmd!
  autocmd FileType scheme,racket,elisp,lisp command! -buffer -bar SiftLint call sift#lint()
  autocmd FileType scheme,racket,elisp,lisp command! -buffer -bar SiftFormat call sift#format()
  autocmd FileType scheme,racket,elisp,lisp command! -buffer -bar SiftFix call sift#fix()
  autocmd FileType scheme,racket,elisp,lisp command! -buffer -bar SiftExplain call sift#explain()
augroup END

let &cpo = s:save_cpo
unlet s:save_cpo
