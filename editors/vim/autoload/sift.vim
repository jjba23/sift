" sift --- the Vim side of it
"
" SPDX-License-Identifier: MIT
" Copyright © Josep Bigorra <jjbigorra@gmail.com>
"
" Every function here runs the executable with the buffer on standard input
" and reads what comes back, which is what sift was built to be called that
" way: nothing is written, nothing is saved, and no second implementation of
" the dialect rules lives in Vimscript.

let s:sift = get(g:, 'sift_executable', 'sift')

function! s:dialect() abort
  if &filetype ==# 'scheme'
    return 'scheme'
  elseif &filetype ==# 'elisp'
    return 'elisp'
  elseif &filetype ==# 'lisp'
    return 'cl'
  endif
  return ''
endfunction

function! s:run(arguments, lines) abort
  "Run sift with ARGUMENTS and LINES on standard input; answer [status, out, err].
  "
  " The command is a string because Vim's `system()' takes one, and the
  " redirection is there because there is no second output port: stderr goes
  " to a file that is read and removed, so a usage error is a message and not
  " a mystery.
  let l:errors = tempname()
  let l:command = join(map([s:sift] + a:arguments, 'shellescape(v:val)'), ' ')
  let l:output = systemlist(l:command . ' 2>' . shellescape(l:errors),
        \ join(a:lines, "\n") . "\n")
  let l:status = v:shell_error
  let l:stderr = filereadable(l:errors) ? readfile(l:errors) : []
  call delete(l:errors)
  return [l:status, l:output, l:stderr]
endfunction

function! sift#lint(...) abort
  "Put sift's findings for this buffer in the quickfix list.
  "
  " With a 0 for the first argument the list is filled without opening the
  " window: a fix should leave the cursor where the author put it, and a
  " window that jumps in front of the buffer on every `:SiftFix' is a window
  " that has to be closed again every time.
  let l:open = a:0 > 0 ? a:1 : 1"
  let l:dialect = s:dialect()
  if empty(l:dialect)
    echoerr 'sift: no dialect for ' . &filetype
    return
  endif

  let [l:status, l:out, l:err] = s:run(
        \ ['lint', '--stdin', '--format=json', '--dialect=' . l:dialect,
        \  '--label=' . expand('%')], getline(1, '$'))

  if l:status > 1
    echoerr 'sift: ' . join(l:err, ' ')
    return
  endif

  let l:items = []
  for l:finding in json_decode(join(l:out, ''))
    call add(l:items, {
          \ 'filename': bufname('%'),
          \ 'lnum': l:finding.line,
          \ 'col': l:finding.column,
          \ 'type': l:finding.severity ==# 'error' ? 'E'
          \       : l:finding.severity ==# 'warning' ? 'W' : 'I',
          \ 'text': '[' . l:finding.code . ' ' . l:finding.checker . '] '
          \         . l:finding.message,
          \ 'user_data': l:finding.checker,
          \ })
  endfor

  " The list replaces itself on every run: a stale finding is worse than none.
  call setqflist([], 'r', {'title': 'sift', 'items': l:items})
  if empty(l:items)
    echo 'sift: nothing to report'
  else
    echo printf('sift: %d finding(s)', len(l:items))
    if l:open
      copen
    endif
  endif
endfunction

function! sift#format() abort
  "Format the buffer with `sift format', keeping the cursor where it was."
  let l:dialect = s:dialect()
  if empty(l:dialect)
    echoerr 'sift: no dialect for ' . &filetype
    return
  endif

  let l:view = winsaveview()
  let [l:status, l:out, l:err] = s:run(
        \ ['format', '--stdin', '--dialect=' . l:dialect], getline(1, '$'))

  if l:status != 0
    echoerr 'sift: ' . join(l:err, ' ')
    return
  endif

  call setline(1, l:out)
  call winrestview(l:view)
endfunction

function! sift#fix() abort
  "Apply every fix sift has for this buffer."
  let l:dialect = s:dialect()
  if empty(l:dialect)
    echoerr 'sift: no dialect for ' . &filetype
    return
  endif

  let l:view = winsaveview()
  let [l:status, l:out, l:err] = s:run(
        \ ['fix', '--stdin', '--dialect=' . l:dialect], getline(1, '$'))

  if l:status > 1
    echoerr 'sift: ' . join(l:err, ' ')
    return
  endif

  call setline(1, l:out)
  call winrestview(l:view)
  " The list is refreshed; the window is not opened over the buffer the author
  " is working in.
  call sift#lint(0)
endfunction

function! sift#explain() abort
  "Show what the rule that fired on this line wants, in a scratch buffer.

  " A scratch buffer rather than `:read !', which would put the rule's
  " documentation into the file being edited.
  for l:item in getqflist()
    if l:item.lnum == line('.') && !empty(l:item.user_data)
      let l:lines = systemlist(s:sift . ' explain ' . shellescape(l:item.user_data))
      " `enew!' because the buffer the author is in may have unsaved changes,
      " and an explanation is not a reason to refuse to open a window.
      enew!
      setlocal buftype=nofile bufhidden=wipe noswapfile
      execute 'silent file' fnameescape('sift explain ' . l:item.user_data)
      call setline(1, l:lines)
      setlocal nomodifiable
      return
    endif
  endfor
  echo 'sift: no finding on this line; run :SiftLint first'
endfunction
