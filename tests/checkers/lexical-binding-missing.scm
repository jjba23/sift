;; sift --- checker: lexical-binding-missing
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A file without `-*- lexical-binding: t; -*-` is loaded with the old
;; dynamic binding, which is almost never what its author meant.  The fix
;; inserts the header, and the file still reads afterwards.
;;
;; Every checker has a passing fixture and a failing one, and a fix test when
;; it has a fix: the fixed text is asserted, and so is the fact that the fix
;; leaves nothing behind for the next run.

(define-module (tests checkers lexical-binding-missing)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: lexical-binding-missing"

    (test "reports a file with no header"
      (assert-warnings #:source ";;; f.el --- a package\n" #:dialect 'elisp
                       #:checker 'lexical-binding-missing #:expect '((1 1))))

    (test "accepts a header on the first line"
      (assert-warnings
       #:source ";;; f.el --- a package -*- lexical-binding: t; -*-\n"
       #:dialect 'elisp #:checker 'lexical-binding-missing #:expect '()))

    (test "accepts a header on the second line"
      (assert-warnings
       #:source ";;; f.el --- a package\n;; -*- lexical-binding: t; -*-\n"
       #:dialect 'elisp #:checker 'lexical-binding-missing #:expect '()))

    (test "the fix inserts the header"
      (assert-fix #:source ";;; f.el --- a package\n" #:dialect 'elisp
                  #:checker 'lexical-binding-missing
                  #:expected ";;; -*- lexical-binding: t; -*-\n;;; f.el --- a package\n"))

    (test "the fixed file has nothing left to report"
      (assert-fixes-to-nothing #:source ";;; f.el --- a package\n"
                               #:dialect 'elisp
                               #:checker 'lexical-binding-missing))))
