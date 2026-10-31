;; sift --- reader tests, Common Lisp
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Common Lisp's reader is the one with feature conditionals, read-time
;; evaluation, package markers and unreadable objects.  sift reads all of
;; them without evaluating anything and without dropping anything: a `#+'
;; guard is kept as a node with both halves, `#.' is copied verbatim, and
;; `foo:bar', `foo::bar', `:key' and `#:uninterned' stay four different
;; things.

(define-module (tests reader-cl)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift records)
  #:use-module (sift reader)
  #:use-module (tests helpers)
  #:export (spec))

(define (read-cl source)
  (read-sexps source #:dialect 'cl))

(define (errors-cl source)
  (read-errors source #:dialect 'cl))

(define (round-trip source)
  (call-with-values (lambda () (read-source-string source #:dialect 'cl))
    (lambda (ast errors) (ast->text ast))))

(define (atom-nodes source)
  (call-with-values (lambda () (read-source-string source #:dialect 'cl))
    (lambda (ast errors) (filter node-atom? ast))))

(define (spec)
  (suite "reader: Common Lisp"

    (test "package markers are kept apart"
      (let ((markers (map (lambda (node)
                            (cons (node-value node)
                              (node-package-marker node)))
                       (atom-nodes "foo:bar foo::baz :key #:uninterned"))))
        (assert-equal #:expect '(("foo:bar" . "foo:")
                                 ("foo::baz" . "foo::")
                                 (":key" . #f)
                                 ("#:uninterned" . "#:"))
          #:got markers)))

    (test "a keyword is an atom of its own kind"
      (assert-equal #:expect '(keyword)
        #:got (map (lambda (node) (node-atom-type node))
                (atom-nodes ":key"))))

    (test "a feature conditional keeps both halves"
      (assert-equal #:expect '((feature sbcl) (form (defun f ())))
        #:got
        (call-with-values
          (lambda () (read-source-string "#+sbcl (defun f ())"
                       #:dialect 'cl))
          (lambda (ast errors)
            (let ((node (car ast)))
              (list (list 'feature (node->sexp (node-feature node)))
                (list 'form (node->sexp (node-guarded-form node)))))))))

    (test "a feature expression may be a list"
      (assert-same-text #:expect "#+(or sbcl ccl) (defun f ())"
        #:got (round-trip "#+(or sbcl ccl) (defun f ())")))

    (test "a negated feature conditional is kept"
      (assert-same-text #:expect "#-(not sbcl) (defun f ())"
        #:got (round-trip "#-(not sbcl) (defun f ())")))

    (test "read-time evaluation is copied and not evaluated"
      (assert-same-text #:expect "#.(+ 1 2)"
        #:got (round-trip "#.(+ 1 2)"))
      ;; it is a form of the program, so it is data; it is opaque, so the
      ;; reader hands it over as text rather than as a tree
      (assert-equal #:expect '("#.(+ 1 2)")
        #:got (read-cl "#.(+ 1 2)")))

    (test "an unreadable object is copied verbatim"
      (assert-same-text #:expect "#<buffer foo>"
        #:got (round-trip "#<buffer foo>"))
      (assert-same-text #:expect "(describe #<window 1 on foo>)"
        #:got (round-trip "(describe #<window 1 on foo>)")))

    (test "a pathname is a prefix over a string"
      (assert-same-text #:expect "(probe-file #p\"/tmp/x\")"
        #:got (round-trip "(probe-file #p\"/tmp/x\")")))

    (test "a structure literal is a prefix over a list"
      (assert-same-text #:expect "#s(point :x 1 :y 2)"
        #:got (round-trip "#s(point :x 1 :y 2)")))

    (test "block comments nest"
      (assert-same-text #:expect "#| a #| b |# c |# x"
        #:got (round-trip "#| a #| b |# c |# x")))

    (test "a datum comment holds one datum"
      (assert-same-text #:expect "#;foo bar"
        #:got (round-trip "#;foo bar")))

    (test "radix and exactness prefixes are kept"
      (assert-same-text #:expect "(list #x1F #o17 #b1010 #e1.0)"
        #:got (round-trip "(list #x1F #o17 #b1010 #e1.0)")))

    (test "a case-sensitive symbol keeps its case"
      (assert-same-text #:expect "(defun make-Thing () (list :Thing))"
        #:got (round-trip "(defun make-Thing () (list :Thing))")))

    (test "a LOOP form is read as ordinary forms"
      (assert-same-text #:expect "(loop :for x :in xs :do (print x))"
        #:got (round-trip "(loop :for x :in xs :do (print x))")))

    (test "no parse error is reported for Common Lisp syntax"
      (assert-equal #:expect '()
        #:got (errors-cl "#+sbcl (defun f ()) #.(foo) #<thing>")))

    (test "a Common Lisp file round-trips byte for byte"
      (let ((source "(defpackage :demo (:use :cl) (:export #:run))\n\n(in-package :demo)\n\n(defun run ()\n  \"Run it.\"\n  (format t \"~a~%\" 1))\n"))
        (assert-same-text #:expect source #:got (round-trip source))))))
