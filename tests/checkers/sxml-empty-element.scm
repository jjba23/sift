;; sift --- checker: sxml-empty-element
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; An SXML element that carries attributes and no children serializes as a
;; self-closing tag -- `<div class="card"/>' -- which XML reads as a complete
;; element and HTML does not: the parser has no self-closing syntax for a
;; non-void element, so the tag stays open and swallows what follows it.  The
;; element that is empty on purpose says so with an empty text child, `(div
;; (@ (class "card")) "")', and that is what the fix writes.
;;
;; The evidence that a form is markup is still the attribute list, so ordinary
;; code is never judged: `(f)` and `(f "a")` are not elements.

(define-module (tests checkers sxml-empty-element)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: sxml-empty-element"

    (test "reports an element with attributes and nothing else"
      (assert-warnings #:source "(div (@ (class \"my-2 px-3\")))\n"
                       #:checker 'sxml-empty-element
                       #:expect '((1 1))))

    (test "accepts the same element with an empty text child"
      (assert-warnings #:source "(div (@ (class \"my-2 px-3\")) \"\")\n"
                       #:checker 'sxml-empty-element
                       #:expect '()))

    (test "accepts an element with a child"
      (assert-warnings #:source "(div (@ (class \"x\")) (p \"a\"))\n"
                       #:checker 'sxml-empty-element
                       #:expect '()))

    (test "accepts an element with no attributes at all"
      ;; `(div)' is a call, not markup: the rule is about the attribute list,
      ;; which is the only evidence a file carries that it is writing a
      ;; document.
      (assert-warnings #:source "(div)\n" #:checker 'sxml-empty-element
                       #:expect '()))

    (test "leaves an empty attribute list to the rule that names it"
      ;; `(@)' says nothing about the element, and `sxml-attributes' already
      ;; reports it: two findings about one form would be one too many.
      (assert-warnings #:source "(div (@))\n" #:checker 'sxml-empty-element
                       #:expect '()))

    (test "leaves a repeated attribute list to the rule that names it"
      (assert-warnings #:source "(div (@ (class \"x\")) (@ (id \"y\")))\n"
                       #:checker 'sxml-empty-element
                       #:expect '()))

    (test "reports the inner element of a document"
      (assert-warnings #:source "(div (@ (id \"a\")) (span (@ (id \"b\"))))\n"
                       #:checker 'sxml-empty-element
                       #:expect '((1 19))))

    (test "reports every empty element, not only the outermost"
      (assert-warnings
       #:source "(div (@ (id \"a\"))\n     (span (@ (id \"b\")))\n     (br (@ (id \"c\"))))\n"
       #:checker 'sxml-empty-element
       #:expect '((2 6) (3 6))))

    (test "the fix writes the child that closes the element"
      (assert-fix #:source "(div (@ (class \"my-2 px-3\")))\n"
                  #:checker 'sxml-empty-element
                  #:expected "(div (@ (class \"my-2 px-3\")) \"\")\n"))

    (test "the fix keeps the layout the element was written with"
      (assert-fix #:source "(div\n  (@ (class \"card\")))\n"
                  #:checker 'sxml-empty-element
                  #:expected "(div\n  (@ (class \"card\")) \"\")\n"))

    (test "the fixed text is clean"
      (assert-fixes-to-nothing
       #:source "(div (@ (class \"card\")))\n(ul (@ (id \"x\")) (li \"a\"))\n"
       #:checker 'sxml-empty-element))

    (test "a comment inside the element does not make it a parent"
      ;; Trivia are not children: `(div (@ (class "x")) ; note' followed by
      ;; the closing delimiter is still an element with no content, and that
      ;; is exactly the element whose serialization swallows the next form.
      (assert-warnings #:source "(div (@ (class \"x\")) ; note\n     )\n"
                       #:checker 'sxml-empty-element
                       #:expect '((1 1))))))
