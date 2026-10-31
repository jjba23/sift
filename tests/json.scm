;; sift --- JSON tests
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; The language server speaks JSON, so the reader and the writer are the part
;; of it that has to be exactly right: a document that cannot be read back is
;; a document an editor will not accept.  What is asserted here is the round
;; trip, the shapes a Lisp does not have (an object, an array, `null'), and
;; the errors -- a reader that guesses instead of refusing is worse than one
;; that stops.

(define-module (tests json)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (veritas veritas)
  #:use-module (sift json)
  #:use-module (tests helpers)
  #:export (spec))

(define* (assert-rewrites #:key (text "{}") expect
           (name "the document is written back the same"))
  "Assert that @var{text} reads and writes back as @var{expect}."
  (assert-same-text #:expect expect
    #:got (json-write (json-read text))
    #:name name))

(define* (assert-json-error #:key text (line 1) (column 1)
           (name "the document is refused"))
  "Assert that @var{text} is refused, at @var{line} and @var{column}."
  (let ((error (catch 'sift-json-error
                 (lambda () (json-read text) #f)
                 (lambda (key value) value))))
    (assert-true (and error (pair? error))
      #:name (format #f "~a: it is refused" name))
    (when (pair? error)
      (assert-equal #:expect line #:got (list-ref error 1)
        #:name (format #f "~a: on line ~a" name line))
      (assert-equal #:expect column #:got (list-ref error 2)
        #:name (format #f "~a: at column ~a" name column)))))

(define (spec)
  (suite "JSON"

    ;; --- the shapes ---------------------------------------------------

    (test "an object keeps the order it was written in"
      (assert-rewrites
         #:text "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\"}"
         #:expect "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\"}"))

    (test "an empty object is not an empty array"
      (assert-rewrites #:text "{}" #:expect "{}")
      (assert-rewrites #:text "[]" #:expect "[]"))

    (test "an array of mixed values round trips"
      (assert-rewrites
         #:text "[1,-2500,true,false,null,\"a\"]"
         #:expect "[1,-2500,true,false,null,\"a\"]"))

    (test "a nested document round trips"
      (assert-rewrites
         #:text "{\"params\":{\"capabilities\":{\"textDocument\":{\"formatting\":{}}}}}"
         #:expect "{\"params\":{\"capabilities\":{\"textDocument\":{\"formatting\":{}}}}}"))

    (test "whitespace between tokens is not part of the value"
      (assert-rewrites
         #:text "  {\n  \"a\" : [ 1 , 2 ]\n}\n"
         #:expect "{\"a\":[1,2]}"))

    ;; --- strings ------------------------------------------------------

    (test "the escapes JSON requires are read back as characters"
      (assert-same-text #:expect "a\"b\\c\nd\te"
        #:got (json-read "\"a\\\"b\\\\c\\nd\\te\"")))

    (test "an escaped solidus is a solidus"
      (assert-same-text #:expect "a/b" #:got (json-read "\"a\\/b\"")))

    (test "a \\u escape becomes the character it names"
      (assert-same-text #:expect "é" #:got (json-read "\"\\u00e9\"")))

    (test "a surrogate pair becomes one character"
      (assert-same-text #:expect "😀" #:got (json-read "\"\\ud83d\\ude00\"")))

    (test "a lone surrogate does not raise"
      (assert-same-text #:expect (string (integer->char #xFFFD) #\x)
        #:got (json-read "\"\\ud83dx\"")))

    (test "a string that needs escaping is written escaped"
      (assert-same-text #:expect "\"a\\\"b\\\\c\\nd\""
        #:got (json-write "a\"b\\c\nd")))

    (test "a control character is written as an escape"
      (assert-same-text #:expect "\"\\u0001\"" #:got (json-write (string #\x01))))

    ;; --- numbers ------------------------------------------------------

    (test "numbers are written the way they are read"
      (for-each
         (lambda (text)
           (assert-same-text #:expect text #:got (json-write (json-read text))
             #:name (format #f "~a round trips" text)))
         '("0" "1" "-1" "2500" "-2500" "1.5" "0.25")))

    (test "a number that is not finite is written as null"
      (assert-same-text #:expect "null" #:got (json-write (/ 1.0 0.0))))

    ;; --- the accessors ------------------------------------------------

    (test "a member is found by its name, or a default is answered"
      (let ((object (json-read "{\"a\":1,\"b\":{\"c\":[1,2]}}")))
        (assert-equal #:expect 1 #:got (json-ref object "a"))
        (assert-equal #:expect 'missing #:got (json-ref object "zzz" 'missing))
        (assert-same-text #:expect "[1,2]"
          #:got (json-write (json-ref-path object "b" "c")))))

    (test "a symbol names a member as well as a string does"
      (assert-equal #:expect 1 #:got (json-ref (json-read "{\"a\":1}") 'a)))

    (test "an object is not an array, and neither is a list"
      (assert-true (json-object? (json-read "{}")))
      (assert-true (not (json-object? (json-read "[]"))))
      (assert-true (json-array? (json-read "[]")))
      (assert-true (json-null? (json-read "null"))))

    ;; --- refusals -----------------------------------------------------

    (test "a trailing comma is refused"
      (assert-json-error #:text "{\"a\":1,}" #:line 1 #:column 8
        #:name "an object with a trailing comma"))

    (test "a missing comma is refused"
      (assert-json-error #:text "[1 2]" #:line 1 #:column 4))

    (test "an unterminated string is refused"
      (assert-json-error #:text "{\"a:\"b\"}" #:line 1 #:column 6
        #:name "a malformed member name"))

    (test "a bare word is refused"
      (assert-json-error #:text "nope" #:line 1 #:column 2))

    (test "trailing text after the value is refused"
      (assert-json-error #:text "{} {}" #:line 1 #:column 4))

    (test "the position of an error counts lines from one"
      (assert-json-error #:text "{\n  \"a\": 1,\n}" #:line 3 #:column 1))))
