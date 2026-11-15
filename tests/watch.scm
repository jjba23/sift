;; sift --- the file watcher
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; The watcher asks the file system a question instead of subscribing to it, so
;; what it is made of is a stamp, a snapshot, and a comparison -- three
;; procedures that answer without a clock, a thread or a process.  They are
;; tested here, and the loop that uses them is tested by mutating what it polls:
;; a watcher that is never woken is a watcher that is not there.
;;
;; What `--watch' does to a command is `tests/cli.scm`'s business, because it is
;; a promise about a running process rather than about a comparison.

(define-module (tests watch)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (veritas veritas)
  #:use-module (sift watch)
  #:use-module (tests helpers)
  #:export (spec))

(define (snapshot-of . paths)
  (snapshot paths))

(define (spec)
  (suite "watch"

    ;; --- what a file looks like --------------------------------------

    (test "a file that is not there is `gone'"
      (assert-equal #:expect 'gone
        #:got (file-stamp (string-append (keep-temp-directory) "/nothing-here.scm"))))

    (test "a file that is rewritten has a different stamp"
      (with-temp-directory
       (lambda (directory)
         (let* ((path (string-append directory "/a.scm"))
                (before (file-stamp path)))
           (spit path "(define (f) 1)\n")
           (let ((after (file-stamp path)))
             (assert-true (not (equal? before after))
               #:name "writing a file changes its stamp")
             (spit path "(define (f) 2 2)\n")
             (assert-true (not (equal? after (file-stamp path)))
               #:name "and writing it again changes it again"))))))

    ;; --- what changed -------------------------------------------------

    (test "nothing changed is nothing changed"
      (let* ((paths (list "a.scm" "b.scm"))
             (before (snapshot paths)))
        (assert-equal #:expect '() #:got (changed-paths before before))))

    (test "a file whose stamp differs is a change"
      (let* ((before (list (cons "a.scm" (cons 1 10))
                           (cons "b.scm" (cons 1 10))))
             (after (list (cons "a.scm" (cons 1 10))
                          (cons "b.scm" (cons 2 10)))))
        (assert-equal #:expect '("b.scm") #:got (changed-paths before after))))

    (test "a file that appeared and a file that vanished are both changes"
      (let* ((before (list (cons "a.scm" (cons 1 10))))
             (after (list (cons "b.scm" (cons 1 10)))))
        (assert-equal #:expect '("a.scm" "b.scm")
          #:got (sort (changed-paths before after) string<?))))

    (test "a file that appeared while the rest was quiet is a change"
      ;; the case a directory-shaped watch is for: a new file in a project
      (let* ((before (snapshot (list "a.scm")))
             (after (snapshot (list "a.scm" "b.scm"))))
        (assert-true (if (member "b.scm" (changed-paths before after)) #t #f)
          #:name "the file that was not watched before is noticed")))

    ;; --- saying it ----------------------------------------------------

    (test "a change is named, and a long list is counted"
      (assert-equal #:expect "a.scm" #:got (describe-paths (list "a.scm"))
        #:name "one path is its name")
      (assert-equal #:expect "a.scm, b.scm, c.scm and 2 more"
        #:got (describe-paths (list "a.scm" "b.scm" "c.scm" "d.scm" "e.scm"))
        #:name "more than three is three and a count")
      (assert-equal #:expect "nothing" #:got (describe-paths '())
        #:name "an empty list says so rather than printing a blank"))

    ;; --- the loop -----------------------------------------------------

    (test "the loop runs again when what it watches changes"
      ;; the file list is the loop's input, so a list that grows between two
      ;; looks is exactly "a file appeared", which is what a watch of a
      ;; directory has to notice -- and it grows on every look here, so the
      ;; loop is woken every time until the test stops it
      (let* ((calls 0)
             (runs 0)
             (out (open-output-string))
             (result
              (catch 'enough
                     (lambda ()
                       (watch-loop #:interval 1/100
                                   #:out out
                                   #:files (lambda ()
                                             (set! calls (+ calls 1))
                                             (map (lambda (n) (format #f "f~a.scm" n))
                                                  (iota calls)))
                                   #:run (lambda ()
                                           (set! runs (+ runs 1))
                                           (when (>= runs 2) (throw 'enough runs)))))
                     (lambda (key value) value)))
             (said (get-output-string out)))
        (assert-equal #:expect 2 #:got result
          #:name "the loop ran until the test stopped it, not until it gave up")
        (assert-equal #:expect 2 #:got runs
          #:name "a run per change, and no run without one")
        (assert-contains #:expect said #:got "waiting for a change")
        (assert-contains #:expect said #:got "changed; running again")))))
