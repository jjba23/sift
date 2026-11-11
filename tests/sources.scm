;; sift --- source archives, on the network and on the disk
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Downloading is the one part of sift that reaches outside the machine, so the
;; tests are about the fence rather than the fetch: which URLs are source URLs,
;; which archive members are allowed out of the extraction directory, and that
;; a local archive is unpacked once and then answered from the cache.  A test
;; that fetched the network would be a test that fails on an aeroplane; a test
;; with a `file://' URL exercises curl, tar, the cache and the report without
;; leaving the machine.

(define-module (tests sources)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (veritas veritas)
  #:use-module (sift sources)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "sources: archives, the cache and the fence"

    (test "only the schemes a source may use are source URLs"
      (assert-true (source-url? "https://example.org/sift.tar.gz"))
      (assert-true (source-url? "http://example.org/sift.tar.gz"))
      (assert-true (source-url? "file:///tmp/sift.tar.gz"))
      (assert-equal #:expect #f #:got (source-url? "ftp://example.org/x"))
      (assert-equal #:expect #f #:got (source-url? "/tmp/sift.tar.gz")))

    (test "an archive member that leaves the tree is refused"
      (assert-true (safe-archive-member? "pkg/a.scm"))
      (assert-true (safe-archive-member? "./pkg/a.scm"))
      (assert-equal #:expect #f #:got (safe-archive-member? "../evil"))
      (assert-equal #:expect #f #:got (safe-archive-member? "a/../../evil"))
      (assert-equal #:expect #f #:got (safe-archive-member? "/etc/passwd"))
      (assert-equal #:expect #f #:got (safe-archive-member? "")))

    (test "a local archive is unpacked into the cache and then reused"
      (with-temp-directory
       (lambda (directory)
         (let* ((package (string-append directory "/pkg"))
                (archive (string-append directory "/pkg.tar.gz"))
                (cache (string-append directory "/cache")))
           (spit (string-append package "/a.scm") "(define (f) 1)\n")
           (assert-equal #:expect 0
             #:got (system* "tar" "-czf" archive "-C" directory "pkg"))
           (let* ((url (string-append "file://" archive))
                  (tree (resolve-source url cache)))
             (assert-true (and tree #t) #:name "the archive was unpacked")
             (assert-true (and tree (file-exists?
                                     (string-append tree "/pkg/a.scm")))
               #:name "the file it held is in the tree")
             (assert-equal #:expect tree
               #:got (resolve-source url cache)
               #:name "the second answer is the cache, not a second download"))))))))
