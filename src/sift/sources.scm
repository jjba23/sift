;; sift --- source archives, on the network and on the disk
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A standard library that is installed is read where it lives; a source tree
;; that is not installed can be fetched, and that is what this module does.  A
;; URL is downloaded with `curl' into the cache, checked before it is unpacked,
;; and unpacked with `tar' into a directory of its own.  The directory is then
;; just another root for `(sift stdlib)' to index, which is the whole of the
;; integration: downloading and indexing are two jobs, and the second one
;; already knows how to read a directory.
;;
;; Three rules make a download safe enough to be worth having.  Only `http',
;; `https' and `file' URLs are accepted; the archive is listed before it is
;; extracted, and an entry that is absolute, climbs out with `..' or is a
;; symbolic link is refused; and the download is bounded by a timeout, a
;; connection timeout and a size.  A bad archive is a failed source, never a
;; tree written over the cache or over the home directory.
;;
;; `curl' and `tar' are the platform's tools rather than libraries sift links
;; against: both ship with Linux and macOS, and a machine without one reports
;; that the source was not fetched rather than claiming it was.  A source that
;; is already in the cache is not downloaded again; deleting the cache
;; directory is always safe.

(define-module (sift sources)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:export (source-url?
            source-archive?
            safe-archive-member?
            source-cache-key
            source-directory
            program-exists?
            resolve-source
            download-source
            unpack-source
            cached-sources))

(define archive-suffixes
  '(".tar.gz" ".tgz" ".tar" ".tar.xz" ".txz" ".tar.bz2" ".tbz2"))

(define (source-url? text)
  "Return @code{#t} when @var{text} is a source URL this module will fetch."
  (and (string? text)
    (or (string-prefix? "https://" text)
      (string-prefix? "http://" text)
      (string-prefix? "file://" text))))

(define (source-archive? path)
  "Return @code{#t} when @var{path} ends in an archive suffix tar can read."
  (and (string? path)
    (any (lambda (suffix) (string-suffix? suffix path)) archive-suffixes)))

;;; ===== The cache =====

(define (url-hash text)
  "Return a short, deterministic name for @var{text}, for a cache directory.

FNV-1a, because a cache name has to be the same on the next run and a name
chosen by a hash table is not a promise about anything."
  (let loop ((characters (string->list text)) (hash 14695981039346656037))
    (if (null? characters)
      (number->string hash 16)
      (loop (cdr characters)
            (modulo (* (logxor hash (char->integer (car characters)))
                       1099511628211)
                    18446744073709551616)))))

(define (source-cache-key url)
  (url-hash url))

(define (source-directory cache-directory url)
  "Return the directory @var{url} is cached in under @var{cache-directory}."
  (string-append cache-directory "/" (source-cache-key url)))

(define (make-directories path)
  "Create @var{path} and every missing parent, like @code{mkdir -p}."
  (unless (or (file-exists? path) (string=? path "/") (string-null? path))
    (make-directories (dirname path))
    (false-if-exception (mkdir path))))

(define (directory-nonempty? path)
  (and (file-exists? path)
    (pair? (filter (lambda (name) (not (member name '("." ".."))))
                   (or (false-if-exception (scandir path)) '())))))

(define (program-exists? program)
  (let ((path (getenv "PATH")))
    (and path
      (any (lambda (directory)
             (file-exists? (string-append directory "/" program)))
           (string-split path #\:)))))

;;; ===== Running the platform's tools =====

(define output-limit 200000)

(define (run-program program arguments)
  "Run @var{program} with @var{arguments}, returning @code{(status . output)}.

The shell is the portable way to put a child's standard error where its
standard output goes: the script is constant, and the program and its
arguments travel in @code{$@} as words of their own."
  (catch #t
         (lambda ()
           (let ((port (apply open-pipe* OPEN_READ
                              "sh" "-c" "exec \"$@\" 2>&1" "sh"
                              program arguments)))
             (let* ((text (get-string-n port output-limit))
                    (copy (and (string? text) (string-copy text)))
                    (status (close-pipe port)))
               (cons status (or copy "")))))
         (lambda (key . rest) (cons #f ""))))

(define (successful? result)
  (let ((status (car result)))
    (and status (status:exit-val status) (zero? (status:exit-val status)))))

;;; ===== Downloading =====

(define download-timeout "300")
(define connect-timeout "30")
(define download-limit-bytes 104857600)
(define download-limit (number->string download-limit-bytes))

(define* (download-source url target #:key (report #f))
  "Download @var{url} to @var{target}, returning @code{#t} when it arrived.

A partial file is written beside the target and renamed into place, so a
download that ends in the middle leaves the archive it had rather than half
of the one it was making.  The limits are the ones a background fetch needs:
a timeout, a connection timeout and a maximum size."
  (cond
    [(not (program-exists? "curl"))
     (when report (report "sources: curl is not installed; cannot download"))
     #f]
    [else
      (let ((partial (string-append target ".partial")))
        (when report (report (format #f "sources: downloading ~a" url)))
        (let ((result (run-program
                       "curl"
                       (list "--fail" "--location" "--silent" "--show-error"
                             "--max-time" download-timeout
                             "--connect-timeout" connect-timeout
                             "--max-filesize" download-limit
                             "--output" partial url))))
          (if (successful? result)
            (let ((attributes (false-if-exception (stat partial))))
              (if (and attributes (> (stat:size attributes) download-limit-bytes))
                (begin
                  (false-if-exception (delete-file partial))
                  (when report
                    (report (format #f "sources: download too large: ~a" url)))
                  #f)
                (begin
                  (false-if-exception (rename-file partial target))
                  (when report
                    (report (format #f "sources: downloaded ~a (~a bytes)"
                                    url (if attributes (stat:size attributes) 0))))
                  #t)))
            (begin
              (false-if-exception (delete-file partial))
              (when report
                (report (format #f "sources: download failed: ~a"
                                (string-trim-both (cdr result)))))
              #f))))]))

;;; ===== Checking an archive before it is unpacked =====

(define (safe-archive-member? name)
  "Return @code{#t} when @var{name} stays inside the directory it is in.

An absolute path, a component that is `..', or an empty name is refused.  The
check is on the member's spelling, which is what tar writes and what tar will
follow; a name that passes is a name under the extraction directory."
  (and (string? name)
    (not (string-null? name))
    (not (char=? (string-ref name 0) #\/))
    (not (any (lambda (part) (string=? part ".."))
              (string-split name #\/)))))

(define (close-status-ok? status)
  (and status (status:exit-val status) (zero? (status:exit-val status))))

(define (archive-names-safe? archive)
  "Return @code{#t} when every member of @var{archive} stays inside the tree."
  (catch #t
         (lambda ()
           (let ((port (open-pipe* OPEN_READ "tar" "-tf" archive)))
             (let loop ((safe #t))
               (let ((line (read-line port)))
                 (cond
                   [(eof-object? line)
                    (let ((status (close-pipe port)))
                      (and safe (close-status-ok? status)))]
                   [(not (safe-archive-member? line))
                    (close-pipe port)
                    #f]
                   [else (loop safe)])))))
         (lambda (key . rest) #f)))

(define (archive-types-safe? archive)
  "Return @code{#t} when @var{archive} holds only regular files and folders."
  (catch #t
         (lambda ()
           (let ((port (open-pipe* OPEN_READ "tar" "-tvf" archive)))
             (let loop ((safe #t))
               (let ((line (read-line port)))
                 (cond
                   [(eof-object? line)
                    (let ((status (close-pipe port)))
                      (and safe (close-status-ok? status)))]
                   [(or (string-null? line)
                      (memv (string-ref line 0) '(#\- #\d)))
                    (loop safe)]
                   [else
                     (close-pipe port)
                     #f])))))
         (lambda (key . rest) #f)))

(define (safe-archive? archive)
  "Return @code{#t} when @var{archive} holds only regular files and folders.

The listing is read a line at a time and stopped at the first entry that is
not safe: an archive with a million members costs the members before the bad
one, not a string of them held in memory.  The type comes from `tar -tvf'; a
symbolic or hard link is a way out of the tree even when its name is inside."
  (and (archive-names-safe? archive)
    (archive-types-safe? archive)))

;;; ===== Unpacking =====

(define* (unpack-source archive tree #:key (report #f))
  "Unpack @var{archive} into @var{tree}, returning @code{#t} on success."
  (cond
    [(not (program-exists? "tar"))
     (when report (report "sources: tar is not installed; cannot unpack"))
     #f]
    [(not (safe-archive? archive))
     (when report (report (format #f "sources: refusing an unsafe archive: ~a"
                                  archive)))
     #f]
    [else
      (make-directories tree)
      (when report (report (format #f "sources: unpacking ~a" archive)))
      (let ((result (run-program
                     "tar"
                     (list "-xf" archive "-C" tree "--no-same-owner"))))
        (if (successful? result)
          #t
          (begin
            (when report
              (report (format #f "sources: unpack failed: ~a"
                              (string-trim-both (cdr result)))))
            #f)))]))

;;; ===== One URL, from cache or from the network =====

(define* (resolve-source url cache-directory #:key (report #f) (refresh #f))
  "Return the directory @var{url} was unpacked into, or @code{#f}.

A directory that is already there is the answer without a network request;
@var{refresh} asks for the archive again, and is what a reader who wants a
newer source tree uses.  A failed download or a refused archive leaves no
directory, so a later attempt is a later attempt."
  (if (not (source-url? url))
    (begin
      (when report (report (format #f "sources: not a source URL: ~a" url)))
      #f)
    (let* ((directory (source-directory cache-directory url))
           (tree (string-append directory "/tree"))
           (archive (string-append directory "/archive")))
      (cond
        [(and (not refresh) (directory-nonempty? tree))
         (when report (report (format #f "sources: using cached ~a" url)))
         tree]
        [(not (source-archive? url))
         (when report (report (format #f "sources: not an archive URL: ~a" url)))
         #f]
        [else
          (make-directories directory)
          (and (download-source url archive #:report report)
            (unpack-source archive tree #:report report)
            (begin
              (when report (report (format #f "sources: ready ~a" url)))
              tree))]))))

;;; ===== What the cache holds =====

(define (cached-sources cache-directory)
  "Return the source directories in @var{cache-directory}, newest name first."
  (if (not (and cache-directory (file-exists? cache-directory)))
    '()
    (filter-map
     (lambda (name)
       (let ((tree (string-append cache-directory "/" name "/tree")))
         (and (directory-nonempty? tree) tree)))
     (sort (or (false-if-exception (scandir cache-directory)) '()) string<?))))
