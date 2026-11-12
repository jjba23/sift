;; sift --- the project, indexed in a thread of its own
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A jump is a question asked of a project, and reading a project is work an
;; editor cannot wait for.  So the two are separated here: this module reads
;; the tree, parses the files and publishes an index, and nothing that answers
;; a request ever reads a directory or a file to do it.
;;
;; What makes that safe is that an index is a value.  A scan builds the next
;; one beside the one in use and publishes it with a single atomic store, so a
;; reader holds a complete index for as long as it needs it, a writer never
;; mutates what a reader can see, and neither waits for the other.  A file that
;; is saved is one entry replaced and a name table rebuilt -- not the project
;; read again -- and a keystroke never asks for either.
;;
;; One thread per workspace owns all of it: the queue of one-file refreshes and
;; the occasional full rescan go through the same thread in order, which is
;; what keeps two scans from publishing over each other.  The state a request
;; can read without the queue is the same shape as the index -- an atomic value
;; -- and it is the state an editor is told about: uninitialized, scanning the
;; tree, indexing `done' of `total' files, ready.
;;
;; The cache is what makes a second session cheap: a file whose size and
;; modification time have not changed keeps the names it defined and is not
;; read until a request needs its text.  A cache that cannot be read is no
;; cache at all, which is slower and never wrong.

(define-module (sift index)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 textual-ports)
  #:use-module (sift files)
  #:use-module (sift reader)
  #:use-module (sift definitions)
  #:use-module (sift analysis)
  #:use-module (sift json)
  #:use-module (sift stdlib)
  #:export (index-file-limit
            index-byte-limit
            index-start!
            index-rescan!
            index-refresh!
            index-forget!
            index-stop!
            files-under
            index-file))

;;; ===== The project on disk =====
;;;
;;; A project is at most a couple of thousand files and a few hundred kilobytes
;;; a file; past that an index costs more than the jumps it answers.

(define index-file-limit 2000)
(define index-byte-limit 500000)

;; How many files are read between two published snapshots.  A batch is a
;; compromise: publishing per file rebuilds the name table two thousand times,
;; and publishing once at the end leaves the first jump of a session waiting for
;; the last file.
(define publish-batch 64)

(define (directory? path)
  (eq? 'directory (false-if-exception (stat:type (stat path)))))

(define (indexable-file? filter path root)
  "Return @code{#t} when @var{path} is a file the server indexes.

Navigation reads a compressed source as the file it holds, so a `.el.gz' is
indexed when the project names no filter.  A project that names one keeps the
last word, and can name `*.el.gz' itself; the command line never takes this
path, because `sift format --write' must not write decompressed text over a
compressed file."
  (if (or (not filter) (null? filter))
    (lisp-path? (without-compression path))
    (file-included? filter path root)))

(define* (files-under root #:optional (filter #f))
  "Return the Lisp files under @var{root}, at most `index-file-limit' of them.

Directories a build leaves behind are skipped rather than walked: `.git'
alone holds more files than most source trees, and reading it would make the
first definition request slower than finding the file by hand."
  (let loop ((pending (list root)) (found '()) (count 0))
    (cond
      [(or (null? pending) (>= count index-file-limit)) (reverse found)]
      [else
        (let* ((dir (car pending))
               (entries (or (false-if-exception (scandir dir)) '()))
               (subdirectories '())
               (files '()))
          (for-each
           (lambda (name)
             (unless (or (member name skipped-directories)
                       (string-prefix? "." name))
               (let ((path (string-append dir "/" name)))
                 (cond
                   [(directory? path) (set! subdirectories (cons path subdirectories))]
                   [(indexable-file? filter path root)
                    (set! files (cons path files))]))))
           entries)
          (loop (append subdirectories (cdr pending))
                (append files found)
                (+ count (length files))))])))

(define (parse-file-text text dialect path)
  "Return the definitions of @var{text}, read as @var{dialect}."
  (parse-definitions
   (call-with-values
     (lambda () (read-source #:source text #:dialect dialect #:file path))
     (lambda (ast errors) ast))))

(define (top-level-names definitions)
  "Return the names @var{definitions} define at the top level, without repeats."
  (delete-duplicates
   (map definition-name
        (filter (lambda (definition) (not (definition-scope definition)))
                definitions))))

(define (index-file path)
  "Return @var{path} as a fully read `indexed-file', or @code{#f}."
  (let ((dialect (detect-dialect #:file path))
        (text (read-file-text path index-byte-limit)))
    (and dialect text
      (let ((definitions (parse-file-text text dialect path)))
        (make-indexed-file path text dialect definitions
                           (top-level-names definitions))))))

;;; ===== The cache =====
;;;
;;; What is written is what the next session can trust without reading: the
;;; path, the size and the modification time that decide whether the file
;;; changed, the dialect, and the names it defines.  A cache is an
;;; optimisation, so a failure to write one is not a failure of the session.

(define cache-version 1)

(define (path-hash text)
  "Return a short, deterministic name for @var{text}, for a cache file.

FNV-1a, because a cache file's name has to be the same on the next run and a
hash table's order is not a promise about anything; the path is also stored
inside the file, so a collision costs a reread and never a wrong answer."
  (let loop ((characters (string->list text)) (hash 14695981039346656037))
    (if (null? characters)
      (number->string hash 16)
      (loop (cdr characters)
            (modulo (* (logxor hash (char->integer (car characters)))
                       1099511628211)
                    18446744073709551616)))))

(define (cache-file root)
  (let ((directory (cache-home)))
    (and directory (format #f "~a/index-~a.json" directory (path-hash root)))))

(define (read-cache root)
  "Return the cached entries for @var{root}, as a hash of path to metadata.

Each entry is @code{(mtime size dialect names)}.  A cache that cannot be read
-- an older layout, a truncated file, a value that is not an object -- is no
cache at all: the workspace is read again, which is slower and never wrong."
  (let ((path (cache-file root))
        (entries (make-hash-table)))
    (when (and path (file-exists? path))
      (let ((document (false-if-exception
                       (call-with-input-file path
                                             (lambda (port) (json-read (get-string-all port)))))))
        (when (and (json-object? document)
                (equal? (json-ref document "version") cache-version)
                (equal? (json-ref document "root") root))
          (for-each
           (lambda (entry)
             (let ((file (json-ref entry "path" #f)))
               (when file
                 (hash-set! entries file
                            (list (json-ref entry "mtime" 0)
                                  (json-ref entry "size" 0)
                                  (json-ref entry "dialect" "scheme")
                                  (vector->list
                                   (json-ref entry "names" (vector))))))))
           (vector->list (json-ref document "files" (vector)))))))
    entries))

(define (make-cache-directory directory)
  "Create @var{directory} and every missing parent, like @code{mkdir -p}."
  (unless (or (file-exists? directory) (string=? directory "/")
            (string-null? directory))
    (make-cache-directory (dirname directory))
    (false-if-exception (mkdir directory))))

(define (write-cache! root entries)
  "Write the metadata of @var{entries} under @var{root}, when it can be."
  (let ((target (cache-file root)))
    (when target
      (make-cache-directory (dirname target))
      (false-if-exception
       (call-with-output-file target
                              (lambda (port)
                                (display
                                 (json-write
                                  (json-object
                                   (cons "version" cache-version)
                                   (cons "root" root)
                                   (cons "files"
                                         (list->vector
                                          (map (lambda (entry)
                                                 (let* ((file (indexed-file-path entry))
                                                        (attributes (false-if-exception (stat file))))
                                                   (json-object
                                                    (cons "path" file)
                                                    (cons "mtime" (if attributes (stat:mtime attributes) 0))
                                                    (cons "size" (if attributes (stat:size attributes) 0))
                                                    (cons "dialect"
                                                          (symbol->string (indexed-file-dialect entry)))
                                                    (cons "names"
                                                          (list->vector
                                                           (indexed-file-names entry))))))
                                               entries)))))
                                 port)))))))

;;; ===== The worker =====

(define-record-type <worker>
  (make-worker lock condition jobs thread notify stop? scan-requested?)
  worker?
  (lock worker-lock)
  (condition worker-condition)
  (jobs worker-jobs set-worker-jobs!)
  (thread worker-thread set-worker-thread!)
  ;; where progress goes: the server's port, through a procedure it supplies,
  ;; so this module has no opinion about the wire
  (notify worker-notify)
  (stop? worker-stop?)
  (scan-requested? worker-scan-requested?))

(define (ensure-worker! workspace)
  (or (workspace-worker workspace)
    (let ((worker (make-worker (make-mutex) (make-condition-variable) '() #f
                               (make-atomic-box #f)
                               (make-atomic-box #f)
                               (make-atomic-box #f))))
      (set-workspace-worker! workspace worker)
      worker)))

(define (enqueue! worker job)
  "Add @var{job} to @var{worker}'s queue and wake it."
  (lock-mutex (worker-lock worker))
  (set-worker-jobs! worker (cons job (worker-jobs worker)))
  (signal-condition-variable (worker-condition worker))
  (unlock-mutex (worker-lock worker)))

(define (take-job! worker)
  "Return the next job, waiting until there is one."
  (lock-mutex (worker-lock worker))
  (let loop ()
    (if (null? (worker-jobs worker))
      (begin (wait-condition-variable (worker-condition worker)
                                      (worker-lock worker))
        (loop))
      (let ((job (car (worker-jobs worker))))
        (set-worker-jobs! worker (cdr (worker-jobs worker)))
        (unlock-mutex (worker-lock worker))
        job))))

(define (notify! workspace status done total)
  "Say where the index is, through whatever the server installed."
  (let ((notify (atomic-box-ref (worker-notify (ensure-worker! workspace)))))
    (when notify
      (catch #t
             (lambda () (notify status done total))
             (lambda (key . arguments) #f)))))

(define (publish! workspace entries generation status done total)
  "Publish @var{entries} as the workspace's index and report the progress."
  (publish-workspace-index! workspace (index-of-files entries generation)
                            status done total)
  (notify! workspace status done total))

(define (cached-entry path remembered)
  "Return @var{path} as the cache remembers it, or @code{#f} when it cannot."
  (let ((dialect (detect-dialect #:file path)))
    (and dialect remembered
      (let ((attributes (false-if-exception (stat path))))
        (and attributes
          (= (car remembered) (stat:mtime attributes))
          (= (cadr remembered) (stat:size attributes))
          (string=? (caddr remembered) (symbol->string dialect))
          (make-indexed-file path #f dialect #f (cadddr remembered)))))))

(define (read-entry path cached)
  "Return the entry for @var{path}: remembered names, or a read of the file."
  (let ((remembered (hash-ref cached path)))
    (or (cached-entry path remembered)
      (let ((dialect (detect-dialect #:file path)))
        (and dialect (index-file path))))))

(define (scan! workspace)
  "Read the tree under the workspace's root and publish it batch by batch."
  (atomic-box-set! (worker-scan-requested? (ensure-worker! workspace)) #f)
  (let ((root (workspace-root workspace)))
    (if (not root)
      (publish! workspace '() 1 'ready 0 0)
      (begin
        (notify! workspace 'scanning 0 0)
        (let* ((paths (files-under root (workspace-filter workspace)))
               (total (length paths))
               (cached (read-cache root)))
          (notify! workspace 'indexing 0 total)
          (let loop ((rest paths) (entries '()) (done 0))
            (cond
              [(atomic-box-ref (worker-stop? (ensure-worker! workspace))) #f]
              [(null? rest)
               (write-cache! root entries)
               (publish! workspace entries 1 'ready done total)]
              [else
                (let ((entry (read-entry (car rest) cached)))
                  (when entry (set! entries (cons entry entries)))
                  (let ((done (+ done 1)))
                    (when (or (zero? (modulo done publish-batch))
                            (null? (cdr rest)))
                      (publish! workspace entries 1 'indexing done total))
                    (loop (cdr rest) entries done)))])))))))

(define (refresh! workspace path)
  "Read one file again and publish the index it leaves behind.

The whole project is not read: the snapshot's entries are copied, the one path
is replaced or forgotten, and the name table is built from the result.  A file
the filter no longer wants -- deleted, or excluded by a settings change -- is
forgotten rather than kept as a stale answer."
  (let* ((old (workspace-index workspace))
         (root (workspace-root workspace))
         (wanted? (and root (file-exists? path)
                    (indexable-file? (workspace-filter workspace) path root)))
         (entry (and wanted? (index-file path)))
         (others (filter (lambda (candidate)
                           (not (string=? path (indexed-file-path candidate))))
                         (hash-map->list (lambda (path entry) entry)
                                         (index-files old))))
         (entries (if entry (cons entry others) others))
         (status (car (workspace-progress workspace)))
         (done (cadr (workspace-progress workspace)))
         (total (caddr (workspace-progress workspace))))
    (publish! workspace entries (+ 1 (index-generation old))
              (if (eq? status 'uninitialized) 'ready status)
              done total)))

(define (indexer-loop workspace)
  (let loop ()
    (let ((job (take-job! (ensure-worker! workspace))))
      (case [car job]
        [(stop) #f]
        [(scan) (catch #t (lambda () (scan! workspace))
                       (lambda (key . arguments) #f))
         (loop)]
        [(refresh) (catch #t (lambda () (refresh! workspace (cadr job)))
                          (lambda (key . arguments) #f))
         (loop)]
        [else (loop)]))))

;;; ===== What the server calls =====

(define* (index-start! workspace #:key (notify #f))
  "Start @var{workspace}'s indexer and ask it to read the project.

The thread is started once and then lives as long as the session; every later
request for an index is one job on its queue.  @var{notify} is called with
@code{(status done total)} whenever the state changes."
  (let ((worker (ensure-worker! workspace)))
    (when notify
      (atomic-box-set! (worker-notify worker) notify))
    (unless (worker-thread worker)
      (set-worker-thread! worker
                          (call-with-new-thread
                           (lambda () (indexer-loop workspace)))))
    (index-rescan! workspace)
    *unspecified*))

(define (index-rescan! workspace)
  "Ask for the project to be read again, once per request."
  (let ((worker (ensure-worker! workspace)))
    (unless (atomic-box-ref (worker-scan-requested? worker))
      (atomic-box-set! (worker-scan-requested? worker) #t)
      (enqueue! worker '(scan)))))

(define (index-refresh! workspace path)
  "Ask for @var{path} to be read again, in the indexer's own thread."
  (enqueue! (ensure-worker! workspace) (list 'refresh path)))

(define (index-forget! workspace)
  "Drop the index and read the project again, for a changed configuration."
  (publish-workspace-index! workspace empty-index 'scanning 0 0)
  (notify! workspace 'scanning 0 0)
  (index-rescan! workspace))

(define (index-stop! workspace)
  "Stop the indexer and wait for it, so an exit does not lose a write."
  (let ((worker (workspace-worker workspace)))
    (when (and worker (worker-thread worker))
      (atomic-box-set! (worker-stop? worker) #t)
      (enqueue! worker '(stop))
      (catch #t
             (lambda () (join-thread (worker-thread worker)))
             (lambda (key . arguments) #f))
      (set-worker-thread! worker #f)))
  *unspecified*)
