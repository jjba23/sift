;; sift --- re-running a command when the files it read change
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; There is no inotify here and no dependency to carry: the watcher asks the
;; file system a question every half second instead of subscribing to it.  That
;; is the right trade for this program -- the alternative is a C library, a
;; probe for three operating systems, and a fallback for the two that have
;; neither -- and the cost is one `stat' per file per look, which is what a
;; directory listing already costs.
;;
;; What it asks is the cheapest question that catches a save: the modification
;; time and the size.  A time alone is not enough, because a file saved twice
;; inside one tick of the clock has the same time twice; a size alone is not
;; enough, because an edit that swaps one character for another is the same
;; size.  Together they miss exactly one case -- two saves inside one tick that
;; do not change the length -- and a tick is a second on the filesystems that
;; round and a nanosecond on the ones that do not.
;;
;; The list that is polled is the list the last run *read*, not the operands it
;; was given: `sift ci .' is given a directory, and what changed may be a file
;; three levels down.  So the caller re-answers "what would this command read
;; now" on every look, which is also how a new file is noticed.

;;; Code:

(define-module (sift watch)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:export (file-stamp
            snapshot
            changed-paths
            watch-loop
            watch-interval
            describe-paths))

;; How long to wait between looks, in seconds.
;;
;; Half a second is under the notice of a person waiting for a formatter and
;; over the cost of stat'ing a tree: a save is answered before the eye moves
;; back to the terminal, and a look costs a directory listing's worth of system
;; calls.  A number rather than a setting, because a watcher whose interval has
;; to be configured is a watcher nobody configures.
(define watch-interval 1/2)

(define (file-stamp path)
  "Return @var{path}'s modification time and size, or @code{'gone}.

A file that cannot be read -- deleted between the listing and the stat, or
behind a permission the run will report on its own -- is `gone' rather than an
error: the watcher's job is to say that something changed, and a file that has
vanished has certainly changed."
  (catch #t
         (lambda ()
           (let ((info (stat path)))
             (cons (stat:mtime info) (stat:size info))))
         (lambda arguments 'gone)))

(define (snapshot paths)
  "Return what @var{paths} looked like, as an alist of @code{(path . stamp)}."
  (map (lambda (path) (cons path (file-stamp path))) paths))

(define (stamp-of snapshot path)
  "Return the stamp @var{path} had in @var{snapshot}."
  (let ((entry (assoc path snapshot)))
    (and entry (cdr entry))))

(define (changed-paths before after)
  "Return the paths whose stamp differs between @var{before} and @var{after}.

A path that appeared and a path that vanished are both changes: a run that
starts reading a file it did not read before has something new to say about it,
and one that stops reading a file has something to stop saying."
  (let ((paths (delete-duplicates
                (append (map car before) (map car after))
                string=?)))
    (filter (lambda (path)
              (let ((was (stamp-of before path))
                    (is (stamp-of after path)))
                (not (equal? was is))))
            paths)))

(define* (describe-paths paths #:optional (limit 3))
  "Return a sentence naming @var{paths}, @var{limit} of them at most."
  (let ((shown (take paths (min limit (length paths))))
        (rest (- (length paths) (min limit (length paths)))))
    (if (null? shown)
      "nothing"
      (string-append (string-join shown ", ")
                     (if (> rest 0)
                       (format #f " and ~a more" rest)
                       "")))))

(define (wait-for-change before files interval)
  "Return the first snapshot of @var{files} that differs from @var{before}.

One thread, one loop, no scheduler: the process is waiting for a person to
save a file, and there is nothing else it could usefully be doing with the
processor in the meantime."
  (let loop ()
    (usleep (inexact->exact (truncate (* interval 1000000))))
    (let ((after (snapshot (files))))
      (if (null? (changed-paths before after))
        (loop)
        after))))

(define* (watch-loop #:key run files (interval watch-interval)
                     (out (current-error-port)))
  "Watch @var{files}, calling @var{run} whenever they change.

@var{files} answers the paths the next run would read, and @var{run} is the run
itself: the loop is written this way so that what is watched is the command's
own idea of its files -- the project's `file-filter', the dialect an extension
chose, a directory walked again -- and not a second implementation of it here.

The caller has already run the command by the time this is called: its output
is on the screen, and the first thing a watch says is what it is watching.  A
finding is a thing to look at rather than a reason to stop, so nothing about a
run's status is used, and Ctrl-C is how the loop ends."
  (let loop ((before (snapshot (files))))
    (format out "sift: ~a file~a watched; waiting for a change (Ctrl-C to stop)~%"
            (length before) (if (= 1 (length before)) "" "s"))
    (force-output out)
    (let* ((after (wait-for-change before files interval))
           (changed (changed-paths before after)))
      (format out "sift: ~a changed; running again~%"
              (describe-paths changed))
      (force-output out)
      (run)
      (loop (snapshot (files))))))
