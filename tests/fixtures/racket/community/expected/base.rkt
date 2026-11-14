#lang racket

;; A small guestbook: what a module of this dialect looks like when the reader,
;; the formatter and the rules all agree about it.

(require racket/list
         racket/string)

(provide add-entry
         entries->lines)

(struct entry (name note) #:transparent)

(define (add-entry book name note)
  (define cleaned (string-trim note))
  (cons (entry name cleaned) book))

(define (entries->lines book)
  (for/list ([each (in-list (reverse book))])
    (format "~a: ~a" (entry-name each) (entry-note each))))

(define (summarise book)
  (cond
    [(empty? book) "an empty guestbook"]
    [else (format "~a entries" (length book))]))

(define (describe value)
  (match value
    [(list first rest) (list first rest)]
    [(? string? text) (string-length text)]
    [_ value]))

(define (report book)
  (unless (empty? book)
    (for ([line (in-list (entries->lines book))])
      (displayln line))))

(module+ main
  (report (list (entry "ada" "hello"))))
