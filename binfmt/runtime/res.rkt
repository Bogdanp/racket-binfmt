#lang racket/base

(provide
 (struct-out ok)
 (struct-out err)
 make-err
 res-bind
 err-bind)

(struct ok (v) #:transparent)
(struct err (message) #:transparent)

(define (make-err in message . args)
  (define filename (object-name in))
  (define-values (line col pos)
    (port-next-location in))
  (define formatted-message
    (apply format message args))
  (err
   (if (and line col)
       (format "~a~n  in: ~a~n  line: ~a~n  col: ~a" formatted-message filename line col)
       (format "~a~n  in: ~a~n  position: ~a" formatted-message filename pos))))

(define (res-bind res proc)
  (cond
    [(ok? res) (proc (ok-v res))]
    [else res]))

(define (err-bind res proc)
  (cond
    [(ok? res) res]
    [else (proc (err-message res))]))
