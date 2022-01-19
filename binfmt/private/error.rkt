#lang racket/base

(provide oops!)

(define (oops! who message in [line #f] [col #f] [pos #f])
  (define filename (object-name in))
  (unless (or line col pos)
    (set!-values (line col pos) (port-next-location in)))
  (raise-syntax-error who (if (and line col)
                              (format "~a~n  filename: ~a~n  line: ~a~n  column: ~a" message filename line col)
                              (format "~a~n  filename: ~a~n  position: ~a" message filename pos))))
