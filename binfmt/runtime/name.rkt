#lang racket/base

(provide
 (all-defined-out))

(define current-name-seqs
  (make-parameter (make-hasheq)))

(define (next-name id)
  (define seqs (current-name-seqs))
  (hash-update! seqs id add1 0)
  (string->symbol (format "~a_~a" id (hash-ref seqs id))))
