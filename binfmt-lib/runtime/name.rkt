#lang racket/base

(#%declare #:unsafe)

(require (for-syntax racket/base
                     syntax/parse)
         racket/symbol)

(provide
 current-name-seqs
 next-name)

(define current-name-seqs
  (make-parameter (make-hasheq)))

(define (next-name id [seqs (current-name-seqs)])
  (define seq (add1 (hash-ref seqs id 0)))
  (hash-set! seqs id seq)
  (string->symbol
   (string-append
    (symbol->immutable-string id)
    (number->suffix seq))))

(define-syntax (define-number->suffix-proc stx)
  (define N 5)
  (syntax-parse stx
    [(_ id)
     #:with (fast-path ...) (for/list ([i (in-range N)])
                              (datum->syntax stx i))
     #:with (fast-path-res ...) (for/list ([i (in-range N)])
                                  (datum->syntax stx (format "_~a" i)))
     #'(define (id n)
         (case n
           [(fast-path) fast-path-res] ...
           [else (string-append "_" (number->string n))]))]))

(define-number->suffix-proc number->suffix)
