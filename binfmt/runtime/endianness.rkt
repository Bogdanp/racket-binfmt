#lang racket/base

(require racket/contract)

(provide
 current-endianness
 big-endian?)

(define/contract current-endianness
  (parameter/c (or/c 'big 'little))
  (make-parameter (if (system-big-endian?) 'big 'little)))

(define (big-endian?)
  (eq? (current-endianness) 'big))
