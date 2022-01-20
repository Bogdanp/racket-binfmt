#lang racket/base

(provide ref)

(define ref (compose1 cdr assq))
