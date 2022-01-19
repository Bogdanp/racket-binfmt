#lang racket/base

(module reader racket/base
  (require syntax/strip-context
           "private/compiler.rkt")

  (provide
   (rename-out
    [binfmt-read read]
    [binfmt-read-syntax read-syntax]))

  (define (binfmt-read in)
    (syntax->datum
     (binfmt-read-syntax #f in)))

  (define (binfmt-read-syntax _src in)
    (strip-context (compile in))))
