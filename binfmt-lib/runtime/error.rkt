#lang racket/base

(provide
 exn:fail:binfmt?
 exn:fail:binfmt-id)

(struct exn:fail:binfmt exn:fail (id))

(define (oops what fmt . args)
  (raise (exn:fail:binfmt
          (apply format fmt args)
          (current-continuation-marks)
          what)))

(module+ private
  (provide
   exn:fail:binfmt?
   exn:fail:binfmt-id
   oops))
