#lang racket/base

(require racket/port
         rackunit
         "foreign.b")

(define input "\x02\x03abc\x01d")
(check-equal?
 (call-with-output-string
  (lambda (out)
    (un-strings
     (strings (open-input-string input))
     out)))
 input)
