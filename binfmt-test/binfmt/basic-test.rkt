#lang racket/base

(require racket/port
         rackunit
         "basic.b")

(define a-hello
  (call-with-input-string "hello" hello))

(check-equal?
 (call-with-output-string
  (lambda (out)
    (un-hello a-hello out)))
 "hello")

(check-equal?
 (call-with-output-string
  (lambda (out)
    (un-a-string (call-with-input-string "aaa" a-string) out)))
 "aaa")
