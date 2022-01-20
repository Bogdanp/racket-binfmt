#lang racket/base

(require binfmt/runtime
         rackunit
         "common.rkt"
         "example.b")

(define b
  (parameterize ([current-endianness 'big])
    (call-with-input-file "example.dat" batch)))

(define h
  (ref 'header_1 b))
(check-equal?
 (ref 'magic_1 h)
 `((char_1 . #\t)
   (char_2 . #\r)
   (char_3 . #\a)
   (char_4 . #\d)))

(check-equal? (ref 'version_1 h) 1)
(check-equal? (ref 'reserved_1 h) 0)
(check-equal? (ref 'num-offsets_1 (ref 'offsets_1 h)) 97)
