#lang racket/base

(require binfmt/runtime/res)

(provide
 (rename-out
  [parse-string string]
  [unparse-string un-string]))

(define (parse-string in)
  (define len (read-byte in))
  (ok (read-string len in)))

(define (unparse-string out s)
  (define bs (string->bytes/utf-8 s))
  (write-byte (bytes-length bs) out)
  (begin0 (ok s)
    (write-bytes bs out)))
