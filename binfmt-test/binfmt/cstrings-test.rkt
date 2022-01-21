#lang racket/base

(require racket/port
         rackunit
         "common.rkt"
         "cstrings.b")

(define (bytesify node)
  (call-with-output-bytes
   (lambda (out)
     (let loop ([node node])
       (when (pair? node)
         (write-byte (ref 'u8_1 node) out)
         (loop (ref 'cstring_1 node)))))))

(check-equal?
 (cstrings (open-input-bytes #"\x00"))
 `((u8_1 . 0)
   (cstring_1)))

(let ([r (cstrings (open-input-bytes #"\x02abc\x00def\x00ghi\x00"))])
  (check-equal? (ref 'u8_1 r) 2)
  (check-equal? (map bytesify (ref 'cstring_1 r)) '(#"abc" #"def")))
