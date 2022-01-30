#lang racket/base

(require racket/port
         rackunit
         "unparse-key-mismatch.b")

(check-exn
 #rx"expected: Value_1"
 (lambda ()
   (call-with-output-bytes
    (lambda (out)
      (un-Header
       `((Name_1 . ((u8_1 . 2)
                    (u8_2 . (#x01 #x02))))
         (b . ((u8_1 . 0)
               (u8_2))))
       out)))))

(check-exn
 #rx"expected: u8_2"
 (lambda ()
   (call-with-output-bytes
    (lambda (out)
      (un-Header
       `((Name_1 . ((u8_1 . 2)
                    (u8_2 . (#x01 #x02))))
         (Value_1 . ((u8_1 . 0)
                     (bytes))))
       out)))))
