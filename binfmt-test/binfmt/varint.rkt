#lang racket/base

(require binfmt/runtime/parser
         binfmt/runtime/res
         binfmt/runtime/unparser
         rackcheck
         racket/list
         racket/match
         racket/port
         rackunit)

(define-syntax-rule (define-bytes-in-version [id proc] ...)
  (begin (define (id bs) (call-with-input-bytes bs proc)) ...))

(define-syntax-rule (define-bytes-out-version [id proc] ...)
  (begin
    (define (id v)
      (call-with-output-bytes
       (lambda (out)
         (proc out v)))) ...))

(define-syntax-rule (define-roundtrippers [id parser unparser] ...)
  (begin (define id (compose1 parser unparser)) ...))

(define-bytes-in-version
  [parse-uvarint32* parse-uvarint32]
  [parse-uvarint64* parse-uvarint64]
  [parse-varint32*  parse-varint32]
  [parse-varint64*  parse-varint64])

(define-bytes-out-version
  [unparse-uvarint32* unparse-uvarint32]
  [unparse-uvarint64* unparse-uvarint64]
  [unparse-varint32*  unparse-varint32]
  [unparse-varint64*  unparse-varint64])

(define-roundtrippers
  [rt-uvarint32 parse-uvarint32* unparse-uvarint32*]
  [rt-uvarint64 parse-uvarint64* unparse-uvarint64*]
  [rt-varint32  parse-varint32*  unparse-varint32*]
  [rt-varint64  parse-varint64*  unparse-varint64*])

(define (gen:int len [signed? #f])
  (gen:let ([bs (apply gen:tuple (make-list len (gen:integer-in 0 255)))])
    (integer-bytes->integer (apply bytes bs) signed? #t)))

(define-check (check-rt-res res n)
  (match res
    [(ok v) (check-equal? v n)]
    [(err m) (fail m)]))

(check-property
 (property roundtrip-uvarint32
   ([n (gen:int 4)])
   (check-rt-res (rt-uvarint32 n) n)))

(check-property
 (property roundtrip-uvarint64
   ([n (gen:int 8)])
   (check-rt-res (rt-uvarint64 n) n)))

(check-property
 (property roundtrip-varint32
   ([n (gen:int 4 #t)])
   (check-rt-res (rt-varint32 n) n)))

(check-property
 (property roundtrip-varint64
   ([n (gen:int 8 #t)])
   (check-rt-res (rt-varint64 n) n)))
