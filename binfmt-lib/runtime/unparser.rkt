#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse/pre)
         racket/match
         racket/port
         (submod "error.rkt" private)
         "name.rkt"
         "res.rkt")

(provide
 make-unparser-table
 make-unparser
 unparse)

(define (unparse table unparser out v)
  (match (unparse-expr out v table unparser)
    [(ok _) (void)]
    [(err message)
     (oops unparser "unparse failed~n ~a" message)]))

(define (make-unparser-table)
  (make-hasheq
   `((eof       . ,unparse-eof)
     (nul       . ,unparse-nul)
     (f32       . ,unparse-f32)
     (f64       . ,unparse-f64)
     (f32le     . ,unparse-f32le)
     (f64le     . ,unparse-f64le)
     (f32be     . ,unparse-f32be)
     (f64be     . ,unparse-f64be)
     (u8        . ,unparse-u8)
     (i8        . ,unparse-i8)
     (u16       . ,unparse-u16)
     (u32       . ,unparse-u32)
     (u64       . ,unparse-u64)
     (u16le     . ,unparse-u16le)
     (u32le     . ,unparse-u32le)
     (u64le     . ,unparse-u64le)
     (u16be     . ,unparse-u16be)
     (u32be     . ,unparse-u32be)
     (u64be     . ,unparse-u64be)
     (i16       . ,unparse-i16)
     (i32       . ,unparse-i32)
     (i64       . ,unparse-i64)
     (i16le     . ,unparse-i16le)
     (i32le     . ,unparse-i32le)
     (i64le     . ,unparse-i64le)
     (i16be     . ,unparse-i16be)
     (i32be     . ,unparse-i32be)
     (i64be     . ,unparse-i64be)
     (uvarint32 . ,unparse-uvarint32)
     (uvarint64 . ,unparse-uvarint64)
     (varint32  . ,unparse-varint32)
     (varint64  . ,unparse-varint64))))

(define ((make-unparser table . alts) out v)
  (unparse-alts out v table alts))

(define (unparse-alts out v table alts)
  (let loop ([alts alts]
             [errs null])
    (cond
      [(null? alts)
       (let ([errs (reverse errs)])
         (err (call-with-output-string
               (lambda (sout)
                 (fprintf sout "~a" (car errs))
                 (for ([err (in-list (cdr errs))])
                   (fprintf sout "~nor ~a" err))))))]
      [else
       (err-bind
        (unparse-alt out v table (car alts))
        (lambda (message)
          (loop (cdr alts) (cons message errs))))])))

(define (unparse-alt out v table exprs)
  (parameterize ([current-name-seqs (make-hasheq)])
    (cond
      [(= (length exprs) 1)
       (unparse-expr out v table (cdar exprs))]
      [else
       (let loop ([exprs exprs] [vals v])
         (cond
           [(null? exprs) (ok v)]
           [else
            (match-define (cons expr-id expr-e)
              (car exprs))
            (define val-id (caar vals))
            (define expected-name (next-name expr-id))
            (cond
              [(eq? val-id expected-name)
               (res-bind
                (unparse-expr out (cdar vals) table expr-e v)
                (lambda (_v)
                  (loop (cdr exprs)
                        (cdr vals))))]
              [else
               (err (format "field name mismatch~n  expected: ~a~n  found: ~a~n  in: ~e" expected-name val-id vals))])]))])))

(define (unparse-expr out v table expr [context null])
  (match expr
    [(? byte?)
     (cond
       [(equal? v expr)
        (begin0 (ok v)
          (write-byte expr out))]
       [else (err (format "expected ~a but got ~s" expr v))])]
    [(? char?)
     (cond
       [(equal? v expr)
        (begin0 (ok v)
          (write-byte (char->integer expr) out))]
       [else (err (format "expected ~a but got ~s" expr v))])]
    [(? symbol?)
     (define unparser
       (hash-ref table expr #f))
     (if unparser
         (unparser out v)
         (err (format "unparser '~a' not defined" expr)))]
    [`(repeat ,e ,(? exact-nonnegative-integer? n))
     (unparse-repeat out v table e n context)]
    [`(repeat ,e ,id)
     (match (assq id context)
       [(cons _ (? exact-integer? n))
        (unparse-repeat out v table e n context)]
       [(cons _ v)
        (err (format "context var ~a contains ~a, which is not an integer" id v))]
       [_
        (err (format "failed to look up ~a from context" id))])]
    [`(plus ,e)
     (unparse-plus out v table e context)]
    [`(star ,e)
     (unparse-star out v table e context)]
    [_
     (err (format "invalid unparser expression ~s" expr))]))

(define (unparse-repeat out v table expr n context)
  (define cached-unparser
    (and (symbol? expr)
         (hash-ref table expr #f)))
  (let loop ([n n] [v v])
    (if (<= n 0)
        (ok v)
        (res-bind
         (if cached-unparser
             (cached-unparser out (car v))
             (unparse-expr out (car v) table expr context))
         (lambda (_)
           (loop (sub1 n) (cdr v)))))))

(define (unparse-star out v table expr context)
  (if (null? v)
      (ok v)
      (match (unparse-expr out (car v) table expr context)
        [(err _message) (ok v)]
        [(ok _) (unparse-star out (cdr v) table expr context)])))

(define (unparse-plus out v table expr context)
  (if (pair? v)
      (res-bind
       (unparse-expr out (car v) table expr context)
       (lambda (_)
         (unparse-star out (cdr v) table expr context)))
      (unparse-expr out v table expr context)))

(provide
 unparse-eof
 unparse-nul)

(define (unparse-eof _out v)
  (match v
    [(? eof-object?) (ok v)]
    [_ (err (format "expected EOF but found ~s" v))]))

(define (unparse-nul out v)
  (match v
    [0 (begin0 (ok v)
         (write-byte 0 out))]
    [_ (err (format "expected NUL but found ~s" v))]))

(define ((make-unparse-flt who len [big-endian? (system-big-endian?)]) out v)
  (cond
    [(real? v)
     (begin0 (ok v)
       (write-bytes (real->floating-point-bytes v len big-endian?) out))]
    [else (err (format "expected '~a' but found ~s" who v))]))

(define-syntax (define-flt-unparsers stx)
  (syntax-parse stx
    [(_ [id:id len (~optional big-endian?)] ...)
     #:with (unparser-id ...) (for/list ([stx (in-list (syntax-e #'(id ...)))])
                                (format-id stx "unparse-~a" stx))
     #'(begin
         (provide unparser-id ...)
         (define unparser-id
           (make-unparse-flt 'id len (~? big-endian? (system-big-endian?))))
         ...)]))

(define-flt-unparsers
  [f32   4]
  [f64   8]
  [f32le 4 #f]
  [f64le 8 #f]
  [f32be 4 #t]
  [f64be 8 #t])

(define ((make-unparse-int who len signed? [big-endian? (system-big-endian?)]) out v)
  (cond
    [(integer? v)
     (begin0 (ok v)
       (write-bytes (integer->integer-bytes v len signed? big-endian?) out))]
    [else (err (format "expected '~a' but found ~s" who v))]))

(define-syntax (define-int-unparsers stx)
  (syntax-parse stx
    [(_ [id:id len signed? (~optional big-endian?)] ...)
     #:with (unparser-id ...) (for/list ([stx (in-list (syntax-e #'(id ...)))])
                                (format-id stx "unparse-~a" stx))
     #'(begin
         (provide unparser-id ...)
         (define unparser-id
           (make-unparse-int 'id len signed? (~? big-endian? (system-big-endian?))))
         ...)]))

(define-int-unparsers
  [u8    1 #f]
  [u16   2 #f]
  [u16le 2 #f #f]
  [u16be 2 #f #t]
  [u32   4 #f]
  [u32le 4 #f #f]
  [u32be 4 #f #t]
  [u64   8 #f]
  [u64le 8 #f #f]
  [u64be 8 #f #t]
  [i8    1 #t]
  [i16   2 #t]
  [i16le 2 #t #f]
  [i16be 2 #t #t]
  [i32   4 #t]
  [i32le 4 #t #f]
  [i32be 4 #t #t]
  [i64   8 #t]
  [i64le 8 #t #f]
  [i64be 8 #t #t])

(define (reinterpret bits n)
  (define len (quotient bits 8))
  (integer-bytes->integer
   (integer->integer-bytes n len #t #t)
   #f #t))

(define ((make-unparse-varint _who bits signed?) out v)
  (let ([v (if signed?
               (bitwise-xor
                (arithmetic-shift v 1)
                (arithmetic-shift v (- (sub1 bits))))
               (if (< v 0)
                   (reinterpret bits v)
                   v))])
    (define bs
      (let loop ([bs null] [n v])
        (define-values (q r)
          (quotient/remainder n #x80))
        (if (zero? q)
            (apply bytes (reverse (cons r bs)))
            (loop (cons (bitwise-ior r #x80 r) bs) q))))
    (begin0 (ok v)
      (write-bytes bs out))))

(define-syntax (define-varint-unparsers stx)
  (syntax-parse stx
    [(_ [id:id bits signed?] ...)
     #:with (unparser-id ...) (for/list ([stx (in-list (syntax-e #'(id ...)))])
                                (format-id stx "unparse-~a" stx))
     #'(begin
         (provide unparser-id ...)
         (define unparser-id
           (make-unparse-varint 'id bits signed?))
         ...)]))

(define-varint-unparsers
  [uvarint32 32 #f]
  [uvarint64 64 #f]
  [varint32  32 #t]
  [varint64  64 #t])
