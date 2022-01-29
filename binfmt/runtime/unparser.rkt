#lang racket/base

(require racket/match
         racket/port
         "res.rkt")

(provide
 make-unparser-table
 make-unparser
 unparse)

(define (unparse table unparser out v)
  (match (unparse-expr out v table unparser)
    [(ok _) (void)]
    [(err message)
     (error unparser (format "unparse failed~n ~a" message))]))

(define (make-unparser-table)
  (make-hasheq
   `((eof   . ,unparse-eof)
     (nul   . ,unparse-nul)
     (f32   . ,unparse-f32)
     (f64   . ,unparse-f64)
     (f32le . ,unparse-f32le)
     (f64le . ,unparse-f64le)
     (f32be . ,unparse-f32be)
     (f64be . ,unparse-f64be)
     (u8    . ,unparse-u8)
     (i8    . ,unparse-i8)
     (u16   . ,unparse-u16)
     (u32   . ,unparse-u32)
     (u64   . ,unparse-u64)
     (u16le . ,unparse-u16le)
     (u32le . ,unparse-u32le)
     (u64le . ,unparse-u64le)
     (u16be . ,unparse-u16be)
     (u32be . ,unparse-u32be)
     (u64be . ,unparse-u64be)
     (i16   . ,unparse-i16)
     (i32   . ,unparse-i32)
     (i64   . ,unparse-i64)
     (i16le . ,unparse-i16le)
     (i32le . ,unparse-i32le)
     (i64le . ,unparse-i64le)
     (i16be . ,unparse-i16be)
     (i32be . ,unparse-i32be)
     (i64be . ,unparse-i64be))))

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
  (cond
    [(= (length exprs) 1)
     (unparse-expr out v table (cdar exprs))]
    [else
     (let loop ([exprs exprs] [vals v])
       (cond
         [(null? exprs) (ok v)]
         [else
          (match-define (cons _expr-id expr-e)
            (car exprs))
          (res-bind
           (unparse-expr out (cdar vals) table expr-e v)
           (lambda (_v)
             (loop (cdr exprs)
                   (cdr vals))))]))]))

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
       [(cons _ (? exact-nonnegative-integer? n))
        (unparse-repeat out v table e n context)]
       [(cons _ v)
        (err (format "context var ~a contains ~a, which is not a positive integer" id v))]
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
    (if (zero? n)
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

(define unparse-f32   (make-unparse-flt 'f32 4))
(define unparse-f64   (make-unparse-flt 'f64 8))
(define unparse-f32le (make-unparse-flt 'f32 4 #f))
(define unparse-f64le (make-unparse-flt 'f64 8 #f))
(define unparse-f32be (make-unparse-flt 'f32 4 #t))
(define unparse-f64be (make-unparse-flt 'f64 8 #t))

(define ((make-unparse-int who len signed? [big-endian? (system-big-endian?)]) out v)
  (cond
    [(integer? v)
     (begin0 (ok v)
       (write-bytes (integer->integer-bytes v len signed? big-endian?) out))]
    [else (err (format "expected '~a' but found ~s" who v))]))

(define unparse-u8    (make-unparse-int 'u8  1 #f))
(define unparse-u16   (make-unparse-int 'u16 2 #f))
(define unparse-u16le (make-unparse-int 'u16 2 #f #f))
(define unparse-u16be (make-unparse-int 'u16 2 #f #t))
(define unparse-u32   (make-unparse-int 'u32 4 #f))
(define unparse-u32le (make-unparse-int 'u32 4 #f #f))
(define unparse-u32be (make-unparse-int 'u32 4 #f #t))
(define unparse-u64   (make-unparse-int 'u64 8 #f))
(define unparse-u64le (make-unparse-int 'u64 8 #f #f))
(define unparse-u64be (make-unparse-int 'u64 8 #f #t))
(define unparse-i8    (make-unparse-int 'i8  1 #t))
(define unparse-i16   (make-unparse-int 'i16 2 #t))
(define unparse-i16le (make-unparse-int 'i16 2 #t #f))
(define unparse-i16be (make-unparse-int 'i16 2 #t #t))
(define unparse-i32   (make-unparse-int 'i32 4 #t))
(define unparse-i32le (make-unparse-int 'i32 4 #t #f))
(define unparse-i32be (make-unparse-int 'i32 4 #t #t))
(define unparse-i64   (make-unparse-int 'i64 8 #t))
(define unparse-i64le (make-unparse-int 'i64 8 #t #f))
(define unparse-i64be (make-unparse-int 'i64 8 #t #t))
