#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         racket/format
         racket/match
         racket/port
         "name.rkt"
         "res.rkt")

(provide
 make-parser-table
 make-parser
 parse)

(define (parse table parser in)
  (define res
    (parameterize ([current-name-seqs (make-hasheq)])
      (parse-expr in table parser)))
  (match res
    [(ok v) v]
    [(err message)
     (error parser (format "parse failed~n ~a" message))]))

(define (make-parser-table)
  (make-hasheq
   `((TODO  . ,parse-TODO)
     (eof   . ,parse-eof)
     (nul   . ,parse-nul)
     (f32   . ,parse-f32)
     (f64   . ,parse-f64)
     (f32le . ,parse-f32le)
     (f64le . ,parse-f64le)
     (f32be . ,parse-f32be)
     (f64be . ,parse-f64be)
     (u8    . ,parse-u8)
     (i8    . ,parse-i8)
     (u16   . ,parse-u16)
     (u32   . ,parse-u32)
     (u64   . ,parse-u64)
     (u16le . ,parse-u16le)
     (u32le . ,parse-u32le)
     (u64le . ,parse-u64le)
     (u16be . ,parse-u16be)
     (u32be . ,parse-u32be)
     (u64be . ,parse-u64be)
     (i16   . ,parse-i16)
     (i32   . ,parse-i32)
     (i64   . ,parse-i64)
     (i16le . ,parse-i16le)
     (i32le . ,parse-i32le)
     (i64le . ,parse-i64le)
     (i16be . ,parse-i16be)
     (i32be . ,parse-i32be)
     (i64be . ,parse-i64be))))

(define ((make-parser table . alts) in)
  (parse-alts in table alts))

(define (parse-alts in table alts)
  (define pos
    (file-position in))
  (let loop ([alts alts]
             [errs null])
    (cond
      [(null? alts)
       (let ([errs (reverse errs)])
         (err (call-with-output-string
               (lambda (out)
                 (fprintf out "~a" (car errs))
                 (for ([err (in-list (cdr errs))])
                   (fprintf out "~nor ~a" err))))))]
      [else
       (err-bind
        (parse-alt in table (car alts))
        (lambda (message)
          (file-position in pos)
          (loop (cdr alts) (cons message errs))))])))

(define (parse-alt in table exprs)
  (parameterize ([current-name-seqs (make-hasheq)])
    (let loop ([exprs exprs]
               [results null])
      (cond
        [(null? exprs)
         (if (null? (cdr results))
             (ok (cdar results))
             (ok (reverse results)))]
        [else
         (match-define (cons expr-id expr-e)
           (car exprs))
         (res-bind
          (parse-expr in table expr-e results)
          (lambda (v)
            (define res (cons (next-name expr-id) v))
            (loop (cdr exprs) (cons res results))))]))))

(define (parse-expr in table expr [context null])
  (match expr
    [(? byte?)
     (parse-byte in expr)]
    [(? char?)
     (parse-char in expr)]
    [(? symbol?)
     (define parser
       (hash-ref table expr #f))
     (if parser
         (parser in)
         (make-err in "parser '~a' not defined" expr))]
    [`(repeat ,e ,(? exact-nonnegative-integer? n))
     (parse-repeat in table e n context)]
    [`(repeat ,e ,id)
     (match (assq id context)
       [(cons _ (? exact-nonnegative-integer? n))
        (parse-repeat in table e n context)]
       [(cons _ v)
        (make-err in "context var ~a contains ~a, which is not a positive integer" id v)]
       [_
        (make-err in "failed to look up ~a from context" id)])]
    [`(plus ,e)
     (parse-plus in table e context)]
    [`(star ,e)
     (parse-star in table e context)]
    [_
     (make-err in "invalid parser expression ~s" expr)]))

(define (parse-repeat in table expr n context)
  (define cached-parser
    (and (symbol? expr)
         (hash-ref table expr #f)))
  (let loop ([n n] [results null])
    (cond
      [(zero? n)
       (ok (reverse results))]
      [else
       (define res
         (if cached-parser
             (cached-parser in)
             (parse-expr in table expr context)))
       (cond
         [(ok? res) (loop (sub1 n) (cons (ok-v res) results))]
         [else res])])))

(define (parse-star in table expr context)
  (let loop ([results null])
    (define pos (file-position in))
    (define res (parse-expr in table expr context))
    (cond
      [(err? res)
       (file-position in pos)
       (ok (reverse results))]
      [else
       (loop (cons (ok-v res) results))])))

(define (parse-plus in table expr context)
  (res-bind
   (parse-expr in table expr context)
   (lambda (v)
     (ok (cons v (ok-v (parse-star in table expr context)))))))

(define (parse-TODO in)
  (make-err in "TODO: parser not implemented"))

(provide
 parse-byte
 parse-char
 parse-eof
 parse-nul)

(define (~byte n)
  (~a "0x" (~r #:base 16 #:min-width 2 #:pad-string "0" n)))

(define (parse-byte in n)
  (match (read-byte in)
    [(== n) (ok n)]
    [(? eof-object?) (make-err in "expected ~a but found EOF" (~byte n))]
    [b (make-err in "expected ~a but found ~a" (~byte n) (~byte b))]))

(define (parse-char in ch)
  (match (read-byte in)
    [(== (char->integer ch)) (ok ch)]
    [(? eof-object?) (make-err in "expected '~a' but found EOF" ch)]
    [c (make-err in "expected '~a' but found '~a'" ch (integer->char c))]))

(define (parse-eof in)
  (match (read-byte in)
    [(? eof-object?) (ok eof)]
    [c (make-err in "expected EOF but found '~a'" c)]))

(define (parse-nul in)
  (match (read-byte in)
    [0 (ok 0)]
    [(? eof-object?) (make-err in "expected NUL but found EOF")]
    [n (make-err in "expected NUL but found '~a'" n)]))

(define ((make-parse-flt who len [big-endian? (system-big-endian?)]) in)
  (define bs (read-bytes len in))
  (cond
    [(eof-object? bs) (make-err in "expected '~a' but found EOF" who)]
    [(< (bytes-length bs) len) (make-err in "not enough bytes for ~a" who)]
    [else (ok (floating-point-bytes->real bs big-endian?))]))

(define-syntax (define-flt-parsers stx)
  (syntax-parse stx
    [(_ [id:id len (~optional big-endian?)] ...)
     #:with (parser-id ...) (for/list ([stx (in-list (syntax-e #'(id ...)))])
                              (format-id stx "parse-~a" stx))
     #'(begin
         (provide parser-id ...)
         (define parser-id
           (make-parse-flt 'id len (~? big-endian? (system-big-endian?))))
         ...)]))

(define-flt-parsers
  [f32   4]
  [f64   8]
  [f32le 4 #f]
  [f64le 8 #f]
  [f32be 4 #t]
  [f64be 8 #t])

(define ((make-parse-byte who signed?) in)
  (define n (read-byte in))
  (cond
    [(eof-object? n)
     (make-err in "expected '~a' but found EOF" who)]
    [else
     (ok (if (and signed? (> n 127)) (- n 256) n))]))

(provide
 parse-u8
 parse-i8)

(define parse-u8 (make-parse-byte 'u8 #f))
(define parse-i8 (make-parse-byte 'i8 #t))

(define ((make-parse-int who len signed? [big-endian? (system-big-endian?)]) in)
  (define bs (read-bytes len in))
  (cond
    [(eof-object? bs) (make-err in "expected '~a' but found EOF" who)]
    [(< (bytes-length bs) len) (make-err in "not enough bytes for ~a" who)]
    [else (ok (integer-bytes->integer bs signed? big-endian?))]))

(define-syntax (define-int-parsers stx)
  (syntax-parse stx
    [(_ [id:id len signed? (~optional big-endian?)] ...)
     #:with (parser-id ...) (for/list ([stx (in-list (syntax-e #'(id ...)))])
                              (format-id stx "parse-~a" stx))
     #'(begin
         (provide parser-id ...)
         (define parser-id
           (make-parse-int 'id len signed? (~? big-endian? (system-big-endian?))))
         ...)]))

(define-int-parsers
  [u16   2 #f]
  [u16le 2 #f #f]
  [u16be 2 #f #t]
  [u32   4 #f]
  [u32le 4 #f #f]
  [u32be 4 #f #t]
  [u64   8 #f]
  [u64le 8 #f #f]
  [u64be 8 #f #t]
  [i16   2 #t]
  [i16le 2 #t #f]
  [i16be 2 #t #t]
  [i32   4 #t]
  [i32le 4 #t #f]
  [i32be 4 #t #t]
  [i64   8 #t]
  [i64le 8 #t #f]
  [i64be 8 #t #t])
