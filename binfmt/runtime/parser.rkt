#lang racket/base

(require racket/match
         racket/port)

(provide
 make-parser-table
 make-parser
 parse)

(define current-name-seqs
  (make-parameter (make-hasheq)))

(define (next-name id)
  (define seqs (current-name-seqs))
  (hash-update! seqs id add1 0)
  (string->symbol (format "~a_~a" id (hash-ref seqs id))))

(define (parse table parser in)
  (define res
    (parameterize ([current-name-seqs (make-hasheq)])
      (parse-expr in table parser)))
  (if (ok? res)
      (ok-v res)
      (error parser (format "parse failed~n ~a" (err-message res)))))

(define (make-parser-table)
  (make-hasheq
   `((eof   . ,parse-eof)
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

(define parse-f32   (make-parse-flt 'f32 4))
(define parse-f64   (make-parse-flt 'f64 8))
(define parse-f32le (make-parse-flt 'f32 4 #f))
(define parse-f64le (make-parse-flt 'f64 8 #f))
(define parse-f32be (make-parse-flt 'f32 4 #t))
(define parse-f64be (make-parse-flt 'f64 8 #t))

(define ((make-parse-byte who signed?) in)
  (define n (read-byte in))
  (cond
    [(eof-object? n)
     (make-err in "expected '~a' but found EOF" who)]
    [else
     (ok (if (and signed? (> n 127)) (- n 256) n))]))

(define parse-u8 (make-parse-byte 'u8 #f))
(define parse-i8 (make-parse-byte 'i8 #t))

(define ((make-parse-int who len signed? [big-endian? (system-big-endian?)]) in)
  (define bs (read-bytes len in))
  (cond
    [(eof-object? bs) (make-err in "expected '~a' but found EOF" who)]
    [(< (bytes-length bs) len) (make-err in "not enough bytes for ~a" who)]
    [else (ok (integer-bytes->integer bs signed? big-endian?))]))

(define parse-u16   (make-parse-int 'u16 2 #f))
(define parse-u16le (make-parse-int 'u16 2 #f #f))
(define parse-u16be (make-parse-int 'u16 2 #f #t))
(define parse-u32   (make-parse-int 'u32 4 #f))
(define parse-u32le (make-parse-int 'u32 4 #f #f))
(define parse-u32be (make-parse-int 'u32 4 #f #t))
(define parse-u64   (make-parse-int 'u64 8 #f))
(define parse-u64le (make-parse-int 'u64 8 #f #f))
(define parse-u64be (make-parse-int 'u64 8 #f #t))
(define parse-i16   (make-parse-int 'i16 2 #t))
(define parse-i16le (make-parse-int 'i16 2 #t #f))
(define parse-i16be (make-parse-int 'i16 2 #t #t))
(define parse-i32   (make-parse-int 'i32 4 #t))
(define parse-i32le (make-parse-int 'i32 4 #t #f))
(define parse-i32be (make-parse-int 'i32 4 #t #t))
(define parse-i64   (make-parse-int 'i64 8 #t))
(define parse-i64le (make-parse-int 'i64 8 #t #f))
(define parse-i64be (make-parse-int 'i64 8 #t #t))

(struct ok (v) #:transparent)
(struct err (message) #:transparent)

(define (make-err in message . args)
  (define filename (object-name in))
  (define-values (line col pos)
    (port-next-location in))
  (define formatted-message
    (apply format message args))
  (err
   (if (and line col)
       (format "~a~n  in: ~a~n  line: ~a~n  col: ~a" formatted-message filename line col)
       (format "~a~n  in: ~a~n  position: ~a" formatted-message filename pos))))

(define (res-bind res proc)
  (cond
    [(ok? res) (proc (ok-v res))]
    [else res]))

(define (err-bind res proc)
  (cond
    [(ok? res) res]
    [else (proc (err-message res))]))
