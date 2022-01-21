#lang racket/base

(require racket/match
         racket/port
         "endianness.rkt")

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
   `((eof . ,parse-eof)
     (f32 . ,parse-f32)
     (f64 . ,parse-f64)
     (u8  . ,parse-u8)
     (u16 . ,parse-u16)
     (u32 . ,parse-u32)
     (u64 . ,parse-u64)
     (i8  . ,parse-i8)
     (i16 . ,parse-i16)
     (i32 . ,parse-i32)
     (i64 . ,parse-i64)
     (nul . ,parse-nul))))

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
                 (fprintf out "~a" (err-message (car errs)))
                 (for ([err (in-list (cdr errs))])
                   (fprintf out "~nor ~a" (err-message err)))))))]
      [else
       (define res
         (parse-alt in table (car alts)))
       (cond
         [(ok? res)
          (ok (ok-v res))]
         [else
          (file-position in pos)
          (loop (cdr alts) (cons res errs))])])))

(define (parse-alt in table exprs)
  (parameterize ([current-name-seqs (make-hasheq)])
    (let loop ([exprs exprs]
               [results null])
      (cond
        [(null? exprs)
         (if (= (length results) 1)
             (ok (cdar results))
             (ok results))]
        [else
         (define expr (car exprs))
         (define expr-id (car expr))
         (define expr-e (cdr expr))
         (define res (parse-expr in table expr-e results))
         (cond
           [(ok? res) (loop (cdr exprs) (append results `((,(next-name expr-id) . ,(ok-v res)))))]
           [else res])]))))

(define (parse-expr in table expr [context null])
  (match expr
    [(? char?)
     (parse-char in expr)]
    [(? symbol?)
     (cond
       [(hash-ref table expr #f) => (λ (p) (p in))]
       [else (make-err in "parser '~a' not defined" expr)])]
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
  (let loop ([results null] [n n])
    (cond
      [(zero? n)
       (ok (reverse results))]
      [else
       (define pos (file-position in))
       (define res (parse-expr in table expr context))
       (cond
         [(err? res)
          (begin0 res
            (file-position in pos))]
         [else
          (loop (cons (ok-v res) results) (sub1 n))])])))

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
  (define res
    (parse-expr in table expr context))
  (if (ok? res)
      (ok (cons (ok-v res)
                (ok-v (parse-star in table expr context))))
      res))

(define (parse-char in ch)
  (match (peek-byte in)
    [(== (char->integer ch)) (ok (read-char in))]
    [(? eof-object?) (make-err in "expected '~a' but found EOF" ch)]
    [c (make-err in "expected '~a' but found '~a'" ch (integer->char c))]))

(define (parse-eof in)
  (match (peek-char in)
    [(? eof-object?) (ok eof)]
    [c (make-err in "expected EOF but found '~a'" c)]))

(define ((make-parse-flt who n-bytes) in)
  (define bs (peek-bytes n-bytes 0 in))
  (cond
    [(eof-object? bs) (make-err in "expected '~a' but found EOF" who)]
    [(< (bytes-length bs) n-bytes) (make-err in "not enough bytes for ~a" who)]
    [else (ok (floating-point-bytes->real (read-bytes n-bytes in) (big-endian?)))]))

(define parse-f32 (make-parse-flt 'f32 4))
(define parse-f64 (make-parse-flt 'f64 8))

(define ((make-parse-int who signed? n-bytes) in)
  (define bs (peek-bytes n-bytes 0 in))
  (cond
    [(eof-object? bs) (make-err in "expected '~a' but found EOF" who)]
    [(< (bytes-length bs) n-bytes) (make-err in "not enough bytes for ~a" who)]
    [else (ok (integer-bytes->integer (read-bytes n-bytes in) signed? (big-endian?)))]))

(define parse-u8  (make-parse-int 'u8  #f 1))
(define parse-u16 (make-parse-int 'u16 #f 2))
(define parse-u32 (make-parse-int 'u32 #f 4))
(define parse-u64 (make-parse-int 'u64 #f 8))
(define parse-i8  (make-parse-int 'i8  #t 1))
(define parse-i16 (make-parse-int 'i16 #t 2))
(define parse-i32 (make-parse-int 'i32 #t 4))
(define parse-i64 (make-parse-int 'i64 #t 8))

(define (parse-nul in)
  (match (peek-byte in)
    [0 (ok (read-byte in))]
    [(? eof-object?) (make-err in "expected NUL but found EOF")]
    [n (make-err in "expected NUL but found '~a'" n)]))

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
