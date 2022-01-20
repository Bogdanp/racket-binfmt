#lang racket/base

(require racket/match
         racket/port
         "error.rkt")

(provide
 (struct-out token)
 make-lexer
 lexer?
 lexer-in
 lexer-peek
 lexer-read)

(struct token (kind val line col pos) #:transparent)
(struct lexer (in [peeked #:mutable]))

(define (make-lexer in)
  (lexer in #f))

(define (lexer-peek l)
  (cond
    [(lexer-peeked l)]
    [else
     (define tok (lexer-read l))
     (begin0 tok
       (set-lexer-peeked! l tok))]))

(define (lexer-read l)
  (cond
    [(lexer-peeked l)
     (begin0 (lexer-peeked l)
       (set-lexer-peeked! l #f))]
    [else
     (do-lexer-read l)]))

(define (do-lexer-read l)
  (define in (lexer-in l))
  (define-values (line col pos)
    (port-next-location in))
  (define (make-token kind val)
    (token kind val line col pos))
  (match (peek-char in)
    [(? eof-object?)
     (make-token 'eof eof)]
    [(or #\space #\tab #\return #\newline)
     (read-char in)
     (do-lexer-read l)]
    [#\; (make-token 'semicolon (read-char in))]
    [#\= (make-token 'equal     (read-char in))]
    [#\* (make-token 'times     (read-char in))]
    [#\+ (make-token 'plus      (read-char in))]
    [#\| (make-token 'pipe      (read-char in))]
    [#\{ (make-token 'lbrace    (read-char in))]
    [#\} (make-token 'rbrace    (read-char in))]
    [#\' (make-token 'char      (read-literal-char in))]
    [(app char-general-category (or 'll 'lu))
     (make-token 'ident (read-ident in))]
    [(app char-general-category 'nd)
     (make-token 'integer (read-integer in))]
    [_
     (oops! 'lexer-read (format "unexpected char ~s" (read-char in)) in line col pos)]))

(define (read-literal-char in)
  (expect 'read-literal-char in #\')
  (begin0 (read-char in)
    (expect 'read-literal-char in #\')))

(define (read-ident in)
  (string->symbol
   (call-with-output-string
    (lambda (out)
      (write-char (read-char in) out)
      (let loop ()
        (match (peek-char in)
          [(? eof-object?) (void)]
          [(or #\- #\_ (app char-general-category (or 'll 'lu 'nd)))
           (write-char (read-char in) out)
           (loop)]
          [_ (void)]))))))

(define (read-integer in)
  (cond
    [(char=? (peek-char in) #\0)
     (begin0 0
       (void (read-char in)))]
    [else
     (let loop ([n (char->digit (read-char in))])
       (match (peek-char in)
         [(? eof-object?) n]
         [(app char-general-category 'nd)
          (loop (+ (* n 10) (char->digit (read-char in))))]
         [_ n]))]))

(define (expect who in expected)
  (define actual (peek-char in))
  (unless (char=? actual expected)
    (oops! who (format "expected ~a but found ~a" expected actual) in))
  (void (read-char in)))

(define (char->digit c)
  (- (char->integer c) 48))