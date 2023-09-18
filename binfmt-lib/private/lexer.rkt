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
    [#\#
     (read-line in)
     (do-lexer-read l)]
    [#\; (make-token 'semicolon (read-char in))]
    [#\= (make-token 'equal     (read-char in))]
    [#\* (make-token 'star      (read-char in))]
    [#\+ (make-token 'plus      (read-char in))]
    [#\| (make-token 'pipe      (read-char in))]
    [#\{ (make-token 'lbrace    (read-char in))]
    [#\} (make-token 'rbrace    (read-char in))]
    [#\' (make-token 'char      (read-literal-char in))]
    [#\" (make-token 'string    (read-literal-string in))]
    [#\@ (make-token 'keyword   (read-keyword in))]
    [(app char-general-category (or 'll 'lu))
     (make-token 'ident (read-ident in))]
    [(app char-general-category 'nd)
     (make-token 'integer (read-integer in))]
    [_
     (oops! 'lexer-read (format "unexpected char ~s" (read-char in)) in line col pos)]))

(define (read-literal-char in)
  (consume 'read-literal-char in #\')
  (begin0 (read-char in)
    (consume 'read-literal-char in #\')))

(define (read-literal-string in)
  (consume 'read-literal-string in #\")
  (call-with-output-string
   (lambda (out)
     (let loop ([escaped? #f])
       (match (read-char in)
         [(? eof-object?)
          (oops! 'read-literal-string "unexpected EOF while reading string literal" in)]
         [#\\
          (cond
            [escaped?
             (write-char #\\ out)
             (loop #f)]
            [else
             (loop #t)])]
         [#\"
          (cond
            [escaped?
             (write-char #\" out)
             (loop #f)]
            [else
             (void)])]
         [c
          (write-char c out)
          (loop #f)])))))

(define (read-keyword in)
  (consume 'read-keyword in #\@)
  (read-ident in))

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
    [(equal? (peek-string 2 0 in) "0x")
     (void (read-string 2 in))
     (do-read-integer in 16 hex-digit? hex-char->number)]
    [(equal? (peek-char in) #\0)
     (begin0 0
       (void (read-char in)))]
    [else
     (do-read-integer in 10 digit? char->digit)]))

(define (do-read-integer in base digit? char->digit)
  (define first-ch (read-char in))
  (when (eof-object? first-ch)
    (oops! 'read-integer "expected a digit but found EOF" in))
  (let loop ([n (char->digit first-ch)])
    (match (peek-char in)
      [(? eof-object?) n]
      [(? digit?) (loop (+ (* n base) (char->digit (read-char in))))]
      [_ n])))

(define (digit? c)
  (memv c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))

(define (char->digit c)
  (- (char->integer c) 48))

(define (hex-digit? c)
  (or (digit? c) (memv c '(#\A #\B #\C #\D #\E #\F #\a #\b #\c #\d #\e #\f))))

(define (hex-char->number c)
  (if (digit? c)
      (char->digit c)
      (case c
        [(#\A #\B #\C #\D #\E #\F) (- (char->integer c) 55)]
        [(#\a #\b #\c #\d #\e #\f) (- (char->integer c) 87)]
        [else (raise-argument-error 'hex-char->number "hex-digit?" c)])))

(define (consume who in expected)
  (define actual (peek-char in))
  (unless (char=? actual expected)
    (oops! who (format "expected ~a but found ~a" expected actual) in))
  (void (read-char in)))
