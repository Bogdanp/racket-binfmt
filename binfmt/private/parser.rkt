#lang racket/base

(require racket/format
         racket/match
         "error.rkt"
         "lexer.rkt")

(provide
 parse)

(define (parse in)
  (define l (make-lexer in))
  (port-count-lines! in)
  (define mod
    (let loop ([foreign-parsers null]
               [definitions null])
      (match (lexer-peek l)
        [(token 'eof _ _ _ _)
         `((foreign-parsers ,@(reverse foreign-parsers))
           (definitions ,@(reverse definitions)))]
        [(token 'keyword 'foreign-parsers _ _ _)
         (loop (cons (parse-foreign-parsers l) foreign-parsers) definitions)]
        [_
         (loop foreign-parsers (cons (parse-def l) definitions))])))
  (datum->syntax #f mod (srcloc (object-name in) 1 0 1 #f)))

(define (parse-foreign-parsers l)
  (expect 'parse-foreign-parsers l 'keyword 'foreign-parsers)
  (define mod (stx l (expect 'parse-foreign-parsers l 'string)))
  (let loop ([pairs null])
    (match (lexer-peek l)
      [(token 'semicolon _ _ _ _)
       (begin0 `(foreign-parsers ,mod ,(reverse pairs))
         (void (expect 'parse-foreign-parsers l 'semicolon)))]
      [_ (loop (cons (parse-foreign-parser-pair l) pairs))])))

(define (parse-foreign-parser-pair l)
  (expect 'parse-foreign-parser-pair l 'lbrace)
  (define uid (stx l (expect 'parse-foreign-parser-pair l 'ident)))
  (define pid (stx l (expect 'parse-foreign-parser-pair l 'ident)))
  (begin0 `(,uid ,pid)
    (expect 'parse-foreign-parser-pair l 'rbrace)))

(define (parse-def l)
  (define id (stx l (expect 'parse-definition l 'ident)))
  (void (expect 'parse-definition l 'equal))
  (let loop ([alts null])
    (match (lexer-peek l)
      [(token 'semicolon _ _ _ _)
       (begin0 `(definition ,id ,(reverse alts))
         (void (expect 'parse-defintion l 'semicolon)))]
      [_ (loop (cons (parse-alt l) alts))])))

(define (parse-alt l)
  (let loop ([exprs null])
    (match (lexer-peek l)
      [(token 'semicolon _ _ _ _)
       `(alt ,(reverse exprs))]
      [(token 'pipe _ _ _ _)
       (begin0 `(alt ,(reverse exprs))
         (lexer-read l))]
      [_ (loop (cons (parse-expr l) exprs))])))

(define (parse-expr l)
  (define t
    (match (expect 'parse-expr l '(ident char integer))
      [(token 'integer (? (compose1 not byte?) b) _ _ _)
       (oops! 'parse-expr
              (format "byte values must be between 0x00 and 0xFF~n got: 0x~a (decimal ~a)"
                      (~r #:base 16 #:min-width 2 #:pad-string "0" b) b)
              (lexer-in l))]
      [t t]))
  (define e (stx l t))
  (match (lexer-peek l)
    [(and t (token 'lbrace _ _ _ _))
     (void (expect 'parse-expr l 'lbrace))
     (define sub (stx l (expect 'parse-expr l '(ident integer))))
     (void (expect 'parse-expr l 'rbrace))
     (stx l t `(repeat ,e ,sub))]
    [(and t (token 'star _ _ _ _))
     (begin0 (stx l t `(star ,e))
       (lexer-read l))]
    [(and t (token 'plus _ _ _ _))
     (begin0 (stx l t `(plus ,e))
       (lexer-read l))]
    [_ e]))

(define (expect who l kind [value #f])
  (define (ok? k)
    (if (pair? kind)
        (and (member k kind) #t)
        (eq? k kind)))
  (define t
    (match (lexer-read l)
      [(and t (token (app ok? #t) _ _ _ _)) t]
      [(token k _ _ _ _) (oops! who (format "expected ~a but found ~a" kind k) (lexer-in l))]))
  (cond
    [(not value) t]
    [(not (equal? value (token-val t)))
     (oops! who (format "expected ~a but found ~a" value (token-val t)) (lexer-in l))]
    [else t]))

(define (stx l t [e (token-val t)])
  (datum->syntax #f e (srcloc (object-name (lexer-in l)) (token-line t) (token-col t) (token-pos t) #f)))
