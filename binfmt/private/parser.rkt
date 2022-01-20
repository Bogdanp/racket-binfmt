#lang racket/base

(require racket/match
         "error.rkt"
         "lexer.rkt")

(provide
 parse)

(define (parse in)
  (define l (make-lexer in))
  (port-count-lines! in)
  (define defs
    (let loop ([defs null])
      (match (lexer-peek l)
        [(token 'eof _ _ _ _) (reverse defs)]
        [_ (loop (cons (parse-def l) defs))])))
  (datum->syntax #f `(definitions ,@defs) (srcloc (object-name in) 1 0 1 #f)))

(define (parse-def l)
  (define id (stx l (expect 'parse-definition l 'ident)))
  (void (expect 'parse-definition l 'equal))
  (let loop ([alts null])
    (match (lexer-peek l)
      [(token 'semicolon _ _ _ _)
       (begin0 `(def ,id ,(reverse alts))
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
  (define t (expect 'parse-expr l '(ident char)))
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

(define (expect who l kind)
  (define (ok? k)
    (if (pair? kind)
        (and (member k kind) #t)
        (eq? k kind)))
  (match (lexer-read l)
    [(and t (token (app ok? #t) _ _ _ _)) t]
    [(token k _ _ _ _) (oops! who (format "expected ~a but found ~a" kind k) (lexer-in l))]))

(define (stx l t [e (token-val t)])
  (datum->syntax #f e (srcloc (object-name (lexer-in l)) (token-line t) (token-col t) (token-pos t) #f)))
