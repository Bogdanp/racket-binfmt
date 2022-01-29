#lang racket/base

(require racket/syntax
         syntax/parse
         "parser.rkt")

(provide
 compile)

(define-syntax-class exp
  (pattern n:number
           #:with id #'num
           #:with e #'n)
  (pattern ch:char
           #:with id #'char
           #:with e #'ch)
  (pattern id:id
           #:with e #''id)
  (pattern ({~datum repeat} sub-e:exp id-e:exp)
           #:with id #'sub-e.id
           #:with e #'`(repeat ,sub-e.e ,id-e.e))
  (pattern ({~datum star} sub-e:exp)
           #:with id #'sub-e.id
           #:with e #'`(star ,sub-e.e))
  (pattern ({~datum plus} sub-e:exp)
           #:with id #'sub-e.id
           #:with e #'`(plus ,sub-e.e)))

(define-syntax-class alt
  (pattern (sub-e:exp ...+)
           #:with e #'(list (cons 'sub-e.id sub-e.e) ...)))

(define-syntax-class def
  (pattern ({~datum def} id (({~datum alt} alt:alt) ...))
           #:with unid (format-id #'id "un-~a" #'id)
           #:with (alt-e ...) #'(alt.e ...)))

(define (compile in)
  (syntax-parse (parse in)
    [({~datum definitions} d:def ...)
     #'(module parser racket/base
         (require (prefix-in r: binfmt/runtime))
         (define *parsers* (r:make-parser-table))
         (hash-set! *parsers* 'd.id (r:make-parser *parsers* d.alt-e ...)) ...
         (define *unparsers* (r:make-unparser-table))
         (hash-set! *unparsers* 'd.id (r:make-unparser *unparsers* d.alt-e ...)) ...
         (define (d.id [in (current-input-port)]) (r:parse *parsers* 'd.id in)) ...
         (define (d.unid v [out (current-output-port)]) (r:unparse *unparsers* 'd.id out v)) ...
         (provide d.id ... d.unid ...))]))
