#lang racket/base

(require racket/syntax
         syntax/parse/pre
         (prefix-in r: "../runtime.rkt")
         "parser.rkt")

(provide
 compile)

(define-syntax-class exp
  (pattern n:number
           #:with (used-id ...) #'()
           #:with id #'num
           #:with e #'n)
  (pattern ch:char
           #:with (used-id ...) #'()
           #:with id #'char
           #:with e #'ch)
  (pattern id:id
           #:with (used-id ...) #'(id)
           #:with e #''id)
  (pattern ({~datum repeat} sub-e:exp id-e:exp)
           #:with (used-id ...) #'(sub-e.used-id ...)
           #:with id #'sub-e.id
           #:with e #'`(repeat ,sub-e.e ,id-e.e))
  (pattern ({~datum star} sub-e:exp)
           #:with (used-id ...) #'(sub-e.used-id ...)
           #:with id #'sub-e.id
           #:with e #'`(star ,sub-e.e))
  (pattern ({~datum plus} sub-e:exp)
           #:with (used-id ...) #'(sub-e.used-id ...)
           #:with id #'sub-e.id
           #:with e #'`(plus ,sub-e.e)))

(define-syntax-class alt
  (pattern (sub-e:exp ...+)
           #:with e #'(list (cons 'sub-e.id sub-e.e) ...)
           #:with (used-id ...) #'(sub-e.used-id ... ...)))

(define-syntax-class foreign-parser
  (pattern ({~datum foreign-parsers} mod ({id:id uid:id} ...))
           #:with spec #'(only-in mod id ... uid ...)))

(define-syntax-class definition
  (pattern ({~datum definition} id (({~datum alt} alt:alt) ...))
           #:with unid (format-id #'id "un-~a" #'id)
           #:with (alt-used-id ...) #'(alt.used-id ... ...)
           #:with (alt-e ...) #'(alt.e ...)))

(define (compile in)
  (syntax-parse (parse in)
    [(({~datum foreign-parsers} fp:foreign-parser ...)
      ({~datum definitions} d:definition ...))
     (define builtin-parsers
       (for/hasheq ([id (in-hash-keys (r:make-parser-table))])
         (values id #t)))
     (define builtin&foreign-parsers
       (for/fold ([parsers builtin-parsers])
                 ([id (in-list (syntax-e #'(fp.id ... ...)))])
         (hash-set parsers (syntax->datum id) #t)))
     (define known-parsers
       (for/fold ([parsers builtin&foreign-parsers])
                 ([id (in-list (syntax-e #'(d.id ...)))])
         (hash-set parsers (syntax->datum id) #t)))
     (for ([stx (in-list (syntax-e #'(d.alt-used-id ... ...)))])
       (define id (syntax->datum stx))
       (unless (hash-has-key? known-parsers id)
         (raise-syntax-error 'compile (format "undefined parser ~a" id) stx)))

     #'(module parser racket/base
         (require (prefix-in r: binfmt/runtime))
         (require fp.spec ...)
         (define *parsers* (r:make-parser-table))
         (define *unparsers* (r:make-unparser-table))
         (hash-set! *parsers* 'd.id (r:make-parser *parsers* d.alt-e ...)) ...
         (hash-set! *parsers* 'fp.id fp.id) ... ...
         (hash-set! *unparsers* 'd.id (r:make-unparser *unparsers* d.alt-e ...)) ...
         (hash-set! *unparsers* 'fp.id fp.uid) ... ...
         (define (d.id [in (current-input-port)]) (r:parse *parsers* 'd.id in)) ...
         (define (d.unid v [out (current-output-port)]) (r:unparse *unparsers* 'd.id out v)) ...
         (provide d.id ... d.unid ... fp.id ... ... fp.uid ... ...))]))
