#lang info

(define license 'BSD-3-Clause)
(define collection "binfmt")
(define deps '("base"
               "binfmt-lib"))
(define build-deps '("racket-doc" "scribble-lib"))
(define scribblings '(("binfmt-manual.scrbl" () (parsing-library))))
(define implies '("binfmt-lib"))
