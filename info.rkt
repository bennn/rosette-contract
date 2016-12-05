#lang info
(define collection "rosette-contract")
(define deps '("base" "rosette" "rackunit-lib" "typed-racket-lib" "typed-racket-more"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define pkg-desc "Rosette front-end to racket/contract")
(define version "0.0")
(define pkg-authors '(ben))
(define scribblings '(("scribblings/rosette-contract.scrbl" () (experimental))))
(define compile-omit-paths '("benchmark"))
