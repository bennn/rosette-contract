#lang info
(define collection "rosette-contract")
(define deps '("base" "rosette"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define pkg-desc "Rosette front-end to racket/contract")
(define version "0.0")
(define pkg-authors '(ben))
(define scribblings '(("scribblings/rosette-contract.scrbl" () (experimental))))
