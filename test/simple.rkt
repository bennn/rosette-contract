#lang racket/base

(require
  rosette-contract
  #;racket/contract)

(define/contract (f x)
  (-> positive? negative?)
  (* x -1))

(f 1)
(f 2)
(f 3)
