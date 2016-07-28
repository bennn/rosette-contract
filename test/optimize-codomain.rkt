#lang racket/base

(require
  rosette-contract
)

;; =============================================================================

;; f should have no codomain contract
(define/contract (f x)
  (-> positive? negative?)
  (* x -1))

(f 3)
