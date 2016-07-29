#lang racket/base

(require
  rosette-contract
)

;; =============================================================================

(define/contract (f x)
  (-> positive? negative?)
  (if (x . > . (expt 2 20))
    32
    (* x -1)))

(f 3)
(f (expt 2 21))
