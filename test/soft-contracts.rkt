#lang racket/base

(require
  rosette-contract
)

;; =============================================================================

(define/contract (f x)
  (-> positive? negative?)
  (* x -1))

(define/contract (g x)
  (-> integer? negative?)
  (if (positive? x)
    (f x)
    (f 8)))

(g 3)
