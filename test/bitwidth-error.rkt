#lang racket/base

;; Rosette is unsound outside the bitwidth used to validate.
;; This should not be a limitation.
;; https://github.com/bennn/rosette-contract/issues/2

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
(f (expt 2 21)) ;; Should raise a contract error
