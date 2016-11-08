#lang racket/base

;; Rosette is unsound outside the bitwidth used to validate.
;; This should not be a limitation -- check that we don't skip the contract
;;  check for arguments outside the bitwidth.

(require
  rosette-contract
  ;racket/contract
)

;; =============================================================================

(define/contract (f x)
  (-> positive? negative?)
  (if (x . > . (expt 2 20))
    32
    (* x -1)))

;; =============================================================================

(module+ test
  (require rackunit)

  (check-equal? (f 3) -3)

  (check-exn exn:fail:contract?
    (lambda () (f -2)))

  (check-exn exn:fail:contract?
    (lambda () (f (expt 2 21))))
)
