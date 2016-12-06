#lang racket/base
(require rosette-contract)

(define/contract (negate x)
  (-> positive? negative?)
  (* x -1))

(module+ test
  (require rackunit)

  (check-equal? (negate 1) -1)
  (check-equal? (negate 2) -2)
  (check-equal? (negate 2.24) -2.24)
  (check-exn exn:fail:contract?
    (lambda () (negate -1)))
)
