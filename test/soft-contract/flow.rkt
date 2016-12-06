#lang racket/base
(require rosette-contract)

(define/contract (negate x)
  (-> positive? negative?)
  (* x -1))

(define/contract (g x)
  (-> integer? negative?)
  (if (positive? x)
    (negate x)
    (negate 8)))

(module+ test
  (require rackunit)

  (check-equal? (g 1) -1)
  (check-equal? (g 2) -2)
  (check-exn exn:fail:contract?
    (lambda () (g 2.24)))

  (check-equal? (g -1) -8)
)
