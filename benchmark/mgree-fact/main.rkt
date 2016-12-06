#lang racket/base
(require rosette-contract)

(define/contract (fact x y)
  (-> natural-number/c natural-number/c natural-number/c)
  (if (zero? x) y (if (<= x 5) (fact (- x 1) (* x y)) (fact 5 y))))

(define (main)
  (fact 5 0))

(time (main))
