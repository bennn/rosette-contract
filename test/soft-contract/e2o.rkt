#lang racket/base
(require rosette-contract)

;; TODO need to handle higher-order functions

(define/contract (e2o f)
  (-> (-> even? even?) (-> odd? odd?))
  (λ (n) (- (f (+ n 1)) 1)))

(module+ test
  (require rackunit)

  (define (add2 n) (+ n 2))

  (check-equal? ((e2o add2) 1) 3)

  (check-exn exn:fail:contract?
    (λ () ((e2o add2) 0)))

)
