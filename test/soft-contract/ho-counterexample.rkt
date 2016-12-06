#lang racket/base
(require rosette-contract)

;; TODO should tell that `f` can break its contract
;; ALSO generate a counterexample (a function)
;; - (g 42) = 100
;; - (g 42) = (something that doesn't divide 1)

(define/contract (f g)
  (-> (-> integer? integer?) integer?)
  (/ 1 (- 100 (g 42))))

(module+ test
  (require rackunit)

  (check-exn #rx"division by zero"
    (λ () (f (λ (_) 100))))

  (check-exn #rx"broke its own contract"
    (λ () (f (λ (_) 98))))

)
