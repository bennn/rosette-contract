#lang racket/base
(require rosette-contract)

;; First-order counterexamples

;; TODO Rosette's division should guard against 0, etc.

(define/contract (f1 n)
  (-> integer? integer?)
  (/ 1 n))

(define/contract (f2 n)
  (-> integer? integer?)
  (/ 0 (- 100 n)))

(define/contract (f3 n)
  (-> integer? integer?)
  (if (zero? n)
    #f
    n))

(define/contract (f4 n1 n2)
  (-> integer? integer? integer?)
  (if (zero? (* n1 n2))
    0
    #f))

;; -----------------------------------------------------------------------------

(module+ test
  (require rackunit)

  (check-exn #rx"broke its own contract"
    (λ () (f1 98)))

  (check-exn #rx"undefined for 0"
    (λ () (f2 100)))

  (check-exn exn:fail:contract?
    (λ () (f3 0)))

  (check-equal? (f3 4) 4)

  (check-exn exn:fail:contract?
    (λ () (f4 1 1)))

  (check-equal? (f4 0 1) 0)

)
