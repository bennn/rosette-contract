#lang racket/base
(require rosette-contract)

;; Example from the Sage paper
;; https://sage.soe.ucsc.edu/tfp07-herman-tomb-flanagan.pdf
;;
;; Higher-order wrappers

(define/contract (even n k)
  (-> integer? (-> any/c any/c) any/c)
  (if (zero? n) (k #true) (odd (- n 1) k)))

(define/contract (odd n k)
  (-> integer? (-> boolean? boolean?) boolean?)
  (if (zero? n) (k #false) (even (- n 1) k)))

(define (main)
  (for ([i (in-range 40)])
    (even i (λ (x) x))
    (odd i (λ (x) x))
    (void)))

(time (main))
