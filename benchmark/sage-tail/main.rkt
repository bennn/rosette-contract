#lang racket/base
(require rosette-contract)

;; Example from Sage paper, "Space-Efficient Gradual Typing"
;; https://sage.soe.ucsc.edu/tfp07-herman-tomb-flanagan.pdf
;;
;; "naive gradual typing breaks tail recursion"

(define/contract (even? n)
  (-> integer? any/c)
  (if (zero? n) #true (odd? (- n 1))))

(define/contract (odd? n)
  (-> integer? boolean?)
  (if (zero? n) #false (even? (- n 1))))

(define (main)
  (for ([i (in-range (expt 10 3))])
    (even? i)
    (odd? i)
    (void)))

(time (main))

