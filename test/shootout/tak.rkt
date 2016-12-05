#lang racket/base

;; TODO fails to verify, out of memory

(require rosette-contract)
;(require racket/contract)

(define nat? natural-number/c)

(define/contract (tak x y z)
  (-> nat? nat? nat? nat?)
  (cond ((not (< y x)) z)
        (else (tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y)))))

(define n 8)
(time (void (tak (* n 3) (* n 2) n)))
