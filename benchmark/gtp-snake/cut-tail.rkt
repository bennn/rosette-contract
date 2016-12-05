#lang racket/base
(require rosette-contract)

(provide (contract-out
  [cut-tail ((nelistof posn?) . -> . (listof posn?))]
))

(require "data.rkt"
         racket/list)

;; cut-tail : NeSegs -> Segs
;; Cut off the tail.
(define (cut-tail segs)
  (let ([r (cdr segs)])
    (cond [(empty? r) empty]
          [else (cons (car segs) (cut-tail r))])))

