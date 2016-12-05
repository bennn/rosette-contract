#lang racket/base
(require rosette-contract)

(provide (contract-out
  [eliminate-full-rows (-> (listof block?) (listof block?))]
))

(require "data.rkt"
         "bset.rkt"
         "consts.rkt"
         racket/list)

;; Eliminate all full rows and shift down appropriately.
#;(: eliminate-full-rows (-> BSet BSet))
(define (eliminate-full-rows bs)
  (elim-row bs board-height 0))

#;(: elim-row (-> BSet Integer Integer BSet))
(define (elim-row bs i offset)
  (cond [(< i 0) empty]
        [(full-row? bs i)   (elim-row bs (sub1 i) (add1 offset))]
        [else (blocks-union (elim-row bs (sub1 i) offset)
                            (blocks-move 0 offset (blocks-row
                                                   bs i)))]))
