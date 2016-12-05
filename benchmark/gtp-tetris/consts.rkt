#lang racket/base
(require rosette-contract)

(provide (contract-out
  [board-width integer?]
  [board-height integer?]
))

#;(: block-size Integer)
(define block-size 20)

#;(: board-height Integer)
(define board-height 20)

#;(: board-width Integer)
(define board-width 10)

