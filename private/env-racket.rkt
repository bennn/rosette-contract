#lang racket/base

;; Reprovide identifiers from racket/base, racket/contract, etc.

(provide
  symbol?
  (all-from-out racket/contract)
)

(require
  (only-in racket/contract
    ->*
    listof or/c
    vectorof))
