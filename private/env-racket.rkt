#lang racket/base

;; Reprovide identifiers from racket/base, racket/contract, etc.

(provide
  symbol?
  void?
  (all-from-out racket/contract)
)

(require
  (only-in racket/contract
    ->*
    any/c
    none/c
    listof or/c
    vectorof))
