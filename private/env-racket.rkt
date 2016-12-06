#lang racket/base

;; Reprovide identifiers from racket/base, racket/contract, etc.

(provide
  symbol?
  void?
  (all-from-out racket/class)
  (all-from-out racket/contract)
)

(require
  (only-in racket/class
    instanceof/c
    class/c)
  (only-in racket/contract
    ->i
    ->*
    and/c
    any/c
    between/c
    cons/c
    hash/c
    list/c
    listof
    none/c
    one-of/c
    or/c
    vectorof))
