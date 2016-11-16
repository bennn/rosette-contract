#lang racket/base

(module math racket/base
  (require racket/contract)
  ;(require rosette-contract)

  (define (even? x)
    (zero? (modulo x 2)))

  (define (odd? x)
    (even? (+ x 1)))

  (define/contract (even++ n)
    (-> even? odd?)
    (+ n 1))

  (provide
    even? odd?
    even++))

(require 'math)
(require (only-in rosette define-symbolic integer? solve assert))
(define-symbolic x integer?)

(solve
  (assert (even? x)
          (not (odd? (even++ x)))))
