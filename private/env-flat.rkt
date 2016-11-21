#lang racket/base

;; Flat, solvable contracts

;; See `define-rfc` macro for provides.

(require
  rosette-contract/private/flat
  (only-in rosette
    positive? negative? integer?)
  (for-syntax racket/base syntax/parse)
)

;; =============================================================================

(define-syntax (define-rfc stx)
  (syntax-parse stx
   [(_ P:id D:id)
    #:with tmp-name (gensym (syntax-e #'P))
    #'(begin (provide (rename-out [tmp-name P]))
             (define tmp-name (make-solvable-predicate P #:domain D)))]))

(define-rfc integer? integer?)
(define-rfc positive? integer?)
(define-rfc negative? integer?)

;; =============================================================================

(module+ test
  (require rackunit)
)
