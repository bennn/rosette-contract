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
   [(_ name:id P:id D:id)
    #:with tmp-name (gensym (syntax-e #'name))
    #'(begin (provide (rename-out [tmp-name name]))
             (define tmp-name (make-solvable-predicate P #:domain D)))]))

(define-rfc integer? integer? integer?)
(define-rfc positive? positive? integer?)
(define-rfc negative? negative? integer?)

;; =============================================================================

(module+ test
  (require rackunit)
)
