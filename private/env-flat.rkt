#lang racket/base

;; Flat, solvable contracts

(provide
  (rename-out
    [-negative? negative?]
    [-integer? integer?]
    [-positive? positive?]
))

(require
  rosette-contract/private/flat
  (only-in rosette
    positive? negative? integer?)
  (for-syntax racket/base syntax/parse)
)

;; =============================================================================

#;
(define-syntax (define-rfc stx)
  (syntax-parse stx
   [(_ name:id P:id D:id)
    #:with tmp-name (gensym (syntax-e #'name))
    #'(begin (provide (rename-out [tmp-name name]))
             (define tmp-name (make-solvable-predicate P #:domain D)))]))

(define -integer?
  (make-solvable-predicate integer? #:domain integer?))

(define -positive?
  (make-solvable-predicate positive? #:domain integer?))

(define -negative?
  (make-solvable-predicate negative? #:domain integer?))

;; =============================================================================

(module+ test
  (require rackunit)
)
