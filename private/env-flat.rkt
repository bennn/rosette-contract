#lang racket/base

;; Flat, solvable contracts

(provide
  (rename-out
    [-integer? integer?]
    [-negative? negative?]
    [-positive? positive?]
    [-exact-nonnegative-integer? exact-nonnegative-integer?]
    [-exact-nonnegative-integer? natural-number/c]

    [-boolean? boolean?]
))

(require
  rosette-contract/private/flat
  (only-in rosette
    positive? negative? integer? exact-nonnegative-integer?)
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

(define -exact-nonnegative-integer?
  (make-solvable-predicate exact-nonnegative-integer? #:domain integer?))

(define -boolean?
  (make-solvable-predicate boolean? #:domain boolean?))

;; =============================================================================

(module+ test
  (require rackunit)
)
