#lang racket/base

;; Flat, solvable contracts

(provide
  (rename-out
    [-integer? integer?]
    [-negative? negative?]
    [-positive? positive?]
    [-even? even?]
    [-odd? odd?]
    [-real? real?]
    [-exact-nonnegative-integer? exact-nonnegative-integer?]
    [-exact-nonnegative-integer? natural-number/c]
    [-<=/c <=/c]
    [->=/c >=/c]
    [-</c </c]
    [->/c >/c]
    ;; --
    [-boolean? boolean?]
))

(require
  rosette-contract/private/flat
  (prefix-in R. rosette)
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
  (make-solvable-predicate R.integer? #:domain R.integer?))

(define -real?
  (make-solvable-predicate R.real? #:domain R.real?))

(define -positive?
  (make-solvable-predicate R.positive? #:domain R.integer?))

(define -negative?
  (make-solvable-predicate R.negative? #:domain R.integer?))

(define -even?
  (make-solvable-predicate R.even? #:domain R.integer?))

(define -odd?
  (make-solvable-predicate R.odd? #:domain R.integer?))

(define -exact-nonnegative-integer?
  (make-solvable-predicate R.exact-nonnegative-integer? #:domain R.integer?))

(define (-</c n)
  (make-solvable-predicate (λ (x) (< x n)) #:domain R.real?))

(define (->/c n)
  (make-solvable-predicate (λ (x) (> x n)) #:domain R.real?))

(define (-<=/c n)
  (make-solvable-predicate (λ (x) (<= x n)) #:domain R.real?))

(define (->=/c n)
  (make-solvable-predicate (λ (x) (>= x n)) #:domain R.real?))

(define -boolean?
  (make-solvable-predicate R.boolean? #:domain R.boolean?))

;; =============================================================================

(module+ test
  (require rackunit)
)
