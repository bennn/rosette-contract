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
  #;(for-syntax racket/base syntax/parse)
)

;; =============================================================================

#;
(define-syntax (define-rfc stx)
  (syntax-parse stx
   [(_ name:id P:id D:id)
    #:with tmp-name (gensym (syntax-e #'name))
    #'(begin (provide (rename-out [tmp-name name]))
             (define tmp-name (make-rosette-flat-contract P #:type D)))]))

(define -integer?
  (make-rosette-flat-contract #:predicate R.integer? #:type R.integer?))

(define -real?
  (make-rosette-flat-contract #:predicate R.real? #:type R.real?))

(define -positive?
  (make-rosette-flat-contract #:predicate R.positive? #:type R.integer?))

(define -negative?
  (make-rosette-flat-contract #:predicate R.negative? #:type R.integer?))

(define -even?
  (make-rosette-flat-contract #:predicate R.even? #:type R.integer?))

(define -odd?
  (make-rosette-flat-contract #:predicate R.odd? #:type R.integer?))

(define -exact-nonnegative-integer?
  (make-rosette-flat-contract #:predicate R.exact-nonnegative-integer? #:type R.integer?))

(define (-</c n)
  (make-rosette-flat-contract #:predicate (λ (x) (< x n)) #:type R.real?))

(define (->/c n)
  (make-rosette-flat-contract #:predicate (λ (x) (> x n)) #:type R.real?))

(define (-<=/c n)
  (make-rosette-flat-contract #:predicate (λ (x) (<= x n)) #:type R.real?))

(define (->=/c n)
  (make-rosette-flat-contract #:predicate (λ (x) (>= x n)) #:type R.real?))

(define -boolean?
  (make-rosette-flat-contract #:predicate R.boolean? #:type R.boolean?))

;; =============================================================================

(module+ test
  (require rackunit)
)
