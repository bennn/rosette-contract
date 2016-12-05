#lang racket/base
(require rosette-contract)

(provide
  (contract-out
    (min (-> real? real? real?))
    (max (-> real? real? real?))
    (abs (-> real? real?))
    (sqr (-> real? real?))
    (msqrt (-> real? real?))
))

;; =============================================================================

(define-syntax-rule (assert e p) (let ([v e]) (unless (p v) (error 'assert)) v))

#;(: min (-> Real Real Real))
(define (min x y) (if (<= x y) x y))
#;(: max (-> Real Real Real))
(define (max x y) (if (>= x y) x y))
#;(: abs (-> Real Real))
(define (abs x) (if (>= x 0) x (- 0 x)))
#;(: sqr (-> Real Real))
(define (sqr x) (* x x))
#;(: msqrt (-> Real Real))
(define (msqrt x) (assert (sqrt x) real?))
