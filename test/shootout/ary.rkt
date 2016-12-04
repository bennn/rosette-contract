#lang racket/base

;; TODO uses vectors, can't solve anything

(require rosette-contract)
;(require racket/contract)

(define/contract (main args)
  (-> (vectorof string?) void)
  (let* ((n (if (= (vector-length args) 0)
              1
              (string->number (vector-ref args 0))))
         (x (make-vector n 0))
         (y (make-vector n 0))
         (last (- n 1)))
    (define/contract (do1 i) (-> exact-nonnegative-integer? void?)
      (vector-set! x i (+ i 1)))
    (do ((i 0 (+ i 1)))
      ((= i n))
      (do1 i))
    (define/contract (do3 i) (-> exact-nonnegative-integer? void?)
      (vector-set! y i (+ (vector-ref x i) (vector-ref y i))))
    (do ((k 0 (+ k 1)))
      ((= k 1000))
      (do ((i last (- i 1)))
        ((< i 0))
        (do3 i)))
    (print-list (vector-ref y 0) " " (vector-ref y last))))

(define/contract (print-list . items) 
  (->* () #:rest (listof (or/c string? number?)) void)
  (void))

(time (begin (main (vector "600")) (void)))
;(time (begin (main (vector "600000")) (void)))
