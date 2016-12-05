#lang racket/base
(require rosette-contract)

(define/contract (ack m n)
  (-> exact-nonnegative-integer? exact-nonnegative-integer? exact-nonnegative-integer?)
  (cond
   ((zero? m) (+ n 1))
   ((zero? n) (ack (- m 1) 1))
   (else      (ack 0 #;(- m 1) (ack 0 #;m (- n 1))))))

(define (main)
  (ack 3 3))

(time (begin (main) (void)))
