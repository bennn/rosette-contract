#lang racket/base

;; beware: racket/contract takes 2ms
;;         rosette-contract takes 78ms + solver time (~1min total)

(require rosette-contract)
;(require racket/contract)

(define/contract (fib n)
  (-> natural-number/c natural-number/c)
  (cond ((< n 2) 1)
    (else 0 (+ (fib (- n 2)) (fib (- n 1))))))

(define/contract (main args)
  (-> (vectorof string?) natural-number/c)
  (let ((n (if (= (vector-length args) 0)
            1
            (string->number (vector-ref args 0)))))
   (fib n)))

(time (begin (main (vector "23")) (void)))
