#lang racket/base

;; TODO uses values

;; The Computer Language Shootout
;; http://shootout.alioth.debian.org/
;; Based on the MLton version of the benchmark
;; contributed by Scott Cruzen

(require racket/cmdline
         rosette-contract
         #;racket/contract)

(define/contract (floor_ev q r s t x)
  (-> integer? integer? integer? integer? integer? integer?)
  (quotient (+ (* q x) r) (+ (* s x) t)))

(define/contract (comp q r s t  q2 r2 s2 t2)
  (-> integer? integer? integer? integer? integer? integer? integer? integer? (values integer? integer? integer? integer?))
  (values (+ (* q q2) (* r s2))
          (+ (* q r2) (* r t2))
          (+ (* s q2) (* t s2))
          (+ (* s r2) (* t t2))))

(define/contract (next q r s t) 
    (-> integer? integer? integer? integer? integer?)
    (floor_ev q r s t 3))

(define/contract (safe? q r s t n) 
    (-> integer? integer? integer? integer? integer? boolean?)
    (= n (floor_ev q r s t 4)))

(define/contract (prod q r s t n)
    (-> integer? integer? integer? integer? integer? (values integer? integer? integer? integer?))
    (comp 10 (* -10 n) 0 1  q r s t))

(define/contract (mk q r s t k)
    (-> integer? integer? integer? integer? integer? (values integer? integer? integer? integer?))
    (comp q r s t k (* 2 (add1 (* 2 k))) 0 (add1 (* 2 k))))

(define/contract (digit k  q r s t  n row col)
  (-> integer? integer? integer? integer? integer? integer? integer? integer? string?)
  (if (> n 0)
      (let ([y (next q r s t)])
        (if (safe? q r s t y)
            (let-values ([(q r s t) (prod q r s t y)])
              (if (= col 10)
                  (let ([row (+ row 10)])
                    (format "\t:~a\n~a" row y)
                    (digit k q r s t (sub1 n) row 1))
                  (begin
                    (format "~a" y)
                    (digit k q r s t (sub1 n) row (add1 col)))))
            (let-values ([(q r s t) (mk q r s t k)])
              (digit (add1 k) q r s t n row col))))
      (format "~a\t:~a\n"
              (make-string (- 10 col) #\space)
              (+ row col))))

(define/contract (digits n)
  (-> natural-number/c string?)
  (digit 1  1 0 0 1  n 0 0))

(time (begin (digits 4000) (void)))
