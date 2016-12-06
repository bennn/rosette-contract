#lang racket/base
(require rosette-contract)

(struct pair (hd tl))
(struct nil ())

(define (list? x)
  (or (nil? x)
      (and (pair? x) (list? (pair-tl x)))))

(define/contract (null x)
  (-> list? boolean?)
  (nil? x))

(define (non-null? x)
  (not (null x)))

(define/contract (head x)
  (-> (and/c list? non-null?) any/c)
  (pair-hd x))

;; -----------------------------------------------------------------------------
;; Set library

(define set? list?)

(define/contract (empty x)
  (-> set? boolean?)
  (null x))

(define/contract (min x)
  (-> (and/c set? non-null?) any/c)
  (head x))

;; -----------------------------------------------------------------------------

(define (make-list n)
  (let loop ([acc (nil)] [n n])
    (if (< n 0)
      acc
      (loop (pair n acc) (- n 1)))))

(define (main)
  (define xs (make-list 10))
  (for ([i (in-range 20)])
    (min xs)
    (empty xs)
    (void))
  (void))

(time (main))
