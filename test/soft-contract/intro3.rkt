#lang racket/base
(require rosette-contract)

;; From Kobayashi et. al 2011
;; http://www.kb.ecei.tohoku.ac.jp/~koba/papers/pldi11.pdf

;; TODO ->i and/c

(define (f x g)
  (g (+ x 1)))

(define/contract (h x)
  ;; Implementation doesn't matter
  (->i ([x integer?])
       [r (x) (->i ([y (and/c integer? (>/c x))])
                   [r (y) (and/c integer? (>/c y))])])
  add1)

(define/contract (main n)
  (-> integer? (and/c integer? (>/c 0)))
  (if (>= n 0)
    (f n (h n))
    1))

(module+ test
  (require rackunit)

  (check-equal? (main -3) 1)
  (check-equal? (main -9) 1)

  (check-equal? (main 0) 2)
  (check-equal? (main 3) 5)

)
