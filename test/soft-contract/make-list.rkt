#lang racket/base
(require rosette-contract)

;; Convergence for non-tail-recursion

;; From Kobayashi et. al 2011
;; http://www.kb.ecei.tohoku.ac.jp/~koba/papers/pldi11.pdf

(define/contract (main n)
  (-> (and/c integer? (>=/c 0)) (and/c integer? (>=/c 0)))
  (let ([l (make-list n)])
    (if (> n 0) (car (reverse l empty)) 0)))

(define (reverse l ac)
  (if (empty? l)
    ac
    (reverse (cdr l) (cons (car l) ac))))

(define (make-list n)
  (if (= n 0)
    empty
    (cons n (make-list (- n 1)))))

;; -----------------------------------------------------------------------------

(module+ test
  (require rackunit)

  (check-exn exn:fail:contract?
    (λ () (main -2)))

  (check-equal? (main 0) 0)
  (check-equal? (main 1) 1)
  (check-equal? (main 3) 1)

)
