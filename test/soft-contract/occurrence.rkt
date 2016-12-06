#lang racket/base
(require rosette-contract)

;; TODO need to handle or/c

(define/contract (f x p)
  (-> (or/c integer? boolean?) pair? integer?)
  (cond
   [(and (integer? x) (integer? (car p)))
    (+ x (car p))]
   [(integer? (car p))
    (+ (if x 1 0) (car p))]
   [else
    0]))

(module+ test
  (require rackunit)

  (check-equal? (f 2 (cons 9 9)) 11)
  (check-equal? (f #t (cons 1 0)) 2)
  (check-equal? (f #f (cons 1 0)) 1)
  (check-equal? (f 8 (cons #f #f)) 0)

  (check-exn exn:fail:contract?
    (λ () (f "hello" (cons 1 1))))

)
