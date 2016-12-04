#lang racket/base

(require
  rosette-contract
  (only-in rosette *)
  #;racket/contract)

(define/contract (f x)
  (-> positive? negative?)

  (* x -1))

(module+ test
  (require rackunit)

  (check-equal? (f 1) -1)
  (check-equal? (f 2) -2)
  (check-equal? (f 3) -3)

  (check-exn exn:fail:contract?
    (λ () (f -1)))

)
