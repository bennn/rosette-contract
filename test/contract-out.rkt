#lang racket/base

(module u racket/base
  (require
    rosette-contract
    (only-in rosette *))

  (define (f x)
    (* x -1))

  (provide (contract-out [f (-> positive? negative?)]))
)

(require 'u)

(module+ test
  (require rackunit (only-in racket/contract exn:fail:contract:blame?))

  (check-equal? (f 1) -1)
  (check-equal? (f 2) -2)
  (check-equal? (f (expt 2 9)) (- (expt 2 9)))

  (check-exn exn:fail:contract:blame?
    (λ () (f -1)))

)
