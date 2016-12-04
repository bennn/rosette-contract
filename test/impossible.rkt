#lang racket/base
(module+ test
  (require rackunit rosette-contract)

  (define-syntax-rule (check-impossible e* ...)
    (check-exn #rx"cannot satisfy the contract"
      (λ () e* ... (void))))

  (check-impossible
    (define/contract (f x)
      (-> negative? negative?)
      (* x -1)))

  (check-impossible
    (define/contract (f x)
      (-> negative? positive?)
      (* x -1))
    ;; `g` always breaks its contract because `f` always returns positive numbers
    (define/contract (g x)
      (-> integer? negative?)
      (if (positive? x)
        (f -3)
        (f (- x 1)))))

)
