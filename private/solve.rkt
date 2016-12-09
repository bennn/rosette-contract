#lang racket/base

;; Front-end for simplifying contracts with Rosette

(provide
  contract-simplify
)

(require
  rosette-contract/private/arrow
  rosette-contract/private/flat
  rosette-contract/private/util/log
)

;; =============================================================================

(define (contract-simplify v ctc srcloc)
  (cond
   [(solvable-predicate? ctc)
    (solvable-predicate-simplify v ctc srcloc)]
   [(solvable-->? ctc)
    (solvable-->-simplify v ctc srcloc)]
   [else
    ctc]))

;; =============================================================================

(module+ test
  (require
    (prefix-in C. racket/contract)
    (prefix-in F. rosette-contract/private/env/flat)
    rackunit)

  (define test-srcloc '(simplify:test 33 34 35 36))

  (test-case "simplfy:basic"
    (let ([ctc C.none/c])
      (check-equal? (contract-simplify 4 ctc test-srcloc) ctc))
    (check-true (the-trivial-predicate? (contract-simplify 4 F.integer? test-srcloc)))
    (let ([p->p (-> F.positive? F.positive?)])
      (check-true (solved-->? (contract-simplify (lambda (x) x) p->p test-srcloc))))
  )
)
