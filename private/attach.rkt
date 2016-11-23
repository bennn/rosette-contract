#lang racket/base

;; Macros for attaching contracts to values.

(provide
  define/contract
  ;; (define/contract (f ...) (-> ...) e ...)
  ;; Attaches contract (-> ...) to the function named `f`,
  ;;  but first checks whether parts of the contract are unnecessary.

  ->

;  contract-out
  ;; 
)

(require
  rosette-contract/private/chaperone
  rosette-contract/private/flat
  rosette-contract/private/log
  (prefix-in C. racket/contract)
  (prefix-in R. rosette)
  (for-syntax
    racket/base
    syntax/parse
    syntax/srcloc)
)

;; =============================================================================

(begin-for-syntax
  (define-syntax-class id-spec
    #:attributes (name)
    (pattern x:id
     #:attr name #'x)
    (pattern (x:id . e*)
     #:attr name #'x))
)

(define-syntax (define/contract stx)
  (syntax-parse stx
   [(_ dom:id-spec ctc-spec . e*)
    (quasisyntax/loc stx
      (define dom.name
        (let ([ctc ctc-spec])
          (define dom . e*)
          (let ([ctc+ (contract-simplify dom.name ctc (list #,@(build-source-location-list stx)))])
            (cond
             [(the-trivial-predicate? ctc+)
              dom.name]
             [else
              (C.with-contract #:region definition dom.name
                ([new-name ctc+])
                (define new-name dom.name))
              new-name])))))]))

(define-syntax (-> stx)
  (syntax-parse stx
   [(_ dom cod)
    (syntax/loc stx
      (make-solvable--> dom cod))]
   [(_ . e*)
    (syntax/loc stx
      (C.-> . e*))]))

;; -----------------------------------------------------------------------------

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
    rackunit
    racket/string
    (prefix-in RC. rosette-contract/private/env-flat))

  (test-case "C.define/contract-internals"
    ;; Internal uses of the contract-id don't need to follow the contract

    (C.define/contract (f x)
      (C.-> integer? integer?)
      (if (string? x) 0 (+ 1 (f "hi"))))

    (check-equal? (f 1) 1)
    (check-exn C.exn:fail:contract:blame?
      (λ () (f "hi"))))

  (test-case "define/contract-internals"
    ;; ditto for our define/contract
    (define inbox
      (force/rc-log
        (λ ()
          (define/contract (f x)
            (make-solvable--> RC.integer? RC.integer?)
            (if (string? x) 0 (+ 1 (f "hi"))))

          (check-equal? (f 1) 1)
          (check-exn C.exn:fail:contract:blame?
            (λ () (f "hi"))))))

    (check-true (null? (hash-ref inbox 'error)))

    (define infos (hash-ref inbox 'info))
    (check-equal? (length infos) 1)

    (check-true (string-contains? (car infos) "SUCCESS"))
  )
)
