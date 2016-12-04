#lang racket/base

;; Macros for attaching contracts to values.

(provide
  define/contract
  ;; (define/contract (f ...) (-> ...) e ...)
  ;; Attaches contract (-> ...) to the function named `f`,
  ;;  but first checks whether parts of the contract are unnecessary.

  contract-out
  ;; TODO doc
)

(require
  rosette-contract/private/flat
  rosette-contract/private/log
  rosette-contract/private/simplify
  (prefix-in C. racket/contract)
  (for-syntax (prefix-in C. (only-in racket/contract contract-out)))
  (prefix-in R. rosette)
  (for-syntax
    racket/base
    racket/provide-transform
  racket/pretty
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

  (define-syntax-class contract-out-spec
    #:attributes (tmp name ctc)
    (pattern [name:id ctc]
      #:attr tmp #'yo #;(quasisyntax/loc stx #,(gensym (syntax-e #'name))))
    ;(pattern [(~literal rename) name:id tmp:id ctc])
  )
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

(define-syntax contract-out
  (make-provide-pre-transformer
    (λ (stx modes)
      (syntax-parse stx
       [(_ e*:contract-out-spec ...)
        ;; TODO generalize to ctc+*, because `struct ....` introduces many contracts
        (syntax-local-lift-module-end-declaration
          (quasisyntax/loc stx
            (begin
              (define/contract e*.tmp e*.ctc e*.name)
              ...)))
        (syntax/loc stx
          (provide-contract-out-vars (rename-out [e*.tmp e*.name] ...) ))]))))

(define-syntax provide-contract-out-vars
  (make-provide-transformer
    (λ (stx modes)
      (syntax-parse stx
       [(_ e)
        (expand-export (syntax/loc stx e) modes)]))))

;; =============================================================================

(module+ test
  (require
    rackunit
    racket/string
    (prefix-in RC. rosette-contract/private/arrow)
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
            (RC.-> RC.integer? RC.integer?)
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
