#lang racket/base

;; Macros for attaching contracts to values.

(provide
  define/contract
  ;; (define/contract (f ...) (-> ...) e ...)
  ;; Attaches contract (-> ...) to the function named `f`,
  ;;  but first checks whether parts of the contract are unnecessary.

  contract-out
  ;; (provide (contract-out [id ctc] ...))
  ;; Attach contract `ctc` to the identifier `id`,
  ;;  only allowed within a `provide` form
)

(require
  rosette-contract/private/flat
  rosette-contract/private/solve
  rosette-contract/private/util/log
  (prefix-in C. racket/contract)
  (for-syntax (prefix-in C. (only-in racket/contract contract-out)))
  (prefix-in R. rosette)
  (for-syntax
    racket/base
    racket/provide-transform
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
      #:attr tmp (quasisyntax/loc stx #,(gensym (syntax-e #'name))))
    ;(pattern [(~literal rename) name:id tmp:id ctc])
  )
)

(define-syntax (define/contract stx)
  (syntax-parse stx
   [(_ dom:id-spec ctc-spec . e*)
    (quasisyntax/loc stx
      (define dom.name
        (let ([ctc ctc-spec])
          (log-rosette-contract-info "ATTACH define/contract ~a" '#,(syntax->datum #'ctc-spec))
          (define dom . e*)
          (let ([ctc+ (contract-simplify dom.name ctc (list #,@(build-source-location-list stx)))])
            (cond
             [(the-trivial-predicate? ctc+)
              dom.name]
             [else
              (C.contract ctc+ dom.name '(definition dom.name) 'context)])))))]))

(define-syntax contract-out
  (make-provide-pre-transformer
    (λ (stx modes)
      (syntax-parse stx
       [(_ e*:contract-out-spec ...)
        ;; TODO generalize to ctc+*, because `struct ....` introduces many contracts
        (syntax-local-lift-module-end-declaration
          (quasisyntax/loc stx
            (begin
              ;; TODO gives the correct blame labels?
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
    (prefix-in RC. rosette-contract/private/env/flat))

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
    (check-equal? (length infos) 2)

    (check-true (string-contains? (cadr infos) "SUCCESS"))
  )

  (test-case "define/contract-2arg"
    (let ([inbox
      (force/rc-log
        (λ ()
          (define/contract (ide n)
            (RC.-> RC.exact-nonnegative-integer? RC.exact-nonnegative-integer?)
            n)
          (check-equal? (ide 4) 4)
          (check-equal? (ide 0) 0)
          (check-exn C.exn:fail:contract:blame?
            (λ () (ide -5)))))])
      (check-true (null? (hash-ref inbox 'error)))
      (define infos (hash-ref inbox 'info))
      (check-equal? (length infos) 2)
      (check-true (string-contains? (cadr infos) "SUCCESS")))

    (let ([inbox
      (force/rc-log
        (λ ()
          (define/contract (a m n)
            (RC.-> RC.exact-nonnegative-integer? RC.exact-nonnegative-integer? RC.exact-nonnegative-integer?)
            (R.cond
             ((R.zero? m) (R.+ n 1))
             (#t (a 0 1))))
          (check-equal? (a 0 2) 3)
          (check-equal? (a 4 3) 2)
          (check-exn C.exn:fail:contract:blame?
            (λ () (a -5 -6)))))])
      (check-true (null? (hash-ref inbox 'error)))
      (define infos (hash-ref inbox 'info))
      (check-equal? (length infos) 2)
      (check-true (string-contains? (cadr infos) "SUCCESS")))

    #;(let ([inbox
      (force/rc-log
        (λ ()
          ;; LOL verification is taking too long
          (define/contract (ack m n)
            (RC.-> RC.exact-nonnegative-integer? RC.exact-nonnegative-integer? RC.exact-nonnegative-integer?)
            (R.cond
             ((R.zero? m) (R.+ n 1))
             ((R.zero? n) (ack (R.- m 1) 1))
             (else      (ack (R.- m 1) (ack m (R.- n 1))))))
          #;(check-equal? (ack 0 2) 3)
          #;(check-equal? (ack 4 3) 5)
          (check-exn C.exn:fail:contract:blame?
            (λ () (ack -5 -6)))))])
      (check-true (null? (hash-ref inbox 'error)))
      (define infos (hash-ref inbox 'info))
      (check-equal? (length infos) 1)
      (check-true (string-contains? (car infos) "SUCCESS")))
  )

)
