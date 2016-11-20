#lang racket/base

;; Demo showing the main idea behind `rosette-contract`.
;; - before attaching a function contract,
;; - run the solver, see if the precondition implies the function meets the postcontdition
;; - if so, attach a simplified contract
;;
;; Though in this file, the contract is not very simplified.
;; Instead of an `->` there's an `->i` to check that the argument is within
;;  the bitwidth Rosette used to prove the postcondition.

;; =============================================================================

(module rc racket/base
  (provide
    (rename-out [C.-> ->])
    define/contract)

  (require
    (for-syntax racket/base syntax/parse)
    (prefix-in C. racket/contract)
    (prefix-in R. rosette))

  (define-syntax (define/contract stx)
    (syntax-parse stx #:literals (C.->)
     [(_ (f:id var*:id ...+) (C.-> pre* ... post) body* ...)
      (quasisyntax/loc stx
        (define f
          ;; Uncontracted version
          (letrec ([f (λ (var* ...) body* ...)])
            (let ([ctc (triple->contract {[var* pre*] ...} f {post})])
              ;; 2016-08-15: should blame `f` in negative position
              (C.contract ctc f (C.current-contract-region) (C.current-contract-region))))))]
     [(_ arg* ...)
      (syntax/loc stx
        (C.define/contract arg* ...))]))

  (define-syntax (triple->contract stx)
    (syntax-parse stx
     [(_ {[var* pre*] ...} f {post})
      #:with r (gensym 'result)
      (quasisyntax/loc stx
        (if (verify-codomain {[var* pre*] ...} f {post}) ;; TODO use a function
          (let ([in-bitwidth/c (make-bitwidth/c)])
            (C.->i ([var* pre*]
                    ...)
                   [r (var* ...) (C.or/c (in-bitwidth/c var* ...)
                                         post)]))
          (C.-> pre* ... post)))]))

  ;; Use to see if dynamic inputs to a function fit within the current Rosette bitwidth.
  ;; Returns a "curried" function that:
  ;; 1. takes a sequence of numbers (the dynamic inputs)
  ;; 2. returns a function accepting any argument (the function's output)
  ;; 3. returns #t if the sequence of numbers are all "small enough"
  ;; (-> (-> Real * (-> Any Boolean)))
  (define (make-bitwidth/c)
    (define cb (R.current-bitwidth))
    (define (make-contract pass?)
      (C.make-flat-contract
        #:name
          (format "rosette-bitwidth<~a/c" cb)
        #:first-order
          (lambda (x) pass?)
        #:late-neg-projection
          (lambda (blame) (lambda (val neg-party)
            (if pass? val (C.raise-blame-error blame #:missing-party neg-party "too large for current bitwidth"))))))
    (if cb
      (let ([max-int (expt 2 cb)])
        (λ int*
          (make-contract
            (for/and ([i (in-list int*)])
              (and (integer? i)
                   (< (abs i) max-int))))))
      (let ( #;[max-real ???] )
        (λ int*
          (make-contract #f)))))

  ;; Verification of: {P} f {Q}
  ;; - assert P holds
  ;; - assert Q does not hold
  ;; - if unsat, then no counterexamples
  ;;   safe to remove Q check because it always holds
  (define-syntax-rule (verify-codomain {[var* pre*] ...} f {post})
    (let ()
      (R.define-symbolic var* ... R.integer?)
      (define sol
        (R.solve (begin
          (R.assert pre* var*)
          ...
          (R.assert (not (post (f var* ...)))))))
      (R.unsat? sol)))
)

;; =============================================================================

(module example racket/base
  (require
    (submod ".." rc)
    rackunit
    racket/string
    (only-in racket/contract exn:fail:contract:blame?))

  (define/contract (f x)
    (-> positive? negative?)
    (* x -1))

  (check-equal? (f 1) -1)

  (check-exn (lambda (exn)
               (and (exn:fail:contract:blame? exn)
                    (string-contains? (exn-message exn) "(C.or/c (in-bitwidth/c x) negative?)")))
    (lambda () (f -3)))
)
(require 'example)
