#lang racket/base

;; For lifting predicates to contracts that Rosette
;;  can compile to solver constraints.
;;
;; A "Rosette flat contract" contract takes the form:
;;
;;   { x \in D | P(x) }
;;
;; where:
;; - D is a predicate for a **solvable type** (see Rosette Guide)
;;     this is the "domain" for the contract
;; - P is any predicate
;;
;; To compile a Rosette contract to "SMT",
;;  declare a symbolic variable `x` and assert `P(x)`.
;;
;;    (define-symbolic x D)
;;    (assert (P x))

(provide
  make-flat-rosette-contract
  ;; (-> (-> Any Boolean) #:first-order (-> Any Boolean) (-> Any Any))
  ;; more precisely,
  ;; (-> Predicate #:first-order Solvable Contract)
  ;; where
  ;; - Predicate is any function, but preferably from Rosette
  ;; - Solvable is a predicate for a Rosette-`solvable?` type
  ;; - Contract is a flat Racket contract
  ;; Basically, you give a predicate and a type, and you get a contract that
  ;;  can use Rosette to talk about the values it accepts.
)

(require
  (prefix-in prop.
    (only-in racket/contract/private/prop contract-struct-first-order))
  (prefix-in R.
    rosette)
  racket/contract
  rosette-contract/private/log
)

;; =============================================================================

;; TODO make macro version?

(define (make-flat-rosette-contract P #:first-order D)
  (define P-name (object-name P))
  (make-flat-contract
    #:name
      `(flat-rosette-contract ,P-name)
    #:first-order
      D
    #:late-neg-projection
      (λ (blame)
        (λ (val neg-party)
          (if (P val)
            val
            (raise-blame-error blame #:missing-party neg-party `(expected ,P-name)))))
    #:stronger
      (λ (other-ctc)
        (log-rosette-contract-info "stronger? ~a ~a" P-name other-ctc)
        (and (flat-contract? other-ctc)
             (let ([other-D (prop.contract-struct-first-order other-ctc)]
                   [other-P (λ (x) ;; TODO better to just get the predicate out of the contract
                              (with-handlers ([exn:fail:contract:blame? (λ (exn) #f)])
                                (other-ctc x)))])
               (and (R.solvable? other-D)
                    (eq? D other-D)
                    (rosette-stronger? P other-P #:solve-for D)))))))

(define (rosette-stronger? P1 P2 #:domain D)
  (R.define-symbolic* x D)
  (R.unsat?
    (R.solve (R.assert (P1 x))
             (R.assert (R.not (P2 x))))))

;; =============================================================================

(module+ test
  (require rackunit)

  (test-case "rosette-stronger?"
    (check-true (rosette-stronger? R.positive? R.integer? #:domain R.integer?))
    (check-false (rosette-stronger? R.positive? R.negative? #:domain R.integer?))
    (check-false (rosette-stronger? R.integer? R.positive? #:domain R.integer?)))

)

