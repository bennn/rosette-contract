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
  make-solvable-predicate
  ;; (-> (-> Any Boolean) #:domain (-> Any Boolean) (-> Any Any))
  ;; more precisely,
  ;; (-> Predicate #:domain Solvable Contract)
  ;; where
  ;; - Predicate is any function, but preferably from Rosette
  ;; - Solvable is a predicate for a Rosette-`solvable?` type
  ;; - Contract is a flat Racket contract
  ;; Basically, you give a predicate and a type, and you get a contract that
  ;;  can use Rosette to talk about the values it accepts.

  solvable-predicate?
  ;; TODO

  solvable-predicate-stronger

)

(require
  (prefix-in prop.
    (only-in racket/contract/private/prop contract-struct-first-order))
  (prefix-in R.
    rosette)
  racket/contract
  rosette-contract/private/base
  rosette-contract/private/log
)

;; =============================================================================

;; TODO make macro version?

(define (make-solvable-predicate P #:domain D)
  (solvable-predicate D P))

(define (solvable-predicate-name ctc)
  `(solvable-predicate ,(object-name (solvable-predicate-P ctc))))

(define (solvable-predicate-late-neg ctc)
  (define P (solvable-predicate-P ctc))
  (define P-name (solvable-predicate-name ctc))
  (λ (blame)
    (λ (val neg-party)
      (if (P val)
        val
        (raise-blame-error blame val #:missing-party neg-party '(expected "~a") P-name)))))

(define (solvable-predicate-stronger this-ctc that-ctc)
  (define this-D (solvable-predicate-D this-ctc))
  (define this-P (solvable-predicate-P this-ctc))
  (define this-name (solvable-predicate-name this-ctc))
  (log-rosette-contract-info "stronger? ~a ~a" this-name that-ctc)
  (and (solvable-predicate? that-ctc)
       (let ([that-D (solvable-predicate-D that-ctc)]
             [that-P (solvable-predicate-P that-ctc)])
         (and (eq? this-D that-D)
              (rosette-stronger? this-P that-P #:domain this-D)))))

(struct solvable-predicate solvable-contract (
  D
  P
)
#:transparent
#:property prop:flat-contract
  (build-flat-contract-property
   #:name solvable-predicate-name
   #:first-order (λ (ctc) (solvable-predicate-D ctc))
   #:late-neg-projection solvable-predicate-late-neg
   #:stronger solvable-predicate-stronger)
)

(define (rosette-stronger? P1 P2 #:domain D)
  (R.define-symbolic* x D)
  (R.unsat?
    (R.solve (R.assert (P1 x))
             (R.assert (R.not (P2 x))))))

;; =============================================================================

(module+ test
  (require
    (only-in racket/contract/private/blame make-blame)
    (only-in rosette/lib/lift define-lift)
    rackunit)

  (define dummy-blame (make-blame (srcloc #f #f #f #f #f) #f (λ () #f) #t #f '()))

  ;; TODO how to solve using custom functions?
  ;;      So far, only works for Rosette built-ins (see the papers?)
  (define =4
    (let ([=4-orig (λ (x) (= x 4))])
      (define-lift lift-=4 [(R.integer?) =4-orig])
      lift-=4))

  (define sp1 (make-solvable-predicate =4 #:domain R.integer?))
  (define sp2 (make-solvable-predicate R.positive? #:domain R.integer?))
  (define sp3 (make-solvable-predicate R.integer? #:domain R.integer?))

  (test-case "make-solvable-predicate"

    (check-true (solvable-predicate? sp1))

    (check-true ((solvable-predicate-D sp1) 1))
    (check-false ((solvable-predicate-D sp1) #f))

    (check-true ((solvable-predicate-P sp1) 4))
    (check-false ((solvable-predicate-P sp1) 5))
  )

  (test-case "solvable-predicate-name"
    (let ([nm1 (solvable-predicate-name sp1)])
      (check-equal? (car nm1) 'solvable-predicate))

    (let ([nm2 (solvable-predicate-name sp2)])
      (check-equal? nm2 '(solvable-predicate @positive?)))
  )

  (test-case "solvable-predicate-late-neg"
    (let ([ln1 ((solvable-predicate-late-neg sp1) dummy-blame)])
      (check-equal? (ln1 4 'missing) 4)
      (check-exn exn:fail:contract:blame?
        (λ () (ln1 3 'missing))))

    (let ([ln2 ((solvable-predicate-late-neg sp2) dummy-blame)])
      (check-equal? (ln2 9 'missing) 9)
      (check-exn exn:fail:contract:blame?
        (λ () (ln2 -4 'missing))))
  )

  (test-case "solvable-predicate-stronger"
    ;(check-true (solvable-predicate-stronger sp1 sp2))
    (check-true (solvable-predicate-stronger sp2 sp3))
    ;(check-false (solvable-predicate-stronger sp2 sp1))
    (check-false (solvable-predicate-stronger sp3 sp2))
  )

  (test-case "rosette-stronger?"
    (check-true (rosette-stronger? R.positive? R.integer? #:domain R.integer?))
    (check-false (rosette-stronger? R.positive? R.negative? #:domain R.integer?))
    (check-false (rosette-stronger? R.integer? R.positive? #:domain R.integer?))
    #;(check-false (rosette-stronger? R.positive? =4 #:domain R.integer?)))
)

