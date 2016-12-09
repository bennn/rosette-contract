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
  ;; (-> any/c boolean?)
  ;; Return #true if argument is a solvable predicate contract

  solvable-predicate-D
  solvable-predicate-P
  ;; Struct accessors

  solvable-predicate-stronger
  ;; (-> solvable-predicate? solvable-predicate? boolean?)
  ;; Use Rosette to check is the first predicate is stronger than the second.
  ;; Stronger = accepts fewer values than.

  solvable-predicate-simplify
  ;; (-> any/c solvable-predicate? contract?)
  ;; Simplify the given contract with respect to the given value.
  ;; For now this means:
  ;; - if the value obviously meets the contract, remove the contract
  ;; - if the value obviously breaks the contract, raise an exception

  the-trivial-predicate
  ;; solvable-predicate?
  ;; Represents a no-op predicate

  the-trivial-predicate?
  ;; (-> solvable-predicate? boolean?)
  ;; Returns #true if the given predicate is trivial in the current context
)

(require
  (prefix-in R.
    rosette)
  racket/contract
  rosette-contract/private/base
  rosette-contract/private/util/log
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
      (if (or (the-trivial-predicate? ctc) (P val))
        val
        (raise-blame-error blame val #:missing-party neg-party '(expected "~a") P-name)))))

(define (solvable-predicate-stronger this-ctc that-ctc)
  (define this-D (solvable-predicate-D this-ctc))
  (define this-P (solvable-predicate-P this-ctc))
  (define this-name (solvable-predicate-name this-ctc))
  (log-rosette-contract-info "stronger? ~a ~a" this-name that-ctc)
  (and (solvable-predicate? that-ctc)
       (or (the-trivial-predicate? that-ctc)
           (let ([that-D (solvable-predicate-D that-ctc)]
                 [that-P (solvable-predicate-P that-ctc)])
             (and (eq? this-D that-D)
                  (rosette-stronger? this-P that-P #:domain this-D))))))

(define (solvable-predicate-simplify v ctc srcloc)
  (define P (solvable-predicate-P ctc))
  (define D (solvable-predicate-D ctc))
  (cond
   [(the-trivial-predicate? ctc)
    ctc]
   [(rosette-trivial-predicate? v P #:domain D)
    (log-success ctc srcloc "trivial predicate")
    the-trivial-predicate]
   [(rosette-impossible-predicate? v P #:domain D)
    (error 'solvable-predicate "value ~a cannot satisfy the contract ~a" v ctc)]
   [else
    (log-failure ctc srcloc)
    ctc]))

(struct solvable-predicate solvable-contract (
  D ;; domain
  P ;; projection
)
#:transparent
#:methods gen:custom-write
[(define (write-proc v port mode)
   ((if mode write display) (solvable-predicate-name v) port))]
#:property prop:flat-contract
  (build-flat-contract-property
   #:name solvable-predicate-name
   #:first-order (λ (ctc) (solvable-predicate-P ctc))
   #:late-neg-projection solvable-predicate-late-neg
   #:stronger solvable-predicate-stronger)
)

(define the-trivial-predicate (solvable-predicate #f #f))

(define (the-trivial-predicate? v)
  (eq? v the-trivial-predicate))
;; TODO
;; - can we make this more "Rosette official"?
;; - optimize for space? just use a symbol?

;; -----------------------------------------------------------------------------
;; TODO simplify using `no-counterexamples` function

(define (rosette-stronger? P1 P2 #:domain D)
  (R.define-symbolic* x D)
  (R.unsat?
    (R.solve (R.assert (P1 x))
             (R.assert (R.not (P2 x))))))

;; Can we find any counterexample to the predicate?
;; TODO what's a solver to do here?
(define (rosette-trivial-predicate? v P #:domain D)
  (R.unsat?
    (R.solve (R.assert (R.not (P v))))))

;; Can we find any value that meets the predicate?
(define (rosette-impossible-predicate? v P #:domain D)
  (R.unsat?
    (R.solve (R.assert (P v)))))

;; =============================================================================

(module+ test
  (require
    (only-in rosette/lib/lift define-lift)
    racket/string
    rackunit)

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
    (check-equal? (contract sp1 4 'pos 'neg) 4)
    (check-exn exn:fail:contract:blame?
      (λ () (contract sp1 3 'pos 'neg)))

    (check-equal? (contract sp2 9 'pos 'neg) 9)
    (check-exn exn:fail:contract:blame?
      (λ () (contract sp2 -4 'pos 'neg)))
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

  (test-case "solvable-predicate-simplify+"
    (define srcloc (make-srcloc 'flat.rkt 4 1 2 3))

    (check-equal?
      (solvable-predicate-simplify 'A the-trivial-predicate srcloc)
      the-trivial-predicate)

    (let ([trivial-predicate-box
           (force/rc-log
             (λ ()
               (check-equal? (solvable-predicate-simplify 4 sp1 srcloc) the-trivial-predicate)))])
      (define info-msgs (hash-ref trivial-predicate-box 'info))
      (check-equal? (length info-msgs) 1)
      (check-true (null? (hash-ref trivial-predicate-box 'error)))
      (check-true (null? (hash-ref trivial-predicate-box 'warning)))
      (check-true (null? (hash-ref trivial-predicate-box 'debug)))
      (check-true (null? (hash-ref trivial-predicate-box 'fatal)))
      (define info-msg (car info-msgs))
      (check-true (string-contains? info-msg "trivial predicate")))

    (check-exn #rx"cannot satisfy the contract"
      (λ () (solvable-predicate-simplify 0 sp2 srcloc)))

    ;; TODO test for failure? I don't know any 'dont know' cases for these
  )

  (test-case "rosette-trivial-predicate"
    (check-false (rosette-trivial-predicate? -2 R.positive? #:domain R.integer?))
    (check-false (rosette-trivial-predicate? "hello" R.integer? #:domain R.integer?))

    (check-true (rosette-trivial-predicate? 8 R.positive? #:domain R.integer?))
    (check-true (rosette-trivial-predicate? -4 R.integer? #:domain R.integer?))
  )

  (test-case "rosette-impossible-predicate?"
    (check-true (rosette-impossible-predicate? -2 R.positive? #:domain R.integer?))
    (check-true (rosette-impossible-predicate? "hello" R.integer? #:domain R.integer?))

    (check-false (rosette-impossible-predicate? 8 R.positive? #:domain R.integer?))
    (check-false (rosette-impossible-predicate? -4 R.integer? #:domain R.integer?))
  )

  (test-case "end-to-end"
    (check-equal? (sp1 3) #f)
    (check-equal? (sp1 4) #t)

    (check-equal? (sp2 -3) #f)
    (check-equal? (sp2 3) #t)
  )
)

