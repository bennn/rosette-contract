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

(provide
  make-rosette-flat-contract
  ;; (-> #:type solvable? #:predicate (-> any/c boolean?) rosette-flat-contract?)

  rosette-flat-contract?
  ;; (-> any/c boolean?)
  ;; Return #true if argument is a solvable predicate contract

  rosette-flat-contract-type
  rosette-flat-contract-predicate
)

(require
  (prefix-in R. rosette)
  racket/contract
  rosette-contract/private/base
  rosette-contract/private/solve
  rosette-contract/private/util/log
)

;; =============================================================================

(define (make-rosette-flat-contract #:type T #:predicate P)
  (rosette-flat-contract T P))

(define (rosette-flat-contract-name ctc)
  (format "#<rosette-flat-contract:~a>"
          (object-name (rosette-flat-contract-predicate ctc))))

(define (rosette-flat-contract-late-neg ctc)
  (define P (rosette-flat-contract-predicate ctc))
  (define P-name (rosette-flat-contract-name ctc))
  (λ (blame)
    (λ (val neg-party)
      (if (P val)
        val
        (raise-blame-error blame val #:missing-party neg-party '(expected "~a") P-name)))))

(define (rosette-flat-contract-stronger this-ctc that-ctc)
  (define this-name (contract-name this-ctc))
  (define that-name (contract-name that-ctc))
  (cond
   [(the-trivial-contract? that-ctc)
    #true]
   [(and (rosette-flat-contract? that-ctc)
         (eq? (rosette-flat-contract-predicate this-ctc)
              (rosette-flat-contract-predicate this-ctc)))
    (log-rosette-contract-info "stronger? ~a ~a"
                               (rosette-flat-contract-name this-ctc)
                               (rosette-flat-contract-name that-ctc))
    (no-counterexamples values
     #:forall this-ctc
     #:assume this-ctc
     #:derive that-ctc)]
   [else
    #false]))

(define (rosette-flat-contract-assert ctc)
  (rosette-flat-contract-predicate ctc))

(define (rosette-flat-contract-encode ctc)
  (R.define-symbolic* x (rosette-flat-contract-type ctc))
  x)

;; Simplify the given contract with respect to the given value.
;; For now this means:
;; - if the value obviously meets the contract, remove the contract
;; - if the value obviously breaks the contract, raise an exception
(define ((rosette-flat-contract-simplify ctc) v srcloc)
  ;; 2016-12-09 : why do anything? the contract isn't a chaperone, only payoff is to evaluate at compile-time
  ctc
  #;(cond
   [(rosette-trivial-predicate? ctc)
    (log-success ctc srcloc "trivial predicate")
    the-trivial-contract]
   [(rosette-impossible-predicate? ctc)
    (error 'rosette-flat-contract "value ~a cannot satisfy the contract ~a" v ctc)]
   [else
    (log-failure ctc srcloc)
    ctc]))

(struct rosette-flat-contract (
  type
  predicate
)
#:transparent
#:methods gen:custom-write
[(define (write-proc v port mode)
   (display (rosette-flat-contract-name v) port))]
#:property prop:rosette-contract
  (build-rosette-contract-property
   #:assert rosette-flat-contract-assert
   #:encode rosette-flat-contract-encode
   #:simplify rosette-flat-contract-simplify)
#:property prop:flat-contract
  (build-flat-contract-property
   #:name rosette-flat-contract-name
   #:first-order (λ (ctc) (rosette-flat-contract-predicate ctc))
   #:late-neg-projection rosette-flat-contract-late-neg
   #:stronger rosette-flat-contract-stronger)
)

;; -----------------------------------------------------------------------------

;; Can we find any counterexample to the predicate?
(define (rosette-trivial-predicate? ctc)
  (no-counterexamples values
   #:forall ctc
   #:assert the-trivial-contract
   #:derive ctc))

;; Can we find any value that meets the predicate?
(define (rosette-impossible-predicate? ctc)
  (no-examples values
   #:forall ctc
   #:assert the-trivial-contract
   #:derive ctc))

;; =============================================================================

(module+ test
  (require
    (only-in rosette/lib/lift define-lift)
    racket/string
    rackunit)

  (define =4
    (let ([=4-orig (λ (x) (R.= x 4))])
      (define-lift lift-=4 [R.integer? =4-orig])
      lift-=4))

  (define s-=4? (make-rosette-flat-contract #:predicate =4 #:type R.integer?))
  (define s-pos? (make-rosette-flat-contract #:predicate R.positive? #:type R.integer?))
  (define s-int? (make-rosette-flat-contract #:predicate R.integer? #:type R.integer?))

  (test-case "make-rosette-flat-contract"

    (check-true (rosette-flat-contract? s-=4?))

    (check-true ((rosette-flat-contract-type s-=4?) 1))
    (check-false ((rosette-flat-contract-type s-=4?) #f))

    (check-true ((rosette-flat-contract-predicate s-=4?) 4))
    (check-false ((rosette-flat-contract-predicate s-=4?) 5))
  )

  (test-case "rosette-flat-contract-name"
    (let ([nm1 (rosette-flat-contract-name s-=4?)])
      (check-true (string-prefix? nm1 "#<rosette-flat-contract")))

    (let ([nm2 (rosette-flat-contract-name s-pos?)])
      (check-true (string-prefix? nm2 "#<rosette-flat-contract")))
  )

  (test-case "rosette-flat-contract-late-neg"
    (check-equal? (contract s-=4? 4 'pos 'neg) 4)
    (check-exn exn:fail:contract:blame?
      (λ () (contract s-=4? 3 'pos 'neg)))

    (check-equal? (contract s-pos? 9 'pos 'neg) 9)
    (check-exn exn:fail:contract:blame?
      (λ () (contract s-pos? -4 'pos 'neg)))
  )

  (test-case "rosette-flat-contract-stronger"
    (check-true (rosette-flat-contract-stronger s-=4? s-pos?))
    (check-true (rosette-flat-contract-stronger s-pos? s-int?))
    (check-false (rosette-flat-contract-stronger s-pos? s-=4?))
    (check-false (rosette-flat-contract-stronger s-int? s-pos?)) ;; TODO
  )

  #;(test-case "rosette-flat-contract-simplify+"
    (define srcloc (make-srcloc 'flat.rkt 4 1 2 3))

    (let ([trivial-contract-box
           (force/rc-log
             (λ ()
               (check-equal? ((rosette-flat-contract-simplify s-=4?) 4 srcloc) the-trivial-contract)))])
      (define info-msgs (hash-ref trivial-contract-box 'info))
      (check-equal? (length info-msgs) 1)
      (check-true (null? (hash-ref trivial-contract-box 'error)))
      (check-true (null? (hash-ref trivial-contract-box 'warning)))
      (check-true (null? (hash-ref trivial-contract-box 'debug)))
      (check-true (null? (hash-ref trivial-contract-box 'fatal)))
      (define info-msg (car info-msgs))
      (check-true (string-contains? info-msg "trivial predicate")))

    (check-exn #rx"cannot satisfy the contract"
      (λ () ((rosette-flat-contract-simplify s-pos?) 0 srcloc)))

    ;; TODO test for failure? I don't know any 'dont know' cases for these
  )

  #;(test-case "rosette-trivial-predicate"
    (check-false (rosette-trivial-predicate? R.positive?))
    (check-false (rosette-trivial-predicate? R.integer?))

    (check-true (rosette-trivial-predicate? 8 R.positive? #:domain R.integer?))
    (check-true (rosette-trivial-predicate? -4 R.integer? #:domain R.integer?)))

  #;(test-case "rosette-impossible-predicate?"
    (check-true (rosette-impossible-predicate? -2 R.positive? #:domain R.integer?))
    (check-true (rosette-impossible-predicate? "hello" R.integer? #:domain R.integer?))

    (check-false (rosette-impossible-predicate? 8 R.positive? #:domain R.integer?))
    (check-false (rosette-impossible-predicate? -4 R.integer? #:domain R.integer?)))

  (test-case "end-to-end"
    (check-equal? (s-=4? 3) #f)
    (check-equal? (s-=4? 4) #t)

    (check-equal? (s-pos? -3) #f)
    (check-equal? (s-pos? 3) #t)
  )
)

