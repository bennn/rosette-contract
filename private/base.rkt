#lang racket/base

(provide
  prop:rosette-contract

  build-rosette-contract-property
  ;; (-> #:assert! get-assert #:encode get-encode #:simplify get-simplify rosette-contract-property?)
  ;; Build a new property for a rosette contracts

  rosette-contract-property?
  ;; (-> any/c boolean?)

  rosette-contract?
  ;; (-> any/c boolean?)

  rosette-contract-assert!
  ;; (-> rosette-contract-struct? (-> term? void?))
  ;; Update Rosette's current [assertion store] with facts that
  ;;  hold if the given contract is assumed true.

  rosette-contract-encode
  ;; (-> rosette-contract-struct? term?)
  ;; Convert the given contract to symbolic form, return a [symbolic term]
  ;;  representing a value that meets the contract.

  rosette-contract-simplify
  ;; (-> rosette-contract-struct? (->* [any/c] [source-location?] rosette-contract-struct))
  ;; Return a possibly-simplified version of the given contract.
  ;; If the given value can never violate part of the given contract,
  ;;  that part of the contract should be a no-op in the simplified version.

  the-trivial-contract
  ;; solvable-contract?
  ;; Represents a trivially true predicate

  the-trivial-contract?
  ;; (-> solvable-contract? boolean?)
  ;; Returns #true if the given contract is trivial in the current context
)

(require
  racket/contract
  (for-syntax racket/base syntax/parse racket/syntax))

;; -----------------------------------------------------------------------------

(struct rosette-contract-property (
  assert!
  encode
  simplify
)
#:omit-define-syntaxes)

(define rosette-contract-property-symbol 'prop:rosette-contract)

(define (rosette-contract-property-guard prop info)
  (if (rosette-contract-property? prop)
    prop
    (raise
     (make-exn:fail:contract
      (format "~a: expected a rosette-contract-property; got: ~e"
              rosette-contract-property-symbol
              prop)
      (current-continuation-marks)))))

(define-values (prop:rosette-contract rosette-contract? rosette-contract-struct-property)
  (make-struct-type-property
    rosette-contract-property-symbol
    rosette-contract-property-guard))

(define-syntax (define-rosette-contract-accessor stx)
  (syntax-parse stx
   [(_ field-name:id)
    #:with accessor (format-id stx "rosette-contract-~a" (syntax-e #'field-name))
    #:with prop-accessor (format-id stx "rosette-contract-property-~a" (syntax-e #'field-name))
    (syntax/loc stx
      (define (accessor c)
        (let* ([prop (rosette-contract-struct-property c)]
               [f (prop-accessor prop)]
               [v (f c)])
          v)))]))

(define-rosette-contract-accessor assert!)
(define-rosette-contract-accessor encode)
(define-rosette-contract-accessor simplify)

(define (build-rosette-contract-property
         #:assert! get-assert
         #:encode get-encode
         #:simplify get-simplify)
  (rosette-contract-property get-assert get-encode get-simplify))

;; -----------------------------------------------------------------------------

(struct trivial ()
#:property prop:rosette-contract
  (build-rosette-contract-property
   #:assert! (λ (ctc) (λ (v) (void)))
   #:encode (λ (ctc) (raise-user-error 'rosette-contract "cannot encode the trivial contract"))
   #:simplify (λ (ctc) (λ (v) ctc)))
#:property prop:flat-contract
  (build-flat-contract-property
   #:name (λ (ctc) 'rosette-trivial-contract)
   #:first-order (λ (ctc) (λ (v) #t))
   #:late-neg-projection (λ (ctc) (λ (blame) (λ (v missing) v)))
   #:stronger (λ (this that) (the-trivial-contract? that)))
)

(define the-trivial-contract (trivial))

(define (the-trivial-contract? v)
  (eq? v the-trivial-contract))

;; -----------------------------------------------------------------------------

(module+ test
  (require rackunit)

  (check-true (the-trivial-contract? the-trivial-contract))
  (check-true (rosette-contract? the-trivial-contract))

  (check-false (the-trivial-contract? 3))
  (check-false (the-trivial-contract? 'foo))
)
