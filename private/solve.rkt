#lang racket/base

;; API to the Rosette solver

(provide
  no-examples no-counterexamples
  ;; (-> (-> any/c * any/c)
  ;;     #:forall (or/c rosette-contract? (listof rosette-contract?))
  ;;     #:assume (or/c rosette-contract? (listof rosette-contract?))
  ;;     #:derive rosette-contract?
  ;;     boolean?)
  ;;
  ;; (no-counterexamples f #:forall X* #:assume P* #:derive Q) returns true
  ;; Given `(length X*)` symbolic variables, described by respective elements of `X*`
  ;; Assert `P*` about each variable
  ;; Apply `f`
  ;; and Assert `Q` about the conclusion (for examples)
  ;;  or Assert `not Q` about the conclusion (for counterexamples)
)

(require
  rosette-contract/private/base
  rosette-contract/private/util/log
  rosette-contract/private/util/parameters
  (only-in racket/sandbox
    exn:fail:resource?
    call-with-limits)
  (prefix-in R. rosette)
)

;; =============================================================================

(define (handle-resource-exn e)
  (log-rosette-contract-debug "SMT resource limit reached:~n~a" (exn-message e))
  #f)

(define (no-examples f #:forall pre-D* #:assume pre-P* #:derive Q)
  (rosette-unsat? f #false pre-D* pre-P* Q))

(define (no-counterexamples f #:forall pre-D* #:assume pre-P* #:derive Q)
  (rosette-unsat? f #true pre-D* pre-P* Q))

(define (rosette-unsat? f negate-conclusion? pre-D* pre-P* Q)
  (define D* (if (list? pre-D*) pre-D* (list pre-D*)))
  (define P* (if (list? pre-P*) pre-P* (list pre-P*)))
  (log-rosette-contract-debug (format-solver-query D* P* Q))
  (with-handlers ([exn:fail:resource? handle-resource-exn])
    (call-with-limits (*solver-seconds-limit*) (*solver-mb-limit*)
      (λ ()
        (let ([x* (for/list ([D (in-list D*)]) (rosette-contract-encode D))])
          (and (not ;; -- can we prove anything about `f` ?
                 (R.unsat? (R.solve (R.assert (apply f x*)))))
               (let ([m (R.solve
                          (for ([x (in-list x*)] [P (in-list P*)] #:when P)
                            (R.assert ((rosette-contract-assert P) x)))
                          (let ([y (apply f x*)]
                                [q (rosette-contract-assert Q)])
                            (if negate-conclusion?
                              (R.assert (R.not (q y)))
                              (R.assert (q y)))))])
                 (if (R.unsat? m) ;; -- try to prove `P => Q ∘ f`
                   #t
                   (begin
                     (log-rosette-contract-warning "counterexample ~a"
                       (cons (object-name f) (for/list ([x (in-list x*)]) (R.evaluate x m))))
                     #f)))))))))

;; =============================================================================
