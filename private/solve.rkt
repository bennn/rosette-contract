#lang racket/base

;; Front-end for simplifying contracts with Rosette

(provide
  no-counterexamples
  ;; TODO
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

(define (no-counterexamples f #:forall pre-D* #:assume pre-P* #:derive Q)
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
                            ((rosette-contract-assert! P) x))
                          (R.assert (Q (apply f x*))))])
                 (if (R.unsat? m) ;; -- try to prove `P => Q ∘ f`
                   #t
                   (begin
                     (log-rosette-contract-warning "counterexample ~a"
                       (cons (object-name f) (for/list ([x (in-list x*)]) (R.evaluate x m))))
                     #f)))))))))

;; =============================================================================
