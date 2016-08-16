#lang racket/base

(provide
  (rename-out
   [C.-> ->]
   [C.or/c or/c])
  ;;; TODO

  define/contract
  ;; TODO doc
  ;; (It's a little heavy for a tailored-out, but that's the right idea

  ;contract-out
  ;;; TODO
)

(require
  (for-syntax
    racket/base
    rosette-contract/private/log
    rosette-contract/private/parameters
    syntax/parse)
  (prefix-in C. racket/contract)
  (prefix-in R. rosette)
  rosette-contract/private/parameters
)

;; =============================================================================

(define-syntax (define/contract stx)
  (syntax-parse stx #:literals (C.->)
   [(_ (f:id var*:id ...+)
      (C.-> pre* ... post)
      body* ...)
    (quasisyntax/loc stx
      (define f
        ;; Uncontracted version
        (letrec ([f (λ (var* ...) body* ...)])
          (let ([ctc (triple->contract {[var* pre*] ...} f {post})])
            ;; 2016-08-15: should blame `f` in negative position
            (C.contract ctc f (C.current-contract-region) (C.current-contract-region))))))]
   [(_ arg* ...)
    (when (*RCONTRACT-LOG*)
      (displayln (format-log "fall back to racket/contract for ~a" (cons 'define/contract (syntax-e #'(arg* ...))))))
    (syntax/loc stx
      (C.define/contract arg* ...))]))

(define-syntax (triple->contract stx)
  (syntax-parse stx
   [(_ {[var* pre*] ...} f {post}) ;; TODO generalize postcondition
    #:with r (gensym 'result)
    (define success-msg
      (format-log "optimized codomain" #'(C.-> pre* ... post)))
    (quasisyntax/loc stx
      (if (verify-codomain {[var* pre*] ...} f {post})
      ;;if (verify-codomain (list (list var* pre*) ...) f (list post))
        (let ([in-bitwidth/c (make-bitwidth/c)])
          (when (*RCONTRACT-LOG*)
            (displayln '#,success-msg))
          (C.->i ([var* pre*]
                  ...)
                 [r (var* ...) (C.or/c (in-bitwidth/c var* ...)
                                       post)]))
        (C.-> pre* ... post)))]))

;; -----------------------------------------------------------------------------

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
      #:name (format "rosette-bitwidth<~a/c" cb)
      #:first-order (lambda (x) pass?)
      #:late-neg-projection (lambda (pos-blame) (lambda (val neg-blame) (error 'bitwidth/c-late-neg "Not implemented")))))
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

;; 2016-08-15 : maybe later
;(struct bitwidth/c
;  (bitwidth)
;  #:property prop:contract
;  (C.build-flat-contract-property
;    TODO))
;(struct bitwidth/c/fail (bitwidth/c))

(define-syntax-rule (verify-codomain {[var* pre*] ...} f {post})
  (let ()
    (R.define-symbolic var* ... R.integer?)
    (R.unsat?
      (R.solve (begin
        (R.assert pre* var*)
        ...
        (R.assert (not (post (f var* ...)))))))))

