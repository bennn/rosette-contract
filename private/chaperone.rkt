#lang racket/base

;; TODO
;; - double-check, are the error messages readable?

(provide
;  (rename-out
;   [--> ->])
)

(require
  rosette-contract/private/base
  rosette-contract/private/flat
  racket/contract
  (only-in racket/unsafe/ops
    unsafe-chaperone-procedure)
)

;; =============================================================================

(define (solvable-->-name ctc)
  `(solvable-->
    ,(contract-name (solvable-->-dom ctc))
    ,(contract-name (solvable-->-cod ctc))))

(define (solvable-->-first-order ctc)
  (define dom (solvable-->-dom ctc))
  (define cod (solvable-->-cod ctc))
  (define arity 1)
  (λ (val)
    (and (procedure? val)
         (procedure-arity-includes? val arity))))

(define (solvable-->-late-neg ctc)
  (define dom (solvable-->-dom ctc))
  (define cod (solvable-->-cod ctc))
  (define (wrapper x)
    (cod (dom x)))
  (λ (blame)
    (λ (f neg-party)
      ;; TODO
      ;; - [ ] lets just get something working
      (define blame+ (blame-add-missing-party blame neg-party))
      (define (wrapper x)
        (let* ([y (if (dom x)
                    (f x)
                    (raise-blame-error (blame-swap blame+) x '(expected "~a") dom))])
          (if (cod y)
            y
            (raise-blame-error blame+ x '(expected "~a") cod))))
      (chaperone-procedure f wrapper))))

(define (solvable-->-stronger this-ctc that-ctc)
  (cond
   [(solvable-->? that-ctc)
    (define this-dom (solvable-->-dom this-ctc))
    (define that-dom (solvable-->-dom that-ctc))
    (define this-cod (solvable-->-cod this-ctc))
    (define that-cod (solvable-->-cod that-ctc))
    (cond
     [(and (solvable-predicate? this-dom) (solvable-predicate? that-dom)
           (solvable-predicate? this-cod) (solvable-predicate? that-cod))
      (and (solvable-predicate-stronger this-dom that-dom)
           (solvable-predicate-stronger this-cod that-cod))]
     [else
      ;; TODO higher-order functions
      #f])]
   [else
    #f]))

(struct solvable--> solvable-contract (
  dom ;; TODO generalize to dom*, will be harder to make chaperones
  cod
)
#:transparent
#:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name solvable-->-name
   #:first-order solvable-->-first-order
   #:late-neg-projection solvable-->-late-neg
   #:stronger solvable-->-stronger)
)

;; =============================================================================

(module+ test
  (require
    rackunit
    rosette-contract/private/env-flat
    (only-in racket/contract exn:fail:contract:blame?))

  (define i->i (solvable--> integer? integer?))
  (define p->i (solvable--> positive? integer?))
  (define p->n (solvable--> positive? negative?))

  (test-case "solvable-->-name"
    (let ([nm (solvable-->-name i->i)])
      (check-equal? (car nm) 'solvable-->)
      (check-equal? (caadr nm) 'solvable-predicate)
      (check-equal? (cadadr nm) 'int?)
      (check-equal? (caaddr nm) 'solvable-predicate)
      (check-equal? (cadr (caddr nm)) 'int?)))

  (test-case "solvable-->-first-order"
    (let ([fo (solvable-->-first-order i->i)])
      (check-true (fo (λ (x) x)))
      (check-false (fo #f))
      (check-false (fo 2))
      (check-false (fo (λ (x y) x)))))

  (test-case "solvable-->-late-neg"
    (let* ([f1 (contract i->i (λ (x) x) 'pos 'neg-party)]
           [f2 (contract i->i (λ (x) "yolo") 'pos 'neg)])
      (check-equal? (f1 5) 5)
      (check-exn exn:fail:contract:blame?
        (λ () (f1 "yo")))
      (check-exn #rx"blaming: neg-party"
        (λ () (f1 "yo")))
      (check-exn exn:fail:contract:blame?
        (λ () (f2 3)))
      (check-exn #rx"broke its own contract"
        (λ () (f2 3)))))

  (test-case "solvable-->-stronger"
    (check-true (solvable-->-stronger i->i i->i))
    (check-true (solvable-->-stronger p->i i->i))

    (check-false (solvable-->-stronger i->i p->i))
    (check-false (solvable-->-stronger p->i p->n))
  )

)
