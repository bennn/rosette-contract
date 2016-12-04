#lang racket/base

;; Every about solvable function contracts

(provide
  (rename-out [make-solvable--> ->])
  ;; Macro for building solvable--> when possible, otherwise -> contracts
  ;;
  ;; (-> contract? contract? solvable-->?)
  ;; Create a possibly-solvable function contract
  ;; If arguments are solvable, then Rosette can solve using the newly-created
  ;;  function contract

  solvable-->?
  ;; (-> any/c boolean?)
  ;; Return #true if the argument is a solvable--> contract

  solved-->?
  ;; (-> any/c boolean?)
  ;; Return #true if the argument is a solvable--> contract
  ;;  whose codomain always holds*
  ;; *modulo the current bitwidth

  solvable-->-stronger
  ;; (-> solvable-->? solvable-->? boolean?)
  ;; Returns #true if the first function contract accepts fewer values than
  ;;  the second.

  solvable-->-simplify
  ;; (-> any/c solvable-->? contract?)
  ;; Simplify the given contract with respect to the given value.
)

(require
  rosette-contract/private/base
  rosette-contract/private/flat
  rosette-contract/private/log
  racket/contract
  (prefix-in R. rosette)
  (only-in racket/unsafe/ops
    unsafe-chaperone-procedure)
  (for-syntax
    racket/base
    syntax/parse)
)

;; =============================================================================

(define-syntax (make-solvable--> stx)
  (syntax-parse stx
   [(_ dom cod)
    (syntax/loc stx
      (solvable--> dom cod))]
   [(_ . e*)
    (syntax/loc stx
      (C.-> . e*))]))

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
  (λ (blame)
    (λ (f neg-party)
      ;; TODO
      ;; - [ ] lets just get something working
      (define blame+ (blame-add-missing-party blame neg-party))
      (define (wrapper x)
        (let* ([y (if (dom x)
                    (f x)
                    (raise-blame-error (blame-swap blame+) x '(expected "~a") dom))])
          (if (or (the-trivial-predicate? cod) (cod y))
            y
            (raise-blame-error blame+ x '(expected "~a") cod))))
      (unsafe-chaperone-procedure f wrapper))))

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

(define (solvable-->-simplify v ctc srcloc)
  (define dom (solvable-->-dom ctc))
  (define cod (solvable-->-cod ctc))
  (cond
   [(the-trivial-predicate? cod)
    (if (the-trivial-predicate? dom) the-trivial-predicate ctc)]
   [(rosette-trivial-codomain? v dom cod)
    (define cb (R.current-bitwidth))
    (log-success ctc srcloc (format "trivial codomain (bitwidth ~a)" cb))
    (solved--> dom cod cb)]
   [(rosette-impossible-codomain? v dom cod)
    (raise-user-error 'solvable--> "function ~a cannot satisfy the contract ~a" v ctc)]
   [else
    (log-failure ctc srcloc)
    ctc]))

(struct solvable--> solvable-contract (
  dom ;; TODO generalize to dom*, will be harder to make chaperones
  cod
)
#:transparent
#:methods gen:custom-write
[(define (write-proc v port mode)
   ((if mode write display) (solvable-->-name v) port))]
#:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name solvable-->-name
   #:first-order solvable-->-first-order
   #:late-neg-projection solvable-->-late-neg
   #:stronger solvable-->-stronger)
)

;; -----------------------------------------------------------------------------

(define (solved-->-name ctc)
  `(solved--> ,(solved-->-bitwidth ctc)
    ,(contract-name (solvable-->-dom ctc))
    ,(contract-name (solvable-->-cod ctc))))

(define (solved-->-late-neg ctc)
  (define dom (solvable-->-dom ctc))
  (define cod (solvable-->-cod ctc))
  (define bw (solved-->-bitwidth ctc))
  (λ (blame)
    (λ (f neg-party)
      ;; TODO
      ;; - [ ] lets just get something working
      (define blame+ (blame-add-missing-party blame neg-party))
      (define (wrapper x)
        (let* ([y (if (dom x)
                    (f x)
                    (raise-blame-error (blame-swap blame+) x '(expected "~a") dom))])
          (if (or (the-trivial-predicate? cod)
                  (small-enough? x bw) ;; NEW
                  (cod y))
            y
            (raise-blame-error blame+ x '(expected "~a") cod))))
      (unsafe-chaperone-procedure f wrapper))))

(struct solved--> solvable--> (
  bitwidth
)
#:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name solved-->-name
   #:first-order solvable-->-first-order
   #:late-neg-projection solved-->-late-neg
   #:stronger solvable-->-stronger)
)

;; -----------------------------------------------------------------------------

(define (small-enough? val bitwidth)
  (cond
   [(not bitwidth)
    #true]
   [(number? val)
    (<= val (expt 2 bitwidth))]
   [else ; unsound
    (log-rosette-contract-warning "unable to check (small-enough? ~a ~a)" val bitwidth)
    #f]))

(define (rosette-trivial-codomain? v dom cod)
  (and (solvable-predicate? dom)
       (not (the-trivial-predicate? dom))
       (solvable-predicate? cod)
       (let ([dom-D (solvable-predicate-D dom)]
             [dom-P (solvable-predicate-P dom)]
             [cod-P (solvable-predicate-P cod)])
         (R.define-symbolic* x dom-D)
         (and (not (R.unsat? (R.solve (R.assert (v x)))))
              (R.unsat?
                (R.solve (R.assert (dom-P x))
                         (R.assert (R.not (cod-P (v x))))))))))

(define (rosette-impossible-codomain? v dom cod)
  (and (solvable-predicate? dom)
       (not (the-trivial-predicate? dom))
       (solvable-predicate? cod)
       (let ([dom-D (solvable-predicate-D dom)]
             [dom-P (solvable-predicate-P dom)]
             [cod-P (solvable-predicate-P cod)])
         (R.define-symbolic* x dom-D)
         (and (not (R.unsat? (R.solve (R.assert (v x)))))
              (R.unsat?
                (R.solve (R.assert (dom-P x))
                         (R.assert (cod-P (v x)))))))))

;; =============================================================================

(module+ test
  (require
    rackunit
    racket/string
    rosette-contract/private/env-flat)

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

  (test-case "solvable-->simplify"
    (define srcloc (make-srcloc 'chaperone.rkt 8 6 7 5))

    (let ([trivial-box
           (force/rc-log
             (λ ()
               (let ([ctc+ (solvable-->-simplify (λ (x) x) i->i srcloc)])
                 (check-equal? integer? (solvable-->-dom ctc+))
                 (check-equal? integer? (solvable-->-cod ctc+))
                 (check-true (solved-->? ctc+))
                 (check-equal? (R.current-bitwidth) (solved-->-bitwidth ctc+))
                 (void))))])
      (define info-msgs (hash-ref trivial-box 'info))
      (check-equal? (length info-msgs) 1)
      (check-true (null? (hash-ref trivial-box 'error)))
      (check-true (null? (hash-ref trivial-box 'warning)))
      (check-true (null? (hash-ref trivial-box 'debug)))
      (check-true (null? (hash-ref trivial-box 'fatal)))
      (define info-msg (car info-msgs))
      (check-true (string-contains? info-msg "trivial codomain")))

    (check-exn #rx"cannot satisfy the contract"
      (λ () (solvable-->-simplify (λ (x) 'yo) i->i srcloc)))

    (let ([impossible-box
           (force/rc-log
             (λ ()
               (check-equal? (solvable-->-simplify (λ (x) (* 1 2 x)) i->i srcloc) i->i)))])
      (define info-msg (car (hash-ref impossible-box 'info)))
      (check-true (string-contains? info-msg "-")))
  )

  (test-case "small-enough?"
    (check-true (small-enough? 0 10))
    (check-true (small-enough? 1 2))
    (check-true (small-enough? 5 3))
    (check-true (small-enough? 9999 #f))
    (check-true (small-enough? (expt 2 6) 6))

    (check-false (small-enough? 3 1))
    (check-false (small-enough? (+ 1 (expt 2 6)) 6)))

  (test-case "rosette-trivial-codomain"
    (check-true (rosette-trivial-codomain? (λ (x) (if (R.= x 3) 1 2)) integer? integer?))
    (check-true (rosette-trivial-codomain? (λ (x) (R.* x -1)) negative? positive?))

    ;; damn, need to use Rosette's arithmetic
    (check-false (rosette-trivial-codomain? (λ (x) (R.* x -1)) integer? negative?))
    (check-false (rosette-trivial-codomain? (λ (x) (+ x 1)) integer? integer?))
    (check-false
      (rosette-trivial-codomain?
        (λ (x)
          (R.cond
           [(R.equal? x (vector-ref (current-command-line-arguments) 0))
            1]
           [else
            2])) integer? integer?))
  )

  (test-case "rosette-impossible-codomain"
    (check-true (rosette-impossible-codomain? (λ (x) -4) integer? positive?))
    (check-true (rosette-impossible-codomain? (λ (x) (R.* x -1)) positive? positive?))

    (check-false (rosette-impossible-codomain? (λ (x) (R.* x -1)) integer? positive?))
  )

  (test-case "end-to-end"
    (define f+ (contract i->i (λ (x) x) 'pos 'neg))
    (check-equal? (f+ 3) 3)
    (check-exn exn:fail:contract:blame?
      (λ () (f+ 'a)))
  )
)
