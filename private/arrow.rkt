#lang racket/base

;; Every about solvable function contracts

(provide
  ->
  ;; Macro for building solvable--> when possible, otherwise -> contracts

  make-solvable-->
  ;; (-> (listof contract?) contract? solvable-->?)
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
  (only-in racket/sandbox
    exn:fail:resource?
    call-with-limits)
  rosette-contract/private/base
  rosette-contract/private/flat
  rosette-contract/private/log
  rosette-contract/private/parameters
  (prefix-in C. racket/contract)
  (prefix-in R. rosette)
  (only-in racket/unsafe/ops
    unsafe-chaperone-procedure)
  (for-syntax
    racket/base
    rosette-contract/private/log
    syntax/parse)
)

;; =============================================================================

(define-syntax (-> stx)
  (syntax-parse stx
   [(_ dom* ... cod)
    #:when (syntax-parse #'cod [((~literal values) . e*) #f] [_ #t]) ;; not values
    (syntax/loc stx
      (make-solvable--> (list dom* ...) cod (C.-> dom* ... cod)))]
   [(_ . e*)
    #;(log-rosette-contract-error "failed to make solvable-> for ~a" (syntax->datum stx))
    (syntax/loc stx
      (C.-> . e*))]))

(define (solvable-->-name ctc)
  `(solvable-->
    ,(map C.contract-name (solvable-->-dom* ctc))
    ,(C.contract-name (solvable-->-cod ctc))))

(define (solvable-->-first-order ctc)
  (define dom* (solvable-->-dom* ctc))
  (define cod (solvable-->-cod ctc))
  (define arity (length dom*))
  (λ (val)
    (and (procedure? val)
         (procedure-arity-includes? val arity))))

(define (solvable-->-late-neg ctc)
  (define dom* (solvable-->-dom* ctc))
  (define cod (solvable-->-cod ctc))
  (λ (blame)
    (λ (f neg-party)
      ;; TODO
      ;; - [ ] lets just get something working
      ;; - [ ] macro for defining the N cases
      (define blame+ (C.blame-add-missing-party blame neg-party))
      (define-syntax-rule (check-domain dom x)
        (if (dom x)
          x
          (C.raise-blame-error (C.blame-swap blame+) x '(expected: "~a") dom)))
      (define-syntax-rule (check-codomain y)
        (if (or (the-trivial-predicate? cod) (cod y))
          y
          (C.raise-blame-error blame+ y '(expected "~a") cod)))
      (define wrapper
        (case (length dom*)
         [(0)
          (λ ()
            (check-codomain (f)))]
         [(1)
          (λ (x1)
            (check-codomain (f (check-domain (car dom*) x1))))]
         [(2)
          (λ (x1 x2)
            (check-codomain (f (check-domain (car dom*) x1)
                               (check-domain (cadr dom*) x2))))]
         [(3)
          (λ (x1 x2 x3)
            (check-codomain (f (check-domain (car dom*) x1)
                               (check-domain (cadr dom*) x2)
                               (check-domain (caddr dom*) x3))))]
         [(4)
          (λ (x1 x2 x3 x4)
            (check-codomain (f (check-domain (car dom*) x1)
                               (check-domain (cadr dom*) x2)
                               (check-domain (caddr dom*) x3)
                               (check-domain (cadddr dom*) x4))))]
         [(5)
          (λ (x1 x2 x3 x4 x5)
            (check-codomain (f (check-domain (car dom*) x1)
                               (check-domain (cadr dom*) x2)
                               (check-domain (caddr dom*) x3)
                               (check-domain (cadddr dom*) x4)
                               (check-domain (cadddr (cdr dom*)) x5))))]
         [else
          (λ x*
            (check-codomain (apply f (for/list ([dom (in-list dom*)]
                                                [x (in-list x*)])
                                       (check-domain dom x)))))]))
      (unsafe-chaperone-procedure f wrapper))))

(define (solvable-->-stronger this-ctc that-ctc)
  (cond
   [(solvable-->? that-ctc)
    (define this-dom* (solvable-->-dom* this-ctc))
    (define that-dom* (solvable-->-dom* that-ctc))
    (define this-cod (solvable-->-cod this-ctc))
    (define that-cod (solvable-->-cod that-ctc))
    (cond
     [(and (andmap solvable-predicate? this-dom*)
           (andmap solvable-predicate? that-dom*)
           (solvable-predicate? this-cod)
           (solvable-predicate? that-cod))
      (and (andmap solvable-predicate-stronger this-dom* that-dom*)
           (solvable-predicate-stronger this-cod that-cod))]
     [else
      ;; TODO higher-order functions
      #f])]
   [else
    #f]))

(define (solvable-->-simplify v ctc srcloc)
  (define dom* (solvable-->-dom* ctc))
  (define cod (solvable-->-cod ctc))
  (cond
   [(and (the-trivial-predicate? cod) (andmap the-trivial-predicate? dom*))
    the-trivial-predicate]
   [(rosette-trivial-codomain? v dom* cod)
    (define cb (R.current-bitwidth))
    (log-success ctc srcloc (format "trivial codomain (bitwidth ~a)" cb))
    (solved--> dom* cod cb)]
   [(rosette-impossible-codomain? v dom* cod)
    (raise-user-error 'solvable--> "function ~a cannot satisfy the contract ~a" v ctc)]
   [else
    (log-failure ctc srcloc)
    ctc]))

(struct solvable--> solvable-contract (
  dom*
  cod
)
#:transparent
#:methods gen:custom-write
[(define (write-proc v port mode)
   ((if mode write display) (solvable-->-name v) port))]
#:property C.prop:chaperone-contract
  (C.build-chaperone-contract-property
   #:name solvable-->-name
   #:first-order solvable-->-first-order
   #:late-neg-projection solvable-->-late-neg
   #:stronger solvable-->-stronger)
)

(define (make-solvable--> dom* cod default)
  (cond
   [(and (andmap solvable-predicate? dom*) ;; TODO solvable-contract
         (solvable-predicate? cod))
    (solvable--> dom* cod)]
   [else
    #;(log-rosette-contract-debug "failed to (make-solvable--> ~e ~e)" dom* cod)
    default]))

;; -----------------------------------------------------------------------------

(define (solved-->-name ctc)
  `(solved--> ,(solved-->-bitwidth ctc)
              ,@(cdr (solvable-->-name ctc))))

(define (solved-->-late-neg ctc)
  (define dom* (solvable-->-dom* ctc))
  (define cod (solvable-->-cod ctc))
  (λ (blame)
    (λ (f neg-party)
      ;; TODO
      ;; - [ ] lets just get something working
      (define blame+ (C.blame-add-missing-party blame neg-party))
      (define cb (R.current-bitwidth))
      (define-syntax-rule (check-domain dom x)
        (if (dom x)
          x
          (C.raise-blame-error (C.blame-swap blame+) x '(expected: "~a") dom)))
      (define-syntax-rule (check-codomain y)
        (if (or (the-trivial-predicate? cod) (cod y))
          y
          (C.raise-blame-error blame+ y '(expected "~a") cod)))
      (define wrapper
        (case (length dom*)
         [(0)
          (λ ()
            (check-codomain (f)))]
         [(1)
          (λ (x1)
            (let ([y (f (check-domain (car dom*) x1))])
              (if (small-enough? x1 cb)
                y
                (check-codomain y))))]
         [(2)
          (λ (x1 x2)
            (let ([y (f (check-domain (car dom*) x1)
                        (check-domain (cadr dom*) x2))])
              (if (and (small-enough? x1 cb) (small-enough? x2 cb))
                y
                (check-codomain y))))]
         [else
          (λ x*
            (let ([y (apply f (for/list ([dom (in-list dom*)]
                                         [x (in-list x*)])
                                (check-domain dom x)))])
              (if (for/and ([x (in-list x*)]) (small-enough? x cb))
                y
                (check-codomain y))))]))
      (unsafe-chaperone-procedure f wrapper))))

(struct solved--> solvable--> (
  bitwidth
)
#:property C.prop:chaperone-contract
  (C.build-chaperone-contract-property
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
   [(R.term? val)
    (term-small-enough? val bitwidth)]
   [else ; unsound to return #t
    (log-rosette-contract-warning "unable to check (small-enough? ~a ~a)" val bitwidth)
    #f]))

(define (term-small-enough? val bitwidth)
  (define t (R.type-of val))
  (cond
   [(or (eq? t R.integer?) (eq? t R.real?))
    (R.assert (R.< val (R.expt 2 bitwidth)))
    #t]
   [(eq? t R.boolean?)
    #t]
   [else
    #f]))

(define (handle-resource-exn e)
  (log-rosette-contract-debug "SMT resource limit reached:~n~a" (exn-message e))
  #f)

(define (no-counterexamples/function f #:forall D* #:assume P* #:derive Q)
  (log-rosette-contract-debug (format-solver-query D* P* Q))
  (with-handlers ([exn:fail:resource? handle-resource-exn])
    (call-with-limits (*solver-seconds-limit*) (*solver-mb-limit*)
      (λ ()
        (let ([x* (for/list ([D (in-list D*)]) (R.define-symbolic* x D) x)])
          (and (not ;; -- can we prove anything about `f` ?
                 (R.unsat? (R.solve (R.assert (apply f x*)))))
               (let ([m (R.solve
                          (for ([x (in-list x*)] [P (in-list P*)] #:when P)
                            (R.assert (P x)))
                          (R.assert (Q (apply f x*))))])
                 (if (R.unsat? m) ;; -- try to prove `P => Q ∘ f`
                   #t
                   (begin
                     (log-rosette-contract-warning "counterexample ~a"
                       (cons (object-name f) (for/list ([x (in-list x*)]) (R.evaluate x m))))
                     #f)))))))))

;; -----------------------------------------------------------------------------
;; TODO make extensible, maybe as struct property

(define (solvable-contract-D ctc)
  (cond
   [(solvable-predicate? ctc)
    (solvable-predicate-D ctc)]
   [(solvable-->? ctc)
    (define dom*-D (map solvable-predicate-D (solvable-->-dom* ctc)))
    (define cod-D (solvable-predicate-D (solvable-->-cod ctc)))
    (apply R.~> (append dom*-D (list cod-D)))]
   [else
    (error 'solvable-contract-D "cannot generate domain for ~a" ctc)]))

;; TODO purpose statement
(define (solvable-contract-P ctc)
  (cond
   [(solvable-predicate? ctc)
    (solvable-predicate-P ctc)]
   [(solvable-->? ctc)
    #f]
   [else
    (error 'solvable-contract-P "cannot generate assertion for ~a" ctc)]))

;; end TODO
;; -----------------------------------------------------------------------------

(define (rosette-trivial-codomain? v dom* cod)
  (or (the-trivial-predicate? cod)
      (and (not (ormap the-trivial-predicate? dom*)) ;; if trivial, cannot generate inputs
           (let ([cod-P (solvable-contract-P cod)])
             (and cod-P
                  (log-rosette-contract-debug "SOLVE trivial-codomain? ~a~n" (object-name v))
                  (no-counterexamples/function v
                    #:forall (map solvable-contract-D dom*)
                    #:assume (map solvable-contract-P dom*)
                    #:derive (λ (y) (R.not (cod-P y)))))))))

(define (rosette-impossible-codomain? v dom* cod)
  (and (not (the-trivial-predicate? cod))
       (not (ormap the-trivial-predicate? dom*)) ;; if trivial, cannot generate inputs
       (solvable-predicate? cod)
       (let ([cod-P (solvable-contract-P cod)])
         (and cod-P
              (log-rosette-contract-debug "SOLVE impossible-codomain? ~a~n" (object-name v))
              (no-counterexamples/function v
                #:forall (map solvable-predicate-D dom*)
                #:assume (map solvable-predicate-P dom*)
                #:derive cod-P)))))

;; =============================================================================

(module+ test
  (require
    rackunit
    racket/string
    rosette-contract/private/env-flat)

  (define i->i (solvable--> (list integer?) integer?))
  (define p->i (solvable--> (list positive?) integer?))
  (define p->n (solvable--> (list positive?) negative?))

  (test-case "solvable-->-name"
    (let ([nm (solvable-->-name i->i)])
      (check-equal? (car nm) 'solvable-->)
      (let ([dom-name (caadr nm)])
        (check-equal? (car dom-name) 'solvable-predicate)
        (check-equal? (cadr dom-name) 'int?))
      (check-equal? (caaddr nm) 'solvable-predicate)
      (check-equal? (cadr (caddr nm)) 'int?)))

  (test-case "solvable-->-first-order"
    (let ([fo (solvable-->-first-order i->i)])
      (check-true (fo (λ (x) x)))
      (check-false (fo #f))
      (check-false (fo 2))
      (check-false (fo (λ (x y) x)))))

  (test-case "solvable-->-late-neg"
    (let* ([f1 (C.contract i->i (λ (x) x) 'pos 'neg-party)]
           [f2 (C.contract i->i (λ (x) "yolo") 'pos 'neg)])
      (check-equal? (f1 5) 5)
      (check-exn C.exn:fail:contract:blame?
        (λ () (f1 "yo")))
      (check-exn #rx"blaming: neg-party"
        (λ () (f1 "yo")))
      (check-exn C.exn:fail:contract:blame?
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
    (define srcloc (make-srcloc 'arrow.rkt 8 6 7 5))

    (let ([trivial-box
           (force/rc-log
             (λ ()
               (let ([ctc+ (solvable-->-simplify (λ (x) x) i->i srcloc)])
                 (check-equal? integer? (car (solvable-->-dom* ctc+)))
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

  (test-case "term-small-enough?"
    (let ()
      (R.define-symbolic* x R.integer?)
      (check-true (term-small-enough? x 1))
      (check-true (term-small-enough? x 3))
      (check-true (term-small-enough? x 10))
      (R.clear-asserts!))

    (let ()
      (R.define-symbolic* x R.real?)
      (check-true (term-small-enough? x 1))
      (check-true (term-small-enough? x 3))
      (check-true (term-small-enough? x 10))
      (R.clear-asserts!))

    (let ()
      (R.define-symbolic* x R.boolean?)
      (check-true (term-small-enough? x 1))
      (check-true (term-small-enough? x 3))
      (check-true (term-small-enough? x 10))
      (R.clear-asserts!))
  )

  (test-case "rosette-trivial-codomain"
    (check-true (rosette-trivial-codomain? (λ (x) (if (R.= x 3) 1 2)) (list integer?) integer?))
    (check-true (rosette-trivial-codomain? (λ (x) (R.* x -1)) (list negative?) positive?))
    (check-true (rosette-trivial-codomain? (λ (x y) (R.* x y)) (list positive? positive?) positive?))


    ;; need to use Rosette's arithmetic
    (check-false (rosette-trivial-codomain? (λ (x) (R.* x -1)) (list integer?) negative?))
    (check-false (rosette-trivial-codomain? (λ (x) (+ x 1)) (list integer?) integer?))
    (let ([inbox
           (force/rc-log #:level 'debug
             (λ ()
               (check-false
                 (rosette-trivial-codomain?
                   (λ (x)
                     (R.cond
                      [(R.equal? x (vector-ref (current-command-line-arguments) 0))
                       1]
                      [else
                       2])) (list integer?) integer?))))])
       (define debug-msgs (hash-ref inbox 'debug))
       (check-equal? (length debug-msgs) 2)
       (check-true (string-prefix? (car debug-msgs) "rosette-contract: SOLVE trivial-codomain"))
       (check-true (string-prefix? (cadr debug-msgs) "rosette-contract: SMT query")))
  )

  (test-case "solver-timeout"
    (let ([inbox
           (parameterize ([*solver-seconds-limit* 3])
             (force/rc-log #:level 'debug
               (λ ()
                 (check-false
                   (rosette-trivial-codomain?
                     (letrec ([f (λ (x) (f x))]) f)
                     (list integer?) integer?)))))])
      (define debug-msgs (hash-ref inbox 'debug))
      (check-equal? (length debug-msgs) 3)
      (check-true (string-prefix? (cadr debug-msgs) "rosette-contract: SMT query"))
      (check-true (string-prefix? (caddr debug-msgs) "rosette-contract: SMT resource limit"))))

  (test-case "rosette-impossible-codomain"
    (check-true (rosette-impossible-codomain? (λ (x) -4) (list integer?) positive?))
    (check-true (rosette-impossible-codomain? (λ (x) (R.* x -1)) (list positive?) positive?))

    (check-false (rosette-impossible-codomain? (λ (x) (R.* x -1)) (list integer?) positive?))
  )

  (test-case "end-to-end"
    (define f+ (C.contract i->i (λ (x) x) 'pos 'neg))
    (check-equal? (f+ 3) 3)
    (check-exn C.exn:fail:contract:blame?
      (λ () (f+ 'a)))
  )
)
