#lang racket/base

;; Rosette-solvable function contracts

(provide
  ->
  ;; Macro for building rosette--> when possible, otherwise -> contracts

  make-rosette-->
  ;; (-> (listof contract?) contract? rosette-->?)
  ;; Create a possibly-solvable function contract
  ;; If arguments are solvable, then Rosette can solve using the newly-created
  ;;  function contract

  rosette-->?
  ;; (-> any/c boolean?)
  ;; Return #true if the argument is a rosette--> contract

  solved-->?
  ;; (-> any/c boolean?)
  ;; Return #true if the argument is a rosette--> contract
  ;;  whose codomain always holds*
  ;; *modulo the current bitwidth
)

(require
  rosette-contract/private/base
  rosette-contract/private/flat
  rosette-contract/private/solve
  rosette-contract/private/util/log
  rosette-contract/private/util/parameters
  (only-in racket/unsafe/ops
    unsafe-chaperone-procedure)
  (prefix-in C. racket/contract)
  (prefix-in R. rosette)
  (for-syntax
    racket/base
    rosette-contract/private/util/log
    syntax/parse)
)

;; =============================================================================

(define-syntax (-> stx)
  (syntax-parse stx
   [(_ dom* ... cod)
    #:when (syntax-parse #'cod [((~literal values) . e*) #f] [_ #t]) ;; not values
    (syntax/loc stx
      (make-rosette--> (list dom* ...) cod (C.-> dom* ... cod)))]
   [(_ . e*)
    #;(log-rosette-contract-error "failed to make solvable-> for ~a" (syntax->datum stx))
    (syntax/loc stx
      (C.-> . e*))]))

(define (rosette-->-name ctc)
  (format "#<rosette-->:~a:~a>"
          (map C.contract-name (rosette-->-dom* ctc))
          (C.contract-name (rosette-->-cod ctc))))

(define (rosette-->-first-order ctc)
  (define dom* (rosette-->-dom* ctc))
  (define cod (rosette-->-cod ctc))
  (define arity (length dom*))
  (λ (val)
    (and (procedure? val)
         (procedure-arity-includes? val arity))))

(define (rosette-->-late-neg ctc)
  (define dom* (rosette-->-dom* ctc))
  (define cod (rosette-->-cod ctc))
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
        (if (or (the-trivial-contract? cod) (cod y))
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


;; (-> rosette-->? rosette-->? boolean?)
;; Returns #true if the first function contract accepts fewer values than
;;  the second.
(define (rosette-->-stronger this-ctc that-ctc)
  (cond
   [(the-trivial-contract? that-ctc)
    #true]
   [(rosette-->? that-ctc)
    (log-rosette-contract-info "stronger? ~a ~a"
                               (rosette-->-name this-ctc)
                               (rosette-->-name that-ctc))
    (define this-dom* (rosette-->-dom* this-ctc))
    (define that-dom* (rosette-->-dom* that-ctc))
    (define this-cod (rosette-->-cod this-ctc))
    (define that-cod (rosette-->-cod that-ctc))
    (and (andmap C.contract-stronger? this-dom* that-dom*)
         (C.contract-stronger? this-cod that-cod))]
   [else
    #f]))

(define (rosette-->-assert! ctc)
  (raise-user-error 'cannotassert->))

(define (rosette-->-encode ctc)
  (raise-user-error 'cannotencode))

;; (-> any/c rosette-->? contract?)
;; Simplify the given contract with respect to the given value.
(define ((rosette-->-simplify ctc) v srcloc)
  (define dom* (rosette-->-dom* ctc))
  (define cod (rosette-->-cod ctc))
  (cond
   [(and (the-trivial-contract? cod) (andmap the-trivial-contract? dom*))
    the-trivial-contract]
   [(rosette-trivial-codomain? v dom* cod)
    (define cb (R.current-bitwidth))
    (log-success ctc srcloc (format "trivial codomain (bitwidth ~a)" cb))
    (solved--> dom* cod cb)]
   [(rosette-impossible-codomain? v dom* cod)
    (raise-user-error 'rosette--> "function ~a cannot satisfy the contract ~a" v ctc)]
   [else
    (log-failure ctc srcloc)
    ctc]))

(struct rosette--> (
  dom*
  cod
)
#:transparent
#:methods gen:custom-write
[(define (write-proc v port mode)
   (display (rosette-->-name v) port))]
#:property prop:rosette-contract
  (build-rosette-contract-property
   #:assert! rosette-->-assert!
   #:encode rosette-->-encode
   #:simplify rosette-->-simplify)
#:property C.prop:chaperone-contract
  (C.build-chaperone-contract-property
   #:name rosette-->-name
   #:first-order rosette-->-first-order
   #:late-neg-projection rosette-->-late-neg
   #:stronger rosette-->-stronger)
)

(define (make-rosette--> dom* cod default)
  (cond
   [(and (andmap rosette-flat-contract? dom*) ;; TODO rosette-contract?
         (rosette-flat-contract? cod))
    (rosette--> dom* cod)]
   [else
    #;(log-rosette-contract-debug "failed to (make-rosette--> ~e ~e)" dom* cod)
    default]))

;; -----------------------------------------------------------------------------

(define (solved-->-name ctc)
  (format "#<solved-->:~a:~a>"
          (solved-->-bitwidth ctc)
          (rosette-->-name ctc)))

(define (solved-->-late-neg ctc)
  (define dom* (rosette-->-dom* ctc))
  (define cod (rosette-->-cod ctc))
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
        (if (or (the-trivial-contract? cod) (cod y))
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

(struct solved--> rosette--> (
  bitwidth
)
#:property C.prop:chaperone-contract
  (C.build-chaperone-contract-property
   #:name solved-->-name
   #:first-order rosette-->-first-order
   #:late-neg-projection solved-->-late-neg
   #:stronger rosette-->-stronger)
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

(define (rosette-trivial-codomain? v dom* cod)
  (or (the-trivial-contract? cod)
      (and (not (ormap the-trivial-contract? dom*)) ;; if trivial, cannot generate inputs
           (let ([cod-P (rosette-flat-contract-predicate cod)]) ;; TODO should use method
             (and cod-P
                  (log-rosette-contract-debug "SOLVE trivial-codomain? ~a" (object-name v))
                  (no-counterexamples v
                    #:forall dom*
                    #:assume dom*
                    #:derive (λ (y) (R.not (cod-P y)))))))))

(define (rosette-impossible-codomain? v dom* cod)
  (and (not (the-trivial-contract? cod))
       (not (ormap the-trivial-contract? dom*)) ;; if trivial, cannot generate inputs
       (rosette-flat-contract? cod)
       (let ([cod-P (rosette-flat-contract-predicate cod)])
         (and cod-P
              (log-rosette-contract-debug "SOLVE impossible-codomain? ~a" (object-name v))
              (no-counterexamples v
                #:forall dom*
                #:assume dom*
                #:derive cod-P)))))

;; =============================================================================

(module+ test
  (require
    rackunit
    racket/string
    rosette-contract/private/env/flat)

  (define i->i (rosette--> (list integer?) integer?))
  (define p->i (rosette--> (list positive?) integer?))
  (define p->n (rosette--> (list positive?) negative?))

  (test-case "rosette-->-name"
    (let ([nm (rosette-->-name i->i)])
      (check-true (string-prefix? nm "#<rosette-->"))
      (check-true (string-contains? nm (C.contract-name integer?)))))

  (test-case "rosette-->-first-order"
    (let ([fo (rosette-->-first-order i->i)])
      (check-true (fo (λ (x) x)))
      (check-false (fo #f))
      (check-false (fo 2))
      (check-false (fo (λ (x y) x)))))

  (test-case "rosette-->-late-neg"
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

  (test-case "rosette-->-stronger"
    (check-true (rosette-->-stronger i->i i->i))
    (check-true (rosette-->-stronger p->i i->i))

    (check-false (rosette-->-stronger i->i p->i))
    (check-false (rosette-->-stronger p->i p->n))
  )

  (test-case "rosette-->simplify"
    (define srcloc (make-srcloc 'arrow.rkt 8 6 7 5))

    (let ([trivial-box
           (force/rc-log
             (λ ()
               (let ([ctc+ ((rosette-->-simplify i->i) (λ (x) x) srcloc)])
                 (check-equal? integer? (car (rosette-->-dom* ctc+)))
                 (check-equal? integer? (rosette-->-cod ctc+))
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
      (λ () ((rosette-->-simplify i->i) (λ (x) 'yo) srcloc)))

    (let ([impossible-box
           (force/rc-log
             (λ ()
               (check-equal? ((rosette-->-simplify i->i) (λ (x) (* 1 2 x)) srcloc) i->i)))])
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
