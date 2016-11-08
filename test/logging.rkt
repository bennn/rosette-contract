#lang racket/base

;; Check that the test modules generate expected log messages

(require
  rosette-contract/private/log
  racket/logging
  rackunit
  syntax/modcode
  (only-in racket/list add-between)
  (only-in racket/string string-join)
  (for-syntax
    racket/base
    syntax/parse))

(define-syntax (check-rosette-contract-logs stx)
  (syntax-parse stx
   [(_ module-name ?pattern*)
    (quasisyntax/loc stx
      (with-check-info* (list (make-check-location '#,(syntax->location stx)))
        (λ ()
          (define pattern* ?pattern*)
          (define inbox (intercept-rosette-contract-log (path->complete-path module-name)))
          (define log* (reverse (hash-ref inbox 'info)))
          (define len (length pattern*))
          (define num-log* (length log*))
          (define fail-msg
            (format "too ~a log messages:~n  ~s"
              (if (< num-log* len) "few" "many")
              (string-join (add-between log* " ~n  "))))
          (check-equal? num-log* len fail-msg)
          ;; (printf fail-msg) (newline) ;; --- for debugging
          (when (= num-log* len)
            (for ([log (in-list log*)]
                  [pat (in-list pattern*)])
              (check-true (regexp-match? pat log)
                (format "pattern '~a' does not match log message: ~a" pat log))))
           (void))))]))

(define (intercept-rosette-contract-log module-name)
  (define inbox (make-hasheq '((debug . ()) (info . ()) (warning . ()) (error . ()) (fatal . ()))))
  (with-intercepted-logging
    (λ (l)
      (define lvl (vector-ref l 0))
      (define msg (vector-ref l 1))
      (when (eq? 'rosette-contract (vector-ref l 3)) ;; filters out logs from racket/contract, etc
        (hash-set! inbox lvl (cons msg (hash-ref inbox lvl))))
      (void))
    (λ ()
      (eval (get-module-code module-name))
      (void))
    #:logger rosette-contract-logger
    'info)
  inbox)

(define-for-syntax (syntax->location stx)
  (list (syntax-source stx)
        (syntax-line stx)
        (syntax-column stx)
        (syntax-position stx)
        (syntax-span stx)))

;; =============================================================================

(module+ test

  (check-rosette-contract-logs
    "beyond-current-bitwidth.rkt"
    '(#rx"define/contract\\+"))

)
