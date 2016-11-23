#lang racket/base

(provide
  log-success
  ;; (->* [Contract Source-Location] [String] Void)
  ;; Log an info event that the given contract was somehow improved/simplified/reduced/pruned.
  ;; Optional argument tells the "what & why" of the success.

  log-failure
  ;; (->* [Contract Source-Location] [String] Void)
  ;; Log an info event that Rosette failed to learn anything about the given contract.
  ;; Optional argument tells the "what & why" of the failure.

  force/rc-log
  ;; (-> (-> any/c) (hash/c log-level/c (listof string?) #:immutable #t #:flat #t))
  ;; Execute the given thunk and collect all log messages to the rosette-contract logger.
  ;; Ignore the value returned by the thunk,
  ;;  instead return a hashtable mapping log levels to messages received at that level.

  ;; -- boring logger stuff

  rosette-contract-logger
  log-rosette-contract-info
  log-rosette-contract-warning
  log-rosette-contract-error
  log-rosette-contract-fatal
)

(require
  racket/logging
  syntax/srcloc
)

;; =============================================================================

(define-logger rosette-contract)

(define (format-rc-info kind ctc srcloc why)
  (format
    "[~a:~a]~a ~a in ~a"
    (source-location-line srcloc)
    (source-location-column srcloc)
    kind
    why
    ctc))

(define (log-rc kind ctc srcloc why)
  (log-rosette-contract-info (format-rc-info kind ctc srcloc why)))

(define (log-solver input result)
  (log-rosette-contract-info "SOLVE '~a' to '~a'" input result))

(define (log-success ctc srcloc [why ""])
  (log-rc 'SUCCESS ctc srcloc why))

(define (log-failure ctc srcloc [why ""])
  (log-rc 'FAILURE ctc srcloc why))

(define (force/rc-log thunk [level 'info])
  (define inbox (make-hasheq '((debug . ()) (info . ()) (warning . ()) (error . ()) (fatal . ()))))
  (with-intercepted-logging
    (λ (l)
      (define lvl (vector-ref l 0))
      (define msg (vector-ref l 1))
      (when (eq? 'rosette-contract (vector-ref l 3))
        (hash-set! inbox lvl (cons msg (hash-ref inbox lvl))))
      (void))
    thunk
    #:logger rosette-contract-logger
    level)
  ;; Return immutable hash with keys in [first recieved] -> [last received] order
  (for/hasheq ([(k v) (in-hash inbox)])
    (values k (reverse v))))

;; =============================================================================

(module+ test
  (require rackunit racket/string)

  (test-case "log-rc"
    (let* ([type "TYPE"]
           [ctc "CONTRACT"]
           [l 1]
           [c 2]
           [srcloc (make-srcloc 'here l c 3 4)]
           [why "reasons"]
           [msg (format-rc-info type ctc srcloc why)])
      (check-true (string-contains? msg (format "~a:~a" l c)))
      (check-true (string-contains? msg type))
      (check-true (string-contains? msg ctc))
      (check-true (string-contains? msg why))))

  (test-case "force/rc-log"
    (let* ([srcloc (make-srcloc 'testcase 1 800 529 1010)]
           [thunk (λ () (log-success "CTC1" srcloc)
                        (log-success "CTC2" srcloc))]
           [inbox (force/rc-log thunk)]
           [infos (hash-ref inbox 'info)])
      (check-equal? (length infos) 2)
      (check-true (null? (hash-ref inbox 'error)))
      (check-true (string-contains? (car infos) "CTC1"))
      (check-true (string-contains? (cadr infos) "CTC2"))
      (check-true (string-contains? (car infos) (number->string (source-location-line srcloc))))))
)
