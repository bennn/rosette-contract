#lang racket/base

(provide (all-defined-out))

;; =============================================================================

(define-logger rosette-contract)

(define (format-rcontract-info kind ctc stx)
  (format
    "[~a:~a] ~a~a in ~a"
    (syntax-line stx)
    (syntax-column stx)
    ctc
    kind
    (syntax->datum stx)))

(define (rcontract-info kind ctc stx)
  (log-rosette-contract-info (format-rcontract-info kind ctc stx)))

(define (rcontract- ctc stx)
  (rcontract-info "-" ctc stx))

(define (rcontract+ ctc stx)
  (rcontract-info "+" ctc stx))

(define (log-solver input result)
  (log-rosette-contract-info "SOLVE '~a' to '~a'" input result))

