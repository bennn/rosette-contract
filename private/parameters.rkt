#lang racket/base

(provide (all-defined-out))

;; =============================================================================

(define-syntax-rule (define-parameters [id default-val] ...)
  (begin
    (define id (make-parameter default-val))
    ...))

(define-parameters
  [*solver-seconds-limit* 30]
  [*solver-mb-limit* 50]
)

;; -----------------------------------------------------------------------------
