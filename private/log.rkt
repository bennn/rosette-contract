#lang racket/base

(provide
  format-log
  ;; (-> String Syntax String)
  ;; Format a message to print for logging
)

;; =============================================================================

(define (format-log msg stx)
  (format "[LOG:~a:~a] ~a in ~a" (syntax-line stx) (syntax-column stx) msg (syntax->datum stx)))

