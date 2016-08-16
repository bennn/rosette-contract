#lang racket/base

(provide
  format-log
  ;; (-> String Syntax String)
  ;; Format a message to print for logging
)

;; =============================================================================

(define (format-log msg stx #:loc [maybe-loc #f])
  (define loc (or maybe-loc stx))
  (format "[LOG:~a:~a] ~a in ~a" (syntax-line loc) (syntax-column loc) msg (syntax->datum stx)))

