#lang racket/base

(require
  "command-types.rkt"
  "eval.rkt"
  racket/runtime-path)

;; =============================================================================

(define-runtime-path HISTORY "./history.txt")

(define (main)
  (call-with-input-file* (ann HISTORY Path-String)
    (lambda (p)
      (let-values ([(_e _s) (forth-eval* p)]) (void))))
  (void))

(time (main))
