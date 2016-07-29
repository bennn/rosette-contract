#lang racket/base

(provide
  (rename-out
   [C.-> ->])
  ;;; TODO

  define/contract
  ;; TODO doc
  ;; (It's a little heavy for a tailored-out, but that's the right idea

  ;contract-out
  ;;; TODO
)

(require
  (for-syntax racket/base syntax/parse)
  (prefix-in C. racket/contract)
  (prefix-in R. rosette)
)

(R.current-bitwidth #f)

;; =============================================================================

(define-syntax (define/contract stx)
  (syntax-parse stx #:literals (C.->)
    [(_ (nm:id var*:id ...)
       (C.-> pre* ... post)
       body* ...)
     (syntax/loc stx
       (define nm
         ;; Uncontracted version
         (letrec ([nm (λ (var* ...) body* ...)])
           (let ([ctc (if (R.unsat? (hoare {[var* pre*] ...} nm {post}))
                        (C.-> pre* ... C.any)
                        (C.-> pre* ... post))])
             (C.contract ctc nm 'rosette (C.current-contract-region))))))]))

(define-syntax (hoare stx)
  (syntax-parse stx
    [(_ {[var* pre*] ...} f {post}) ;; TODO generalize postcondition
     (syntax/loc stx
       (let ()
         (R.define-symbolic var* ... R.integer?)
         (R.solve (begin
           (R.assert pre* var*)
           ...
           (R.assert (not (post (f var* ...))))))))]))

