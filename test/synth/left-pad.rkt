#lang racket/base

;; TODO cannot verify/synthesize because Rosette cannot solve for strings

(require racket/contract
         (only-in racket/format ~a)
         (only-in racket/string string-prefix? string-suffix?)
         left-pad/private/left-pad)

(define (string-prefix/c v)
  (λ (s) (string-prefix? s v)))

(define (string-suffix/c v)
  (λ (s) (string-suffix? s v)))

(provide
  (contract-out
   [left-pad
    (->i ([value any/c]
          [pad-width (value) exact-positive-integer?])
         ([pad-value any/c])
         #:pre/name (value pad-width) "value wider than given pad-width"
         (<= (string-length (~a value)) pad-width)
         #:pre/name (pad-value) "pad-value wider than 1 character"
         (or (unsupplied-arg? pad-value)
             (= 1 (string-length (~a pad-value))))
         [padded-string (value pad-width pad-value) string?]
         #:post/name (pad-width padded-string) "padded-string must have pad-width characters"
         (= (string-length padded-string) pad-width)
         #:post/name (value padded-string) "padded-string must contain at least the original value"
         (string-suffix? padded-string (~a value))
         #:post/name (value pad-width pad-value padded-string) "padded-string must have the correct padding"
         (string-prefix? padded-string
                         (make-string (- pad-width (string-length (~a value)))
                                      (string-ref (if (unsupplied-arg? pad-value) " " (~a pad-value)) 0))))]))

(define (left-pad value pad-width [pad-value " "])
  (~a value #:min-width pad-width #:align 'right #:left-pad-string (~a pad-value)))
