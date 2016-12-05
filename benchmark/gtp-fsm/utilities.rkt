#lang racket/base
(require rosette-contract)

(provide (contract-out
 (choose-randomly
  (->* [[listof probability/c] natural-number/c] [#:random (or/c #f real?)] [listof natural-number/c]))
 (relative-average (-> [listof real?] real? real?))
))

(require racket/list)

;; Utility Functions

(define probability/c (>=/c 0))

;; constraint [0,1]

 #;(: sum (-> [Listof Real] Real))
 #;(: relative-average (-> [Listof Real] Real Real))
 #;(: choose-randomly
  (-> [Listof Probability] Natural [#:random (U False Real)] [Listof Natural]))

;; =============================================================================

(define (sum l)
  (apply + l))


(define (relative-average l w)
  (exact->inexact
   (/ (sum l)
      w (length l))))

;; -----------------------------------------------------------------------------

(define (choose-randomly probabilities speed #:random (q #false))
  (define %s (accumulated-%s probabilities))
  (for/list ([n (in-range speed)])
    [define r (or q (random))]
    ;; population is non-empty so there will be some i such that ...
    (let loop  ([%s  %s])
      (cond
        [(< r (first %s)) 0]
        [else (add1 (loop (rest %s)))]))
    #;
    (for/last ([p (in-naturals)] [% (in-list %s)] #:final (< r %)) p)))

#;(: accumulated-%s (-> [Listof Probability] [Listof Real]))
;; [Listof Probability] -> [Listof Probability]
;; calculate the accumulated probabilities 

(define (accumulated-%s probabilities)
  (define total (sum probabilities))
  (let relative->absolute
    ([payoffs  probabilities][so-far  #i0.0])
    (cond
      [(empty? payoffs) '()]
      [else (define nxt (+ so-far (first payoffs)))
            (cons #;{inst cons Real Real}
             (/ nxt total) (relative->absolute (rest payoffs) nxt))])))
