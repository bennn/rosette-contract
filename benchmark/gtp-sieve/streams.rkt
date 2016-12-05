#lang racket/base

;; Simple streams library.
;; For building and using infinite lists.

(require rosette-contract)
;(require racket/contract)
(define nat? natural-number/c)

(provide
  (contract-out
   [stream-first (-> stream? nat?)]
   [stream-rest (-> stream? stream?)]
   [make-stream (-> nat? (-> stream?) stream?)]
   [stream-unfold (-> stream? (values nat? stream?))]
   [stream-get (-> stream? nat? nat?)]
   [stream-take (-> stream? nat? (listof nat?))]))


;; ;; A stream is a cons of a value and a thunk that computes the next value when applied
(struct stream (first rest))

;;--------------------------------------------------------------------------------------------------

#;(: make-stream (-> Natural (-> stream) stream))
(define (make-stream hd thunk)
  (stream hd thunk))

;; Destruct a stream into its first value and the new stream produced by de-thunking the tail
#;(: stream-unfold (-> stream (values Natural stream)))
(define (stream-unfold st)
  (values (stream-first st) ((stream-rest st))))

;; [stream-get st i] Get the [i]-th element from the stream [st]
#;(: stream-get (-> stream Natural Natural))
(define (stream-get st i)
  (define-values (hd tl) (stream-unfold st))
  (cond [(= i 0) hd]
        [else    (stream-get tl (sub1 i))]))

;; [stream-take st n] Collect the first [n] elements of the stream [st].
#;(: stream-take (-> stream Natural (Listof Natural)))
(define (stream-take st n)
  (cond [(= n 0) '()]
        [else (define-values (hd tl) (stream-unfold st))
              (cons hd (stream-take tl (sub1 n)))]))
