#lang racket/base
(require racket/contract)

(require
  "image.rkt"
  "zombie.rkt"
  racket/runtime-path
)

;(define-type World
;  (-> Symbol (U (Pairof 'on-mouse (-> Real Real String World))
;                (Pairof 'on-tick (-> World))
;                (Pairof 'to-draw (-> Image))
;                (Pairof 'stop-when (-> Boolean)))))

;; =============================================================================

#;(: replay (-> World (Listof Any) Void))
(define (replay w0 hist)
 (let loop ((w  w0)
            (h  hist))
  (cond
   [(null? h)
    (void)]
   [(not (list? (car h)))
    (error "input error")]
   [else
    (define m (caar h))
    (define as (cdar h))
    (case m
     ;; no rendering
     [(to-draw stop-when)
       (loop w (cdr h))]
     [(on-mouse)
      (define r (apply (world-on-mouse w) as #;(cast as (List Real Real String))))
      (loop r (cdr h))]
     [(on-tick)
      (define r ((world-on-tick w)))
      (loop r (cdr h))])])))

(define-runtime-path SMALL_TEST "./zombie-hist-small.rktd")
(define-runtime-path MICRO_TEST "./zombie-hist-micro.rktd")

#;(: main (-> Path-String Void))
(define (main filename)
  (define raw-hist (with-input-from-file filename read))
  (cond
   [(list? raw-hist)
    (define hist (reverse raw-hist))
    (for ([i  (in-range 100)])
      (replay w0 hist))]
   [else
    (error "bad input")]))

(time (main MICRO_TEST))
