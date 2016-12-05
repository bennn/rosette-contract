#lang racket/base

(require "data.rkt"
         "const.rkt"
         "motion.rkt"
         "handlers.rkt"
         racket/list
         racket/match
         racket/runtime-path)

#;(: replay : World (Listof Any) -> Void)
(define (replay w0 hist)
  (reset!)
  (let loop ((w  w0)
             (h  hist))
    (if (empty? h)
        w
        (let ()
          (loop
           (match (car h)
             [`(on-key ,(? string? ke))
              (handle-key w ke)]
             [`(on-tick)
              (world->world w)]
             [`(stop-when)
              (game-over? w)
              w])
           (cdr h)))))
  (void))

(define-runtime-path SMALL_TEST "./snake-hist-small.rktd")
(define-runtime-path LARGE_TEST "./snake-hist-large.rktd")

#;(: main (-> String Void))
(define (main filename)
  (define w0 (WORLD))
  (define raw-hist (with-input-from-file filename read))
  (cond [(list? raw-hist)
         (define hist (reverse raw-hist))
         (for ([i (in-range 100)])
           (replay w0 hist))]
        [else
         (error "bad input")]))

(time (main SMALL_TEST)) ; 66ms
#;(time (main LARGE_TEST)) ; 340ms
