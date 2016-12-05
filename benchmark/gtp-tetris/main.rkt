#lang racket/base
(require rosette-contract)

(require "data.rkt"
         "aux.rkt"
         "bset.rkt"
         "world.rkt"
         racket/list
         racket/runtime-path
         racket/match)

(define (world0)
  (world (list-pick-random tetras) empty))

#;(: replay : World (Listof Any) -> Void)
(define (replay w0 hist)
  (for/fold ([w  w0])
            ([e hist])
    (match e
      [`(on-key ,(? string? ke)) (world-key-move w ke)]
      [`(on-tick) (next-world w)]
      [`(stop-when)
       (λ (w) (blocks-overflow? (world-blocks w))) ;; Unused in original code https://github.com/philnguyen/soft-contract/blob/master/benchmark-contract-overhead/tetris.rkt#L959
       w]))
  (void))


(define-runtime-path SMALL_TEST "./tetris-hist-small.rktd")
(define-runtime-path LARGE_TEST "./tetris-hist-large.rktd")

#;(: main (-> String Void))
(define (main filename)
  (define w0 (world0))
  (define raw (with-input-from-file filename read))
  (if (list? raw)
    (replay w0 (reverse raw))
    (error "bad input")))

(time (main SMALL_TEST))
;(time (main LARGE_TEST))
