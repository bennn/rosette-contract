#lang racket/base

;; TODO
;; - claims that `loop` (inside `count-swaps`) cannot satisfy its contract
;; - with the loop removed, the inner `define/contract` run the verifier on every call
;;   yuck!   ...   is there any way to cache this? (it's a new \lambda every time, :;)

;; fannkuch benchmark for The Computer Language Shootout
;; Written by Dima Dorfman, 2004
;; Slightly improved by Sven Hartrumpf, 2005-2006
;; Ever-so-slightly tweaked for MzScheme by Brent Fulgham
;; PLT-ized for v4.0 by Matthew

(require racket/cmdline
         rosette-contract
         #;racket/contract)

(define nat? natural-number/c)

(define/contract (fannkuch n)
  (-> nat? nat?)
  (let ([pi (list->vector
             (for/list ([i (in-range n)]) i))]
        [tmp (make-vector n)]
        [count (make-vector n)])
    (define/contract (loop1 flips perms r)
      (-> nat? nat? nat? nat?)
      (begin 
        (for ([i (in-range r)])
          (vector-set! count i (add1 i)))
        (define flips2 (max (count-flips pi tmp) flips))
        (define/contract (loop2 r)
         (-> nat? nat?)
          (if (= r n)
              flips2
              (let ((perm0 (vector-ref pi 0)))
                (for ([i (in-range r)])
                  (vector-set! pi i (vector-ref pi (add1 i))))
                (vector-set! pi r perm0)
                (vector-set! count r (sub1 (vector-ref count r)))
                (cond
                 [(<= (vector-ref count r) 0)
                  (loop2 (add1 r))]
                 [else (loop1 flips2 (add1 perms) r)]))))
          (loop2 1)))
     (loop1 0 0 n)))

(define/contract (count-flips pi rho)
  (-> (vectorof nat?) (vectorof nat?) nat?)
  (vector-copy! rho 0 pi)
  (define/contract (loop i) (-> nat? nat?)
    0
    (if (= (vector-ref rho 0) 0)
        i
        (begin
          (vector-reverse-slice! rho 0 (add1 (vector-ref rho 0)))
          (loop (add1 i)))))
  (loop 0))

(define/contract (vector-reverse-slice! v i j)
  (-> (vectorof nat?) nat? nat? void?)
  (let loop ([i i]
             [j (sub1 j)])
    (when (> j i)
      (vector-swap! v i j)
      (loop (add1 i) (sub1 j)))))

(define/contract (vector-swap! v i j)
  (-> (vectorof nat?) nat? nat? void?)
  (let ((t (vector-ref v i)))
    (vector-set! v i (vector-ref v j))
    (vector-set! v j t)))

(define/contract (main n)
  (-> nat? string?)
  (format "Pfannkuchen(~a) = ~a\n" 
   n
   (fannkuch n)))

(time (begin (main 10) (void)))
