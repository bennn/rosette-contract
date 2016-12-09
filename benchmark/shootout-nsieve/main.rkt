#lang racket/base
(require rosette-contract)

;; TODO fails to verify nsieve

(require racket/cmdline)

;; $Id: nsieve-mzscheme.code,v 1.6 2006/06/10 23:38:29 bfulgham Exp $
;; The Computer Language Benchmarks Game
;; http://shootout.alioth.debian.org/
;;
;; nsieve benchmark for The Computer Language Shootout
;; Written by Dima Dorfman, 2004
;; Converted to MzScheme by Brent Fulgham

(define/contract (nsieve m)
  (-> natural-number/c natural-number/c)
  (let ((a (make-vector m #t)))
    (let loop ((i 2) (n 0))
      (if (< i m)
          (if (vector-ref a i)
              (begin
                (let clear ((j (+ i i)))
                  (when (< j m)
                    (vector-set! a j #f)
                    (clear (+ j i))))
                (loop (+ 1 i) (+ 1 n)))
              (loop (+ 1 i) n))
          n))))

(define/contract (string-pad s len)
  (-> string? natural-number/c string?)
  (string-append (make-string (- len (string-length s)) #\space)
                 s))

(define/contract (test n)
  (-> natural-number/c string?)
  (let* ((m (* (expt 2 n) 10000))
         (count (nsieve m)))
    (format "Primes up to ~a ~a\n"
            (string-pad (number->string m) 8)
            (string-pad (number->string count) 8))))

(define/contract (main n)
  (-> natural-number/c string?)
  (when (>= n 0) (test n))
  (when (>= n 1) (test (- n 1)))
  (when (>= n 2) (test (- n 2))))

(time (begin (main 3) (void)))
