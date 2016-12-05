#lang racket/base

;; TODO
;; - doesn't verify anything (should it?)

(require (only-in mzlib/string real->decimal-string))
(require rosette-contract)
;(require racket/contract)

;;; http://shootout.alioth.debian.org/
;;; Random implementation, by Jens Axel Sogaard
;;;
;;; Modified for proper string output by Brent Fulgham

(define IM 139968)
(define IA   3877)
(define IC  29573)

(define gen_random
  (let ((LAST 42))
  (begin 
  (define/contract (lam max)
    (-> real? real?)
      (set! LAST (modulo (+ (* LAST IA) IC) IM))
      (/ (* max LAST) IM)))
  lam))

(define/contract (roundto digits num)
  (-> natural-number/c natural-number/c natural-number/c)
  (let* ([e (expt 10 digits)]
         [num (round (* e (inexact->exact num)))])
    (format "~a.~a"
            (quotient num e)
            (substring (string-append (number->string (remainder num e))
                                      (make-string digits #\0))
                       0 digits))))

(define/contract (main-loop iter)
  (-> natural-number/c boolean?)
    (if (> iter 0)
        (begin
          (gen_random 100.0)
          (main-loop (- iter 1)))
        #t))

(define/contract (main n)
  (-> natural-number/c string?)
  (main-loop n)
  (format "~a\n"
          (real->decimal-string (gen_random 100.0) 9)))

(time (begin (main 400 #;40000000) (void)))
