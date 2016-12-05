#lang racket/base
(require rosette-contract)

;; TODO why does the rosette version run faster?

(define HI 0)
(define LO 0)

(define/contract (hi-excp? x)
  (-> symbol? boolean?)
  (eq? x 'Hi_Exception))

(define/contract (lo-excp? x)
  (-> symbol? boolean?)
  (eq? x 'Lo_Exception))

(define/contract (true? x)
  (-> any/c boolean?)
  (if (boolean? x) x #t))

(define/contract (some_fun n)
  (-> exact-nonnegative-integer? boolean?)
  (with-handlers
      ([true? (lambda (exn) #f)])
    (hi_fun n)))

(define/contract (hi_fun n)
  (-> none/c none/c)
  (with-handlers
      ([hi-excp? (lambda (exn) (set! HI (+ HI 1))) ])
    (lo_fun n)))

(define/contract (lo_fun n)
  (-> none/c none/c)
  (with-handlers
      ([lo-excp? (lambda (exn) (set! LO (+ LO 1))) ])
    (blowup n)))

(define/contract (blowup n)
  (-> exact-nonnegative-integer? none/c)
  (if (= 0 (modulo n 2))
      (raise 'Hi_Exception)
      (raise 'Lo_Exception)))

(define/contract (main args)
  (-> (vectorof string?) string?)
  (let* ((n (if (= (vector-length args) 1) (string->number (vector-ref args 0)) 1)))
    (do ((i 0 (+ i 1)))
        ((= i n))
      (some_fun i)))
  (format "Exceptions: HI=~a / LO=~a\n" HI LO))

(time (begin (main (vector "50" #;"2000000")) (void)))
