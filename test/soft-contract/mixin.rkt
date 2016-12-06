#lang racket/base
(require rosette-contract)

;; TODO test failing, not sure why

(define/contract (vec/c msg)
  (-> (one-of/c 'x 'y 'add) any/c)
  (case msg
   [(x y) real?]
   [(add) (-> vec/c vec/c)]))

(define/contract (ext-vec/c msg)
  (-> (one-of/c 'x 'y 'add 'len) any/c)
  (case msg
   [(x y) real?]
   [(add) (-> vec/c vec/c)]
   [(length) (and/c real? (>=/c 0))]))

(define/contract (extend mk-vec)
  (-> (-> real? real? vec/c) (-> real? real? ext-vec/c))
  (λ (x y)
    (let ([vec (mk-vec x y)])
      (λ (m)
        (case m
         [(len)
          (let ([x (vec 'x)] [y (vec 'y)])
            (sqrt (+ (* x x) (* y y))))]
         [else (vec m)])))))

;; -----------------------------------------------------------------------------

(module+ test
  (require rackunit)

  (define/contract zero-vector
    vec/c
    (λ (msg)
      (case msg
       [(x y) 0]
       [(add) (λ (other) other)])))

  (define/contract unit-vector
    vec/c
    (λ (msg)
      (case msg
       [(x y)
        1]
       [(add)
        (λ (other)
          (λ (msg)
            (case msg
             [(x y)
              (+ 1 (other msg))]
             [(add)
              (error 'lazy)])))])))

  (check-equal? (zero-vector 'x) 0)
  (check-equal? (zero-vector 'y) 0)
  (check-equal? (((zero-vector 'add) zero-vector) 'x) 0)

  (define zero-vector+ (extend (λ (x y) zero-vector)))

  (check-equal? (zero-vector+ 'x) 0)
  (check-equal? (zero-vector+ 'length) 0)

)
