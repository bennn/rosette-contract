#lang racket/base

(require
  rosette-contract
)

(module+ test
  (require rackunit))

;; =============================================================================

(define/contract (f x)
  (-> positive? negative?)
  (* x -1))

(module+ test
  (check-equal? (f 1) -1)
  (check-equal? (f 2) -2)
  (check-equal? (f 2.24) -2.24)
  (check-exn exn:fail:contract?
    (lambda () (f -1)))
)

;; -----------------------------------------------------------------------------

(define/contract (g x)
  (-> integer? negative?)
  (if (positive? x)
    (f x)
    (f 8)))

(module+ test
  (check-equal? (g 3) -3)
  (check-equal? (g -3) -8)
)

;; -----------------------------------------------------------------------------
;
;(define/contract (e2o f)
;  (-> (-> even? even?) (-> odd? odd?))
;  (λ (n) (- (f (+ n 1)) 1)))
;
;(define/contract (e2o-evil f)
;  (-> (-> even? even?) (-> odd? odd?))
;  (λ (n) (- (f (+ n 1)) 0)))
;
;;((e2o-evil (compose1 add1 add1)) 4) ;; ERROR rosette is blamed
;((e2o-evil (compose1 add1 add1)) 5) ;; FAIL produces 8, no blame
;
;;; -----------------------------------------------------------------------------
;
;(define/contract (flows x p)
;  (-> (or/c integer? string?) pair? integer?)
;  (cond
;   [(and (integer? x) (integer? (car p)))
;    (+ x (car p))]
;   [(integer? (car p))
;    (+ (string-length x) (car p))]
;   [else 0]))
;
;;(flows 'cotc '( 0 , 0 ))
;(flows 2 '( hai ho))
;(flows "silly" '( 6 ho))
;
;;; -----------------------------------------------------------------------------
;
;(let () ;; from Kobayashi et al. 2011
;  (define (f x g)
;    (g (+ x 1)))
;
;  (define/contract (h x y)
;    (->i [x integer?] [y (and/c integer? (>/c x))] [(x y) (and/c integer? (>/c y))])
;    '?) ;; hidden defn.
;
;  (define/contract (main n)
;    (-> integer? (and/c integer? (>/c 0)))
;    (if (>= n 0)
;      (f n (h n))
;      1))
;
;  ;; goal: verify main's contract
;)
