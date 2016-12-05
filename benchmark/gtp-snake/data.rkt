#lang racket/base
(require rosette-contract)

(provide
  nelistof
  dir/c
  (contract-out
   (posn=?
    (-> posn? posn? boolean?))
   (posn
    (-> real? real? posn?))
   (posn-x
    (-> posn? real?))
   (posn-y
    (-> posn? real?))
   (posn?
    (-> any/c boolean?))
   (snake
    (-> dir/c (nelistof posn?) snake?))
   (snake-dir
    (-> snake? dir/c))
   (snake-segs
    (-> snake? (nelistof posn?)))
   (snake?
    (-> any/c boolean?))
   (world
    (-> snake? posn? world?))
   (world-snake
    (-> world? snake?))
   (world-food
    (-> world? posn?))
   (world?
    (-> any/c boolean?))
))

(struct snake (dir
               segs))
(struct world (snake
               food))

(struct posn (x
              y))

;(define-type Posn  posn)
;(define-type (NEListof A) (Pairof A (Listof A)))
;(define-type Snake snake)
;(define-type Dir (U "up" "down" "left" "right"))

(define (nelistof A) (cons/c A (listof A)))
(define dir/c (or/c "up" "down" "left" "right"))

#;(: posn=? (-> posn posn Boolean))
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))  

