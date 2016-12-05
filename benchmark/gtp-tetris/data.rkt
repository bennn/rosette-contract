#lang racket/base
(require rosette-contract)

(provide (contract-out
  [posn=? (-> posn? posn? boolean?)]
  [posn
   (-> real? real? posn?)]
  [posn?
   (-> any/c boolean?)]
  [posn-x
   (-> posn? real?)]
  [posn-y
   (-> posn? real?)]
  [block
   (-> real? real? symbol? block?)]
  [block?
   (-> any/c boolean?)]
  [block-x
   (-> block? real?)]
  [block-y
   (-> block? real?)]
  [block-color
   (-> block? symbol?)]
  [tetra
   (-> posn? (listof block?) tetra?)]
  [tetra?
   (-> any/c boolean?)]
  [tetra-center
   (-> tetra? posn?)]
  [tetra-blocks
   (-> tetra? (listof block?))]
  [world
   (-> tetra? (listof block?) world?)]
  [world?
   (-> any/c boolean?)]
  [world-tetra
   (-> world? tetra?)]
  [world-blocks
   (-> world? (listof block?))]
))

(struct posn (x
              y))
(struct block (x
               y
               color))
(struct tetra (center
               blocks))
(struct world (tetra
               blocks))

#;(: posn=? (-> posn posn Boolean))
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

