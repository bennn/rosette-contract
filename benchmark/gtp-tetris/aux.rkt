#lang racket/base
(require rosette-contract)

(provide (contract-out
  [list-pick-random (-> (listof tetra?) tetra?)]
  [tetras (listof tetra?)]
  [neg-1 -1]
))

(require "data.rkt"
         "tetras.rkt")

(define r (make-pseudo-random-generator))
(parameterize ((current-pseudo-random-generator r))
  (random-seed 43453))

#;(: list-pick-random (-> (Listof Tetra) Tetra))
(define (list-pick-random ls)
  (list-ref ls (random (length ls) r)))

(define neg-1 -1)

(define tetras
  (list 
   (build-tetra-blocks 'green   1/2 -3/2    0 -1 0 -2 1 -1 1 -2)
   (build-tetra-blocks 'blue    1   -1      0 -1 1 -1 2 -1 3 -1)
   (build-tetra-blocks 'purple  1   -1      0 -1 1 -1 2 -1 2 -2)
   (build-tetra-blocks 'cyan    1   -1      0 -1 1 -1 2 -1 0 -2)
   (build-tetra-blocks 'orange  1   -1      0 -1 1 -1 2 -1 1 -2)
   (build-tetra-blocks 'red     1   -1      0 -1 1 -1 1 -2 2 -2)
   (build-tetra-blocks 'pink    1   -1      0 -2 1 -2 1 -1 2 -1)))

