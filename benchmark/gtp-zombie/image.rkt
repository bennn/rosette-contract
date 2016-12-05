#lang racket/base
(require rosette-contract)

;; Placeholder for images.
;; Pretends to render data.

(provide
  (contract-out
   [image?
    (-> any/c boolean?)]
   [empty-scene
    (-> real? real? image?)]
   [place-image
    (-> image? real? real? image? image?)]
   [circle
    (-> real? string? string? image?)]
))

;; =============================================================================

(struct image (
 impl
))

#;(: empty-scene (-> Real Real Image))
(define (empty-scene w h)
  (when (or (negative? w) (negative? h))
    (error 'image "Arguments must be non-negative real numbers"))
  (image (cons w h)))

#;(: place-image (-> Image Real Real Image Image))
(define (place-image i1 w h i2)
  (image (list i1 w h i2)))

#;(: circle (-> Real String String Image))
(define (circle radius style color)
  (image (list radius style color)))
