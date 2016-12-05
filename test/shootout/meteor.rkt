#lang racket/base

;; TODO
;; - says `valid-xy?` cannot satisfy its contract (obviously it can)

;; -----------------------------------------------------------------------------

;; The Computer Language Benchmarks Game
;; http://shootout.alioth.debian.org/
;;
;; Based on a Python version:
;;   contributed by Olof Kraigher
;;    modified by Tupteq
;;   contributed by Matthew Flatt
;;   optimized by Eli Barzilay

(require racket/cmdline)
(require rosette-contract)
;(require racket/contract)

(define width 5)
(define height 10)
(define size (* width height))

(define nat? natural-number/c)

(define/contract (valid-xy? x y)
  (-> integer? integer? boolean?)
  (and (0 . <= . x)
       #;(x . < . width)
       #;(0 . <= . y)
       #;(y . < . height)))

;(define/contract (mover fun)
;  (-> (-> integer? integer? (values integer? integer?)) (vectorof integer?))
;  (let ([t (make-vector size)])
;    (define/contract (for-fun p)
;      (-> nat? void?)
;      (vector-set! t p (let*-values ([(y x) (quotient/remainder p width)]
;                                     [(x y) (fun x y)])
;                         (if (valid-xy? x y) (+ x (* y width)) -1))))
;    (for ([p (in-range size)])
;      (for-fun p))
;    t))
;
;(define E
;  (mover (lambda (x y) (values (add1 x) y))))
;(define W
;  (mover (lambda (x y) (values (sub1 x) y))))
;(define NE
;  (mover (lambda (x y) (values (+ x (bitwise-and y 1)) (sub1 y)))))
;(define NW
;  (mover (lambda (x y) (values (sub1 (+ x (bitwise-and y 1))) (sub1 y)))))
;(define SE
;  (mover (lambda (x y) (values (+ x (bitwise-and y 1)) (add1 y)))))
;(define SW
;  (mover (lambda (x y) (values (sub1 (+ x (bitwise-and y 1))) (add1 y)))))
;
;(define direction? (vectorof integer?))
;
;(define rotate-list (list E NE NW W SW SE E))
;(define/contract (rotate dir)
;  (-> vector? vector?)
;  (cadr (memq dir rotate-list)))
;
;(define flip-alist (list (cons E W) (cons NE NW) (cons NW NE)
;                         (cons W E) (cons SW SE) (cons SE SW)))
;(define/contract (flip dir) 
;  (-> vector? vector?)
;  (cdr (assq dir flip-alist)))
;
;(define movers (list E W NE NW SE SW))
;
;(define (valid? p)
;  (p . >= . 0))
;
;(define/contract (clear? board pos)
;  (-> integer? integer? boolean?)
;  (not (bitwise-bit-set? board pos)))
;(define/contract (set board pos)
;  (-> integer? integer? integer?)
;  (bitwise-ior board (arithmetic-shift 1 pos)))
;
;(define/contract (zero-count board)
;  (-> integer? integer?)
;  (for/fold ([count 0]) ([i (in-range size)])
;    (if (clear? board i) (add1 count) count)))
;
;(define/contract (find-free-cell board)
;  (-> integer? integer?)
;  (for/or ([p (in-range 0 size)])
;    (and (clear? board p) p)))
;
;(define/contract (flood-fill board p)
;  (-> integer? integer? integer?)
;  (for/fold ([board (set board p)]) ([mover (in-list movers)])
;    (let ([p (vector-ref mover p)])
;      (if (and (valid? p) (clear? board p))
;        (flood-fill board p)
;        board))))
;
;(define/contract (no-islands? mask)
;  (-> integer? boolean?)
;  (let ([zeros (zero-count mask)])
;    (define/contract (loop mask zeros)
;      (-> integer? integer? boolean?)
;           (if (= mask #x3FFFFFFFFFFFF)
;             #t
;             (let* ([p (find-free-cell mask)]
;                    [mask (flood-fill mask p)]
;                    [new-zeros (zero-count mask)])
;               (and ((- zeros new-zeros) . >= . 5)
;                    (loop mask new-zeros)))))
;    (and (zeros . >= . 5)
;         (loop mask zeros))))
;
;(define/contract (get-bitmask p piece)
;  (-> integer? (listof vector?) (or/c false/c integer?))
;  (let ([mask (arithmetic-shift 1 p)])
;    (define/contract (loop p cells mask)
;      (-> integer? (listof vector?) integer? (or/c false/c integer?))
;      (if (null? cells)
;        mask
;        (let ([p (vector-ref (car cells) p)])
;          (and (valid? p) (loop p (cdr cells) (set mask p))))))
;    (loop p piece mask)
;          ))
;
;(define/contract (all-bitmasks piece color)
;  (-> (listof vector?) integer? (listof integer?))
;  (let ([pieces
;         (let-values ([(accum piece)
;                       (for/fold ([accum null] [piece piece])
;                                 ([orientations (in-range 2)])
;                         (let-values ([(accum piece)
;                                       (for/fold ([accum accum] [piece piece])
;                                                 ([orientations (in-range (- 6 (* 3 (if (= color 4) 1 0))))])
;                                         (values (cons piece accum)
;                                                 (map rotate piece)))])
;                           (values accum (map flip piece))))])
;           accum)])
;    (reverse
;     (for*/fold ([accum null])
;                ([piece (in-list pieces)]
;                 [p (in-range 0 size)])
;       (let ([mask (get-bitmask p piece)])
;         (if (and mask (no-islands? mask)) (cons mask accum) accum))))))
;
;(define generate-bitmasks-pieces
;  (list (list E  E  E  SE)
;        (list SE SW W  SW)
;        (list W  W  SW SE)
;        (list E  E  SW SE)
;        (list NW W  NW SE SW)
;        (list E  E  NE W)
;        (list NW NE NE W)
;        (list NE SE E  NE)
;        (list SE SE E  SE)
;        (list E  NW NW NW)))
;(define/contract (generate-bitmasks)
;  (-> (vectorof (vectorof (listof integer?))))
;  (let ([masks-at-cell
;         (list->vector
;          (for/list ([i (in-range size)])
;            (list->vector (for/list ([j (in-range 10)]) null))))])
;    (for ([piece (in-list generate-bitmasks-pieces)]
;          [color (in-naturals)])
;      (let loop ([masks (sort (all-bitmasks piece color) >)]
;                 [cell-bit (sub1 size)]
;                 [cell-counter (sub1 size)])
;        (if (null? masks)
;          masks-at-cell
;          (if (bitwise-bit-set? (car masks) cell-bit)
;            (let ([vec (vector-ref masks-at-cell cell-counter)])
;              (vector-set! vec color (cons (car masks) (vector-ref vec color)))
;              (loop (cdr masks) cell-bit cell-counter))
;            (loop masks (sub1 cell-bit) (sub1 cell-counter))))))
;    (for ([v (in-vector masks-at-cell)])
;      (for ([j (in-naturals)]
;            [val (in-vector v)])
;        (vector-set! v j (reverse val))))
;    masks-at-cell))
;
;(define masks-at-cell (generate-bitmasks))
;
;(define masks (make-vector 10 0))
;(define to-go 0)
;(define solutions (mcons #f #f)) ; keeps (min max) solutions
;
;(define/contract (solve-cell! cell board)
;  (-> integer? integer? void?)
;  (when (and (positive? to-go) (not (negative? cell)))
;    ;; Need solutions and not off board
;    (cond [(= board #x3FFFFFFFFFFFF)
;           ;; Solved
;           (add-solutions!)]
;          [(not (clear? board cell))
;           ;; Cell full, so try next
;           (solve-cell! (sub1 cell) board)]
;          [else
;           ;; Recur
;           (for* ([color (in-range 10)]
;                  #:when (zero? (vector-ref masks color))
;                  [mask (in-list (vector-ref (vector-ref masks-at-cell cell)
;                                             color))]
;                  #:when (zero? (bitwise-and mask board)))
;             (vector-set! masks color mask)
;             (solve-cell! (sub1 cell) (bitwise-ior board mask))
;             (vector-set! masks color 0))])))
;
;(define/contract (add-solutions!)
;  (-> void?)
;  (define (add! solution)
;    (let ((head (mcar solutions)))
;      (cond [(not head)
;             (set-mcar! solutions solution)
;             (set-mcdr! solutions solution)]
;            [(bytes<? solution head)
;             (set-mcar! solutions solution)]
;            [(bytes>? solution (mcdr solutions))
;             (set-mcdr! solutions solution)])))
;  (let* ([s (list->bytes
;             (for/list ([pos (in-range size)])
;               (for/or ([color (in-range 10)])
;                       (and (not (clear? (vector-ref masks color) pos))
;                            (+ color (char->integer #\0))))))]
;         [ns (make-bytes size)])
;    ;; Inverse
;    (for* ([y (in-range height)]
;           [x (in-range width)])
;      (bytes-set! ns (+ x (* y width))
;                  (bytes-ref s (+ (- width (+ x 1))
;                                  (* width (- height (+ y 1)))))))
;    ;; Keep first and last only
;    (add! s)
;    (add! ns)
;    (set! to-go (- to-go 2))))
;
;(define/contract (print-solution solution)
;  (-> (vectorof integer?) void?)
;  (let ([solution (bytes->string/utf-8 solution)])
;    (for ([y (in-range height)])
;      (when (odd? y) (display " "))
;      (for ([x (in-range width)])
;        (printf "~a " (string-ref solution (+ x (* y width)))))
;      (printf "\n"))
;    (newline)))
;
;(define/contract (solve! n)
;  (-> nat? void?)
;  (set! to-go n)
;  (solve-cell! (sub1 size) 0))
;
;(define/contract (main n)
;  (-> string? void?)
;  (let ([n (string->number n)])
;    (solve! n)))
;    ;; (printf "~a solutions found\n\n" (- n to-go))
;    ;; (print-solution (mcar solutions))
;    ;; (print-solution (mcdr solutions)))
;
;(time (begin (main "200000") (void)))
