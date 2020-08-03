#lang htdp/isl+

(require 2htdp/image)
(require 2htdp/universe)
(require "shuffle.rkt")

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; - - - - - - - - - - - - - - S P R I T E   S P E C S - - - - - - - - - - - - -
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

; SpriteSpec is a [List-of [List-of ColorSymbol]]
; interpretation. encodes a sprite in the game.

(define HIDDEN
  '((W W W W W W W W W W W W W W W S) 
    (W W W W W W W W W W W W W W S G) 
    (W W S S S S S S S S S S S S G G) 
    (W W S S S S S S S S S S S S G G) 
    (W W S S S S S S S S S S S S G G) 
    (W W S S S S S S S S S S S S G G) 
    (W W S S S S S S S S S S S S G G) 
    (W W S S S S S S S S S S S S G G) 
    (W W S S S S S S S S S S S S G G) 
    (W W S S S S S S S S S S S S G G) 
    (W W S S S S S S S S S S S S G G) 
    (W W S S S S S S S S S S S S G G) 
    (W W S S S S S S S S S S S S G G) 
    (W W S S S S S S S S S S S S G G) 
    (W S G G G G G G G G G G G G G G) 
    (S G G G G G G G G G G G G G G G)))

(define MINE
  '((G G G G G G G G G G G G G G G G) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S D S S S S S S S) 
    (G S S S S S S S D S S S S S S S) 
    (G S S S D S D D D D D S D S S S) 
    (G S S S S D D D D D D D S S S S) 
    (G S S S D D W W D D D D D S S S) 
    (G S S S D D W W D D D D D S S S) 
    (G S D D D D D D D D D D D D D S) 
    (G S S S D D D D D D D D D S S S) 
    (G S S S D D D D D D D D D S S S) 
    (G S S S S D D D D D D D S S S S) 
    (G S S S D S D D D D D S D S S S) 
    (G S S S S S S S D S S S S S S S) 
    (G S S S S S S S D S S S S S S S) 
    (G S S S S S S S S S S S S S S S)))

(define FLAG
  '((W W W W W W W W W W W W W W W S) 
    (W W W W W W W W W W W W W W S G) 
    (W W S S S S S S S S S S S S G G) 
    (W W S S S S S R R S S S S S G G) 
    (W W S S S R R R R S S S S S G G) 
    (W W S S R R R R R S S S S S G G) 
    (W W S S S R R R R S S S S S G G) 
    (W W S S S S S R R S S S S S G G) 
    (W W S S S S S S D S S S S S G G) 
    (W W S S S S S S D S S S S S G G) 
    (W W S S S S D D D D S S S S G G) 
    (W W S S D D D D D D D D S S G G) 
    (W W S S D D D D D D D D S S G G) 
    (W W S S S S S S S S S S S S G G) 
    (W S G G G G G G G G G G G G G G) 
    (S G G G G G G G G G G G G G G G)))

(define EMINE
  '((G G G G G G G G G G G G G G G G) 
    (G R R R R R R R R R R R R R R R) 
    (G R R R R R R R D R R R R R R R) 
    (G R R R R R R R D R R R R R R R) 
    (G R R R D R D D D D D R D R R R) 
    (G R R R R D D D D D D D R R R R) 
    (G R R R D D W W D D D D D R R R) 
    (G R R R D D W W D D D D D R R R) 
    (G R D D D D D D D D D D D D D R) 
    (G R R R D D D D D D D D D R R R) 
    (G R R R D D D D D D D D D R R R) 
    (G R R R R D D D D D D D R R R R) 
    (G R R R D R D D D D D R D R R R) 
    (G R R R R R R R D R R R R R R R) 
    (G R R R R R R R D R R R R R R R) 
    (G R R R R R R R R R R R R R R R)))

(define CMINE
  '((G G G G G G G G G G G G G G G G) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S D S S S S S S S) 
    (G S R R S S S S D S S S S R R S) 
    (G S S R R S D D D D D S R R S S) 
    (G S S S R R D D D D D R R S S S) 
    (G S S S D R R W D D R R D S S S) 
    (G S S S D D R R D R R D D S S S) 
    (G S D D D D D R R R D D D D D S) 
    (G S S S D D D R R R D D D S S S) 
    (G S S S D D R R D R R D D S S S) 
    (G S S S S R R D D D R R S S S S) 
    (G S S S R R D D D D D R R S S S) 
    (G S S R R S S S D S S S R R S S) 
    (G S R R S S S S D S S S S R R S) 
    (G S S S S S S S S S S S S S S S)))

(define ZERO
  '((G G G G G G G G G G G G G G G G) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S S S S S S S S S)))

(define ONE
  '((G G G G G G G G G G G G G G G G) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S B B S S S S S S) 
    (G S S S S S S B B B S S S S S S) 
    (G S S S S S B B B B S S S S S S) 
    (G S S S S B B B B B S S S S S S) 
    (G S S S S S S B B B S S S S S S) 
    (G S S S S S S B B B S S S S S S) 
    (G S S S S S S B B B S S S S S S) 
    (G S S S S S S B B B S S S S S S) 
    (G S S S S B B B B B B B S S S S) 
    (G S S S S B B B B B B B S S S S) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S S S S S S S S S)))

(define TWO
  '((G G G G G G G G G G G G G G G G) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S C C C C C C C C S S S S) 
    (G S S C C C C C C C C C C S S S) 
    (G S S C C C S S S S C C C S S S) 
    (G S S S S S S S S S C C C S S S) 
    (G S S S S S S S C C C C S S S S) 
    (G S S S S S C C C C C S S S S S) 
    (G S S S C C C C C S S S S S S S) 
    (G S S C C C C S S S S S S S S S) 
    (G S S C C C C C C C C C C S S S) 
    (G S S C C C C C C C C C C S S S) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S S S S S S S S S)))

(define THREE
  '((G G G G G G G G G G G G G G G G) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S S S S S S S S S) 
    (G S S R R R R R R R R R S S S S) 
    (G S S R R R R R R R R R R S S S) 
    (G S S S S S S S S S R R R S S S) 
    (G S S S S S S S S S R R R S S S) 
    (G S S S S S R R R R R R S S S S) 
    (G S S S S S R R R R R R S S S S) 
    (G S S S S S S S S S R R R S S S) 
    (G S S S S S S S S S R R R S S S) 
    (G S S R R R R R R R R R R S S S) 
    (G S S R R R R R R R R R S S S S) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S S S S S S S S S)))

(define FOUR
  '((G G G G G G G G G G G G G G G G) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S N N N S N N N S S S S) 
    (G S S S S N N N S N N N S S S S) 
    (G S S S N N N S S N N N S S S S) 
    (G S S S N N N S S N N N S S S S) 
    (G S S N N N N N N N N N N S S S) 
    (G S S N N N N N N N N N N S S S) 
    (G S S S S S S S S N N N S S S S) 
    (G S S S S S S S S N N N S S S S) 
    (G S S S S S S S S N N N S S S S) 
    (G S S S S S S S S N N N S S S S) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S S S S S S S S S)))

(define FIVE
  '((G G G G G G G G G G G G G G G G) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S S S S S S S S S) 
    (G S S M M M M M M M M M M S S S) 
    (G S S M M M M M M M M M M S S S) 
    (G S S M M M S S S S S S S S S S) 
    (G S S M M M S S S S S S S S S S) 
    (G S S M M M M M M M M M S S S S) 
    (G S S M M M M M M M M M M S S S) 
    (G S S S S S S S S S M M M S S S) 
    (G S S S S S S S S S M M M S S S) 
    (G S S M M M M M M M M M M S S S) 
    (G S S M M M M M M M M M S S S S) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S S S S S S S S S)))

(define SIX
  '((G G G G G G G G G G G G G G G G) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S F F F F F F F F S S S S) 
    (G S S F F F F F F F F F S S S S) 
    (G S S F F F S S S S S S S S S S) 
    (G S S F F F S S S S S S S S S S) 
    (G S S F F F F F F F F F S S S S) 
    (G S S F F F F F F F F F F S S S) 
    (G S S F F F S S S S F F F S S S) 
    (G S S F F F S S S S F F F S S S) 
    (G S S F F F F F F F F F F S S S) 
    (G S S S F F F F F F F F S S S S) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S S S S S S S S S)))

(define SEVEN
  '((G G G G G G G G G G G G G G G G) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S S S S S S S S S) 
    (G S S D D D D D D D D D D S S S) 
    (G S S D D D D D D D D D D S S S) 
    (G S S S S S S S S S D D D S S S) 
    (G S S S S S S S S S D D D S S S) 
    (G S S S S S S S S D D D S S S S) 
    (G S S S S S S S S D D D S S S S) 
    (G S S S S S S S D D D S S S S S) 
    (G S S S S S S S D D D S S S S S) 
    (G S S S S S S D D D S S S S S S) 
    (G S S S S S S D D D S S S S S S) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S S S S S S S S S)))

(define EIGHT
  '((G G G G G G G G G G G G G G G G) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S G G G G G G G G S S S S) 
    (G S S G G G G G G G G G G S S S) 
    (G S S G G G S S S S G G G S S S) 
    (G S S G G G S S S S G G G S S S) 
    (G S S S G G G G G G G G S S S S) 
    (G S S S G G G G G G G G S S S S) 
    (G S S G G G S S S S G G G S S S) 
    (G S S G G G S S S S G G G S S S) 
    (G S S G G G G G G G G G G S S S) 
    (G S S S G G G G G G G G S S S S) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S S S S S S S S S) 
    (G S S S S S S S S S S S S S S S)))

; ColorSymbol and Color are as specified:

(define DARK   (color 000 000 000 255))
(define NAVY   (color 000 000 123 255))
(define GREEN  (color 000 123 000 255))
(define MAROON (color 123 000 000 255))
(define BLUE   (color 000 000 255 255))
(define RED    (color 255 000 000 255))
(define WHITE  (color 255 255 255 255))
(define SILVER (color 189 189 189 255))
(define GREY   (color 123 123 123 255))
(define SURFIE (color 000 123 123 255))

; [List-of (list ColorSymbol COLOR))
(define COLOR-ASSOC
  `((W ,WHITE) (S ,SILVER) (C ,GREEN)  (M ,MAROON) (F ,SURFIE)
               (B ,BLUE) (R ,RED) (D ,DARK) (N ,NAVY) (G ,GREY)))

; interpretation. color symbols are a part of sprite specs
; while colors are used to build actual sprite images

; ColorSymbol -> Color
; gets the color associated with the color symbol
(define (color-symbol->color color-symbol)
  (second (assoc color-symbol COLOR-ASSOC)))

; SpriteSpec Number -> Image
; renders a sprite spec and scales each color symbol is sym size
(define (sprite-spec->image sprite-spec)
  (2dfold above beside empty-image empty-image
          (2dmap (λ (s) (square 1 "solid" (color-symbol->color s)))
                 sprite-spec)))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; - - - - - - - - - - - G E N E R I C   H E L P E R S - - - - - - - - - - - - -
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

; [X] Number [List-of X] -> [List-of X]
; list with the first n elements of l
(define (take n l)
  (if (or (zero? n) (empty? l)) empty (cons (first l) (take (sub1 n) (rest l)))))

; [X] Number [List-of X] -> [List-of X]
; list without the first n elements of l
(define (drop n l)
  (if (or (zero? n) (empty? l)) l (drop (sub1 n) (rest l))))

; [X] Number [List-of X] -> [List-of [List-of X]]
; chunk the list up into sublists of of length upto n
(define (split n l)
  (if (empty? l) empty (cons (take n l) (split n (drop n l)))))

; [X] [List-of X] [List-of X] -> [List-of X]
; remove all elements of l1 from l2
(define (remove-all-in l1 l2)
  (cond
    [(empty? l1) l2]
    [else (remove-all-in (rest l1) (remove-all (first l1) l2))]))

; [List-of Number] [List-of Number] -> [List-of (make-posn Number Number]]
; cartesian product of two lists 
(define (cartesian-product xs ys)
  (apply append (map (lambda (x) (map (lambda (y) (make-posn x y)) ys)) xs)))


; [X] [List-of X] -> [List-of X]
; remove duplicates from the list using equality predicate e
(define (remove-duplicates lon e)
  (foldr (lambda (x y) (cons x (filter (lambda (z) (not (e x z))) y))) empty lon))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; - - - - - - - A B S T R A C T I O N S   O N   2 D   L I S T S - - - - - - - -
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

; (: 2dfold  : (All (X Y Z) (Y Z -> Z) (X Y -> Y) Y (Listof (Listof X)) -> Z))
(define (2dfold f1 f2 b1 b2 2dl)
  (foldr (λ (r f1-r) (f1 (foldr (λ (c f2-r) (f2 c f2-r)) b2 r) f1-r)) b1 2dl))

; (: 2dmap   : (All (X) (X -> Y) (Listof X) -> (Listof X)))
(define (2dmap f 2dl)
  (foldr (λ (r ra) (cons (foldr (λ (c ca) (cons (f c) ca)) empty r) ra)) empty 2dl))

; (: 2dbuild : (All (X) Number (Number Number -> X) -> (Listof X)))
(define (2dbuild xmax ymax f)
  (build-list ymax (λ (y) (build-list xmax (λ (x) (f x y))))))

; (: 2dget   : (All (X) Number Number (Listof (Listof X)) -> X))
(define (2dget x y 2dl)
  (list-ref (list-ref 2dl y) x))

; (: 2dset   : (All (X) X (Listof (Listof X)) -> (Listof (Listof X))))
(define (2dset xc yc e 2dl)
  (2dbuild (length (first 2dl)) (length 2dl) (λ (x y) (if (and (= xc x) (= yc y)) e (2dget x y 2dl)))))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; - - - - - - - - - - - - D A T A   D E F I N I T I O N - - - - - - - - - - - - 
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

; CellType is one of
; - [0, 9)
; - 'mine

(define-struct cell [pos neighbors celltype hidden? flagged?])
; Cell is a structure
;  (make-cell Posn [List-of Posn] CellType Boolean Boolean)
; interpretation. represents one cell on the board. its position,
; position of neighbors, type of cell, is it hidden, is it flagged?

; Board is a [List-of [List-of Cell]]

(define-struct ms [board flagon? gameover?])
; Minesweeper (MS) is a structure
;  (make-ms Board Boolean)
; interpretation. represents the board
; and whether the flag mode is on. 

; The WorldState is MS

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; - - - - - - - - - - - - - - - C O N S T A N T S - - - - - - - - - - - - - - -
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define BOARD-ROWS 9)
(define BOARD-COLS 9)
(define BOARD-WIDTH  (* 16 BOARD-COLS))
(define BOARD-HEIGHT (* 16 BOARD-ROWS))
(define BOARD-BACK (rectangle BOARD-WIDTH BOARD-HEIGHT "solid" "green"))
(define MINES 10)

; - - - -  S P R I T E S   I M A G E S  - - - -
(define HIDDEN-img (sprite-spec->image HIDDEN))
(define MINE-img (sprite-spec->image MINE))
(define FLAG-img (sprite-spec->image FLAG))
(define EMINE-img (sprite-spec->image EMINE))
(define CMINE-img (sprite-spec->image CMINE))
(define ZERO-img (sprite-spec->image ZERO))
(define ONE-img (sprite-spec->image ONE))
(define TWO-img (sprite-spec->image TWO))
(define THREE-img (sprite-spec->image THREE))
(define FOUR-img (sprite-spec->image FOUR))
(define FIVE-img (sprite-spec->image FIVE))
(define SIX-img (sprite-spec->image SIX))
(define SEVEN-img (sprite-spec->image SEVEN))
(define EIGHT-img (sprite-spec->image EIGHT))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; - - - - - - - - - - - - - - - N E I G H B O R S - - - - - - - - - - - - - - -
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

; Posn Posn -> Boolean
; is p1 the same as p2?
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

; Posn -> [List-of Posn]
; get the neighbors of p
(define (get-neighbors p)
  (local ((define x (posn-x p)) (define y (posn-y p)))
    (filter (λ (n) (and (> BOARD-COLS (posn-x n) -1) (> BOARD-ROWS (posn-y n) -1) (not (posn=? p n)))) 
            (cartesian-product (list (- x 1) x (+ x 1)) (list (- y 1) y (+ y 1))))))

; Cell [List-of Posn] -> Cell
; set the neighbors of c to neighbors
(define (set-neighbors c neighbors)
  (make-cell (cell-pos c) neighbors (cell-celltype c) (cell-hidden? c) (cell-flagged? c)))

; Board -> Board
; add neighbors to board
(define (add-neighbors board)
  (2dmap (λ (c) (set-neighbors c (get-neighbors (cell-pos c)))) board))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; - - - - - - - - - - - - - - - - - M I N E S - - - - - - - - - - - - - - - - -
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

; Board Posn -> Board
; adds mines to board where the first click was on p
(define (add-mines board p)
  (local ((define mines (gen-mines (map cell-pos board) p MINES)))
    (update-numberings (put-mines board mines) mines)))

; [List-of Posn] Posn Number -> [List-of Posn]
; get mine placements among lop that are not p or its neighbors
(define (gen-mines lop p mines)
  (take mines (shuffle (remove-all-in (cons p (get-neighbors p)) lop))))

; Board [List-of Posn] -> Board
; putmines on board according to mines
(define (put-mines board mines)
  (map (λ (m) (local ((define x (posn-x m)) (define y (posn-y m)))
                (2dset x y (put-mine (2dget x y board)) board)))
       mines))

; Cell -> Cell
; c with cell type as 'mine
(define (put-mine c)
  (make-cell (cell-pos c) (cell-neighbors c) 'mine (cell-hidden? c) (cell-flagged? c)))

; Board -> Board
; increment all neigbhors of all mines 
(define (update-numberings board mines)
  (map (λ (mine) (increment-neighbors (2dget (posn-x mine) (posn-y mine) board) board)) mines))

; Cell Board -> Board
; increment all neighbors of c
(define (increment-neighbors c board) 
  (2dmap (λ (n) (2dset (posn-x n) (posn-y n) n board))
         (map increment-number (cell-neighbors c))))

; Cell -> Cell
; add number of cell if it's not a mine
(define (increment-number c)
  (make-cell (cell-pos c) (cell-neighbors c)
             (if (symbol? (cell-celltype c)) 'mine (add1 (cell-celltype c)))
             (cell-hidden? c) (cell-flagged? c)))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; - - - - - - - - - - - - - - - I N I T I A L - - - - - - - - - - - - - - - - -
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

; Number Number -> [List-of [List-of Posn]]
; posns, as coordinates of a cols x rows grid
(define (2d-posn cols rows)
  (2dbuild cols rows make-posn))

; Number Number -> Board
; make the initial board cols wide and rows long
(define (initial-board c r)
  (local ((define board (2dmap (λ (p) (make-cell p empty 0 true false)) (2d-posn c r))))
    (add-neighbors board)))

; Number Number -> MS
; initial minesweeper 
(define (initial-ms c r)
  (make-ms (initial-board c r) false false))





; WS -> Image
; the last scene of the game
(define (last-scene ms)
  (text "game over" 20 "red"))

; MS -> MS
; toggle the flag 
(define (toggle-flag ms)
  (make-ms (ms-board ms) (not (ms-flagon? ms)) (ms-gameover? ms)))

; MS -> MS
; end the game
(define (end-game ms)
  (make-ms (ms-board ms) (not (ms-flagon? ms)) true))

; Cell -> Cell
(define (reveal-this-cell c)
  (make-cell (cell-pos c) (cell-neighbors c) (cell-celltype c) false (cell-flagged? c)))

; Board -> Board 
(define (reveal-cell x y board)
  (2dset x y (reveal-this-cell (2dget x y board)) board))


(define (scale-mouse->cell p)
  (inexact->exact (floor (/ p 16))))


; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; - - - - - - - - - - - - - - - - R E N D E R - - - - - - - - - - - - - - - - -
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

; Boaed -> Image
; renders the board 
(define (render-board board)
  (2dfold above beside empty-image empty-image (2dmap render-cell board)))

; Cell -> Image
; renders a single cell
(define (render-cell c)
  (cond [(cell-hidden? c) HIDDEN-img]
        [(cell-flagged? c) FLAG-img]
        [(equal? (cell-celltype c) 'mine) MINE-img]
        [(equal? (cell-celltype c) 0) ZERO-img]
        [(equal? (cell-celltype c) 1) ONE-img]
        [(equal? (cell-celltype c) 2) TWO-img]
        [(equal? (cell-celltype c) 3) THREE-img]
        [(equal? (cell-celltype c) 4) FOUR-img]
        [(equal? (cell-celltype c) 5) FIVE-img]
        [(equal? (cell-celltype c) 6) SIX-img]
        [(equal? (cell-celltype c) 7) SEVEN-img]
        [(equal? (cell-celltype c) 8) EIGHT-img]))


; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; - - - - - - - - - - - - - - - H A N D L E R S - - - - - - - - - - - - - - - -
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

; MS -> Image
(define (render ms)
  (render-board (ms-board ms)))

; MS Number Number MouseEvent -> MS
(define (me-h ms x y me)
  (cond [(mouse=? me "button-down") (make-ms (reveal-cell (scale-mouse->cell x) (scale-mouse->cell y) (ms-board ms)) (ms-flagon? ms) (ms-gameover? ms))]
        [else ms]))

; MS -> Boolean
(define (end? ms)
  (ms-gameover? ms))

; MS KeyEvent -> MS
(define (key-h ms ke)
  (cond [(key=? ke " ") (toggle-flag ms)]
        [(or (key=? ke "escape") (key=? ke "q")) (end-game ms)]
        [else ms]))

; Number Number -> "Bye"
(define (main r c)
  ((λ (ws) "Bye")
   (big-bang (initial-ms r c)
     [to-draw render]
     [on-mouse me-h]
     [on-key key-h]
     [stop-when end? last-scene]
     [check-with ms?])))