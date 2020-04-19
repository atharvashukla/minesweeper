#lang racket

(require 2htdp/universe)
(require 2htdp/image)

(provide (all-defined-out))


; Constants

(define CELL-SIZE 32)
(define MINES 10)

; Sprites

(define ZERO (scale 2 (bitmap "./sprites/0.png")))
(define ONE (scale 2 (bitmap "./sprites/1.png")))
(define TWO (scale 2 (bitmap "./sprites/2.png")))
(define THREE (scale 2 (bitmap "./sprites/3.png")))
(define FOUR (scale 2 (bitmap "./sprites/4.png")))
(define FIVE (scale 2 (bitmap "./sprites/5.png")))
(define SIX (scale 2 (bitmap "./sprites/6.png")))
(define SEVEN (scale 2 (bitmap "./sprites/7.png")))
(define EIGHT (scale 2 (bitmap "./sprites/8.png")))
(define CMINE (scale 2 (bitmap "./sprites/cmine.png")))
(define EMINE (scale 2 (bitmap "./sprites/emine.png")))
(define FLAG (scale 2 (bitmap "./sprites/flag.png")))
(define HIDDEN (scale 2 (bitmap "./sprites/hidden.png")))
(define MINE (scale 2 (bitmap "./sprites/mine.png")))

; - - - - - - - - - - D A T A   D E F I N I T I O N S - - - - - - - - - - 

(struct posn [x y] #:transparent)
; Posn is a structure
;  (posn Number Number)
; Interpretation:
; - x: The x-coordinate of a cartesian plane.
; - y: The y-coordinate of a cartesian plane.

; CellType is one of
; - 'mine
; - [0, 9)


(struct cell [pos neighbors celltype hidden? flagged?] #:transparent)
; Cell is a structure
;  (cell Posn [List-of Posn] CellType Boolean Boolean Boolean)
; Interpretation:
; - pos: The position of this cell.
; - neighbors: The list of positions of the neighbors of this cell.
; - celltype: The type of the cell.
; - hidden?: Is this cell hidden?
; - flagged?: Is this cell flagged?

(struct ms [board flagon? firstclick? gameover?] #:transparent)
; Minesweeper (MS) is a structure
;  (ms [List-of [List-of Cell]] Boolean Boolean Boolean)
; Interpretation:
; - board: The two dimensional array of the cells of the board.
; - flagon?: Is the flag mode on?
; - firstclick?: True before the first click. 
; - gameover?: Is the game over?


(define (set-cell-neighbors c neighbors)
  (cell (cell-pos c) neighbors (cell-celltype c) (cell-hidden? c) (cell-flagged? c)))

(define (set-cell-celltype c celltype)
  (cell (cell-pos c) (cell-neighbors c) celltype (cell-hidden? c) (cell-flagged? c)))

(define (set-cell-hidden c hidden?)
  (cell (cell-pos c) (cell-neighbors c) (cell-celltype c) hidden? (cell-flagged? c)))

(define (set-cell-flagged c flagged?)
  (cell (cell-pos c) (cell-neighbors c) (cell-celltype c) (cell-hidden? c) flagged?))

(define (set-ms-board minesweeper board)
  (ms board (ms-flagon? minesweeper) (ms-firstclick? minesweeper) (ms-gameover? minesweeper)))

(define (set-ms-flagon minesweeper flagon?)
  (ms (ms-board minesweeper) flagon? (ms-firstclick? minesweeper) (ms-gameover? minesweeper)))

(define (set-ms-firstclick minesweeper firstclick?)
  (ms (ms-board minesweeper) (ms-flagon? minesweeper) firstclick? (ms-gameover? minesweeper)))

(define (set-ms-gameover minesweeper gameover?)
  (ms (ms-board minesweeper) (ms-flagon? minesweeper) (ms-firstclick? minesweeper) gameover?))


; Number Number -> Cell
(define (initial-cell x y)
  (cell (posn x y) empty 0 true false))

; [X] [Vector-of [Vector-of X]] Number Number -> X
(define (get-cell-from-board board x y)
  (vector-ref (vector-ref board y) x))

; [X] [Vector-of [Vector-of X]] Number Number X -> Void
(define (set-cell-in-board board x y cell)
  (define row  (vector-ref board y))
  (define elem (vector-ref row x))
  (vector-set! row x cell)
  (vector-set! board y row)
  board)

; Number Number -> [Vector-of [Vector-of Cell]]
(define (initial-board x-max y-max)
  (build-vector y-max (λ (y) (build-vector x-max (λ (x) (initial-cell x y))))))

; Posn Posn -> Boolean
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

; [List-of Number] [List-of Number] -> [List-of (posn Number Number]]
(define (cartesian-product xs ys)
  (apply append (map (lambda (x) (map (lambda (y) (posn x y)) ys)) xs)))

; Posn -> [List-of Posn]
(define (get-neighbors y-max x-max pos)
  (local ((define x (posn-x pos)) (define y (posn-y pos)))
    (filter (λ (n) (and (> x-max (posn-x n) -1) (> y-max (posn-y n) -1) (not (posn=? pos n)))) 
            (cartesian-product (list (- x 1) x (+ x 1)) (list (- y 1) y (+ y 1))))))

; Number Number Board -> Board
(define (add-neighbors y-max x-max board)
  (for/vector ([y y-max])
    (for/vector ([x x-max])
      (let* ([current-cell (get-cell-from-board board x y)]
             [neighbors (get-neighbors y-max x-max (cell-pos current-cell))])
        (set-cell-neighbors current-cell neighbors)))))

; Number Number -> MS
(define (initial-ms y-max x-max)
  (ms (add-neighbors y-max x-max (initial-board y-max x-max)) false true false))

; Number -> Number
(define (scale-cell->im-pos x-or-y)
  (+ (/ CELL-SIZE 2) (* CELL-SIZE x-or-y)))

; Number -> Number
(define (scale-im->cell-pos x-or-y)
  (floor (/ x-or-y CELL-SIZE)))

; Posn MS -> MS
(define (hide pos ms)
  (let* ([x (posn-x pos)]
         [y (posn-y pos)]
         [board (ms-board ms)]
         [cell (get-cell-from-board board x y)]
         [revealed-cell (set-cell-hidden cell true)]
         [new-board (set-cell-in-board board x y revealed-cell)])
    (set-ms-board ms new-board)))

; Posn MS -> MS
(define (reveal pos ms)
  (let* ([x (posn-x pos)]
         [y (posn-y pos)]
         [board (ms-board ms)]
         [cell (get-cell-from-board board x y)]
         [revealed-cell (set-cell-hidden cell false)]
         [new-board (set-cell-in-board board x y revealed-cell)])
    (set-ms-board ms new-board)))

; Posn MS -> MS
(define (flagon pos ms)
  (let* ([x (posn-x pos)]
         [y (posn-y pos)]
         [board (ms-board ms)]
         [cell (get-cell-from-board board x y)]
         [new-cell (set-cell-flagged cell true)])
    (set-ms-board ms (set-cell-in-board board x y new-cell))))

; Posn MS -> MS
(define (flagoff pos ms)
  (let* ([x (posn-x pos)]
         [y (posn-y pos)]
         [board (ms-board ms)]
         [cell (get-cell-from-board board x y)]
         [new-cell (set-cell-flagged cell false)])
    (set-ms-board ms  (set-cell-in-board board x y new-cell))))

; Posn MS -> MS
(define (add-mine pos ms)
  (let* ([board (ms-board ms)]
         [x (posn-x pos)]
         [y (posn-y pos)]
         [cell (get-cell-from-board board x y)]
         [new-cell (set-cell-celltype cell 'mine)])
    (set-ms-board ms (set-cell-in-board board x y new-cell))))


; Pos MS -> MS
(define (increment-one pos ms)
  (let* ([board (ms-board ms)]
         [x (posn-x pos)]
         [y (posn-y pos)]
         [cell (get-cell-from-board board x y)])
    (cond [(symbol? (cell-celltype cell)) ms]
          [else (define new-cell (set-cell-celltype cell (add1 (cell-celltype cell))))
                (set-ms-board ms (set-cell-in-board board x y new-cell))])))

; Posn MS -> MS
(define (increment-all pos ms)
  (let* ([board (ms-board ms)]
         [x (posn-x pos)]
         [y (posn-y pos)]
         [cell (get-cell-from-board board x y)]
         [neighbors (cell-neighbors cell)])
    (foldr increment-one ms neighbors)))

; [X] [List-of X] -> [List-of X]
(define (knuth-shuffle x)
  (define (swap! vec i j)
    (let ([tmp (vector-ref vec i)])
      (vector-set! vec i (vector-ref vec j))
      (vector-set! vec j tmp)))
  (if (list? x)
      (vector->list (knuth-shuffle (list->vector x)))
      (begin (for ([i (in-range (sub1 (vector-length x)) 0 -1)])
               (define r (random (+ i 1)))
               (swap! x i r))
             x)))

; MS KeyEvent -> MS
(define (key-handler ms ke)
  (cond [(key=? ke " ") (set-ms-flagon ms (not (ms-flagon? ms)))]
        [(key=? ke "escape") (set-ms-gameover ms true)]
        [else ms]))

; MouseEvent Number Number MS -> MS
(define (mouse-click-handler ms x y me)
  (cond
    [(mouse=? me "button-down")
     (define posn-of-cell-clicked (posn (scale-im->cell-pos x) (scale-im->cell-pos y)))
     (define new-state (board-click posn-of-cell-clicked ms))
     (if (won-state? new-state ) (set-ms-gameover new-state true) new-state )]
    [else ms]))

; Posn MS -> MS
(define (board-click pos ms)
  
  (define cell (get-cell-from-board (ms-board ms) (posn-x pos) (posn-y pos)))
  
  (match-define-values (flag-mode-on before-first-click safe-cell hidden flagged)
    (values (ms-flagon? ms) (ms-firstclick? ms) (number? (cell-celltype cell)) (cell-hidden? cell) (cell-flagged? cell)))
 
  (match-define-values (flag-mode-off after-first-click mine-cell revealed unflagged)
    (values (not flag-mode-on) (not before-first-click) (not safe-cell) (not hidden) (not flagged)))
  
  (cond
    ; The first click triggers mine generation
    [(and flag-mode-off before-first-click safe-cell hidden unflagged)   (on-first-click pos ms)]
    ; clicking on a safe cell to reveal it after post first click
    [(and flag-mode-off after-first-click safe-cell hidden unflagged)    (flood-fill pos ms)]
    ; clicking on a mine cell to reveal the mine post first click
    [(and flag-mode-off after-first-click mine-cell hidden unflagged)    (end-the-game pos ms)]
    ; Toggling the flag occurs when the flag mode is on and the cell is hidden
    [(and flag-mode-on before-first-click safe-cell hidden flagged)      (flagoff pos ms)]
    [(and flag-mode-on before-first-click safe-cell hidden unflagged)    (flagon pos ms)]
    [(and flag-mode-on before-first-click mine-cell hidden flagged)      (flagoff pos ms)]
    [(and flag-mode-on before-first-click mine-cell hidden unflagged)    (flagon pos ms)]
    [(and flag-mode-on after-first-click safe-cell hidden flagged)       (flagoff pos ms)]
    [(and flag-mode-on after-first-click safe-cell hidden unflagged)     (flagon pos ms)]
    [(and flag-mode-on after-first-click mine-cell hidden flagged)       (flagoff pos ms)]
    [(and flag-mode-on after-first-click mine-cell hidden unflagged)     (flagon pos ms)]
    ; Cannot put a flag on a revealed cell
    [(and flag-mode-on after-first-click safe-cell revealed unflagged)   ms]
    ; Cannot reveal a cell that has already been revealed
    [(and flag-mode-off after-first-click safe-cell revealed unflagged)  ms]
    ; Cannot un-flag a cell when the flag-mode is off
    [(and flag-mode-off after-first-click safe-cell hidden flagged)      ms]
    [(and flag-mode-off after-first-click mine-cell hidden flagged)      ms]
    [(and flag-mode-off before-first-click safe-cell hidden flagged)     ms]
    ; One of the following holds for a state to be unreachable:
    ; - (and before-first-click revealed)
    ; - (and before-first-click mine-cell)
    ; - (and revealed flagged) 
    ; - (and mine-cell revealed)
    [(and flag-mode-on before-first-click safe-cell revealed flagged)    ms]
    [(and flag-mode-on before-first-click safe-cell revealed unflagged)  ms]
    [(and flag-mode-on before-first-click mine-cell revealed flagged)    ms]
    [(and flag-mode-on before-first-click mine-cell revealed unflagged)  ms]
    [(and flag-mode-on after-first-click safe-cell revealed flagged)     ms]
    [(and flag-mode-on after-first-click mine-cell revealed flagged)     ms]
    [(and flag-mode-on after-first-click mine-cell revealed unflagged)   ms]
    [(and flag-mode-off after-first-click safe-cell revealed flagged)    ms]
    [(and flag-mode-off after-first-click mine-cell revealed flagged)    ms]
    [(and flag-mode-off after-first-click mine-cell revealed unflagged)  ms]
    [(and flag-mode-off before-first-click safe-cell revealed flagged)   ms]
    [(and flag-mode-off before-first-click safe-cell revealed unflagged) ms]
    [(and flag-mode-off before-first-click mine-cell hidden flagged)     ms]
    [(and flag-mode-off before-first-click mine-cell hidden unflagged)   ms]
    [(and flag-mode-off before-first-click mine-cell revealed flagged)   ms]
    [(and flag-mode-off before-first-click mine-cell revealed unflagged) ms]))


(define (end-the-game pos ms)
  
  (define x (posn-x pos))
  (define y (posn-y pos))
  (define board (ms-board ms))
  (define cell (get-cell-from-board board x y))
  (define new-cell (set-cell-celltype cell 'emine))
  (define new-board (set-cell-in-board board x y new-cell))
 
  ; (set-ms-gameover ms true)
  (set-ms-board (reveal pos ms) new-board))

; Posn MS -> MS
(define (on-first-click pos ms)
  (define res (let ([mines (generate-mines pos ms)])
                (flood-fill pos (set-ms-firstclick (increment-cells mines (add-mines mines ms)) false))))
  res)

; Posn MS -> [List-of Posn]
(define (generate-mines pos ms)
  (define res
    (let* ([board (ms-board ms)]
           [max-y (vector-length board)]
           [max-x (vector-length (vector-ref board 0))]
           [all-list (build-list (* max-y max-x) identity)]
           [neighbors (neighbors-of pos ms)]
           [exclude-list (map (λ (p) (posn->number p max-x)) (cons pos neighbors))])
      (take (map (λ (p) (number->posn p max-x)) (knuth-shuffle (remove* exclude-list all-list))) MINES)))
  res)

; [List-of Posn] MS -> MS
(define (add-mines mine-list ms)
  (define res (foldr add-mine ms mine-list))
  res)

; [List-of Posn] MS -> MS
(define (increment-cells mine-list ms)
  (define res (foldr increment-all ms mine-list))
  res)


; Posn Number -> Number
(define (posn->number pos max-x)
  (+ (* (posn-y pos) max-x)
     (posn-x pos)))

; Number Number -> Posn
(define (number->posn num max-x)
  (match-define-values (y x) (quotient/remainder num max-x))
  (posn x y))

; Posn MS -> MS
(define (reveal-cell+neighbors pos ms)
  (define res
    (let* ([neighbors (neighbors-of pos ms)]
           [cell+neighbors (cons pos neighbors)])
      (foldr reveal ms cell+neighbors)))
  res)

; Posn MS -> MS
(define (neighbors-of pos ms)
  (define res (cell-neighbors (get-cell-from-board (ms-board ms) (posn-x pos) (posn-y pos))))
  res)


; Posn -> Boolean
(define (num+hidden? pos ms)
  (let* ([cell (get-cell-from-board (ms-board ms) (posn-x pos) (posn-y pos))]
         [celltype (cell-celltype cell)])
    (and (number? celltype) (cell-hidden? cell))))

; Posn MS -> MS
(define (flood-fill pos ms)
  (define res
    (let* ([board (ms-board ms)]
           [x (posn-x pos)] 
           [y (posn-y pos)]
           [cell (get-cell-from-board board x y)]
           [cell-type (cell-celltype cell)]
           [neighbors (cons pos (cell-neighbors cell))])
      (cond [(and (number? cell-type) (= cell-type 0))
             (define hidden-zero-neighbors-posn-list (filter (λ (p) (num+hidden? p ms)) neighbors))
             (define new-ms (foldr reveal ms hidden-zero-neighbors-posn-list))
             (foldr flood-fill new-ms hidden-zero-neighbors-posn-list)]
            [(and (number? cell-type) (> cell-type 0))
             (reveal pos ms)]
            [(symbol? cell-type) ms])))
  res)

; Cell -> Image
(define (render-cell c)
  (define posn (cell-pos c))
  (define celltype (cell-celltype c))
  (cond [(cell-flagged? c) FLAG]
        [(cell-hidden? c) HIDDEN]
        [(equal? celltype 'mine) MINE]
        [(equal? celltype 'emine) EMINE]
        [(equal? celltype 0) ZERO]
        [(equal? celltype 1) ONE]
        [(equal? celltype 2) TWO]
        [(equal? celltype 3) THREE]
        [(equal? celltype 4) FOUR]
        [(equal? celltype 5) FIVE]
        [(equal? celltype 6) SIX]
        [(equal? celltype 7) SEVEN]
        [(equal? celltype 8) EIGHT]))

; MS Number Number -> Image
(define (render-ms ms)
  (let* ([board (ms-board ms)]
         [y-max (vector-length board)]
         [x-max (vector-length (vector-ref board 0))])
    (render-board board)))

; Board Number Number -> Image
(define (render-board board)
  (let ([y-max (vector-length board)]
        [x-max (vector-length (vector-ref board 0))])
    (define im (rectangle (* y-max CELL-SIZE) (* x-max CELL-SIZE) "solid" "black"))
    (for ([y y-max])
      (for ([x x-max])
        (set! im (place-image
                  (render-cell (vector-ref (vector-ref board y) x))
                  (scale-cell->im-pos x)
                  (scale-cell->im-pos y)
                  im))))
    im))

(define (won-state? ms)
  (define board (ms-board ms))
  ; - - -
  (define total-rows (vector-length board))
  (define first-row (vector-ref board 0))
  (define total-cols (vector-length first-row))
  ; - - -
  (define total-hidden
    (for/sum ([row board])
      (for/sum ([elem row])
        (if (and (not (cell-hidden? elem)) (number? (cell-celltype elem))) 1 0))))
  (= total-hidden (- (* total-rows total-cols) MINES)))

; MS -> Image
(define (last-scene ms)
  (if (won-state? ms)
      (text "you won" 20 "red")
      (text "you lost" 20 "red")))

; Number Number -> Void
(define (main r c)
  (void
   (big-bang (initial-ms r c)
     [to-draw render-ms]
     [on-mouse mouse-click-handler]
     [on-key key-handler]
     [stop-when ms-gameover? last-scene]
     [check-with ms?])))
