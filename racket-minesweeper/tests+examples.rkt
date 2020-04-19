#lang racket

(require "minesweeper.rkt")


(define cell00 (cell (posn 0 0) empty 0 true false))
(define cell10 (cell (posn 1 0) empty 0 true false))
(define cell20 (cell (posn 2 0) empty 0 true false))
(define cell01 (cell (posn 0 1) empty 0 true false))
(define cell11 (cell (posn 1 1) empty 0 true false))
(define cell21 (cell (posn 2 1) empty 0 true false))
(define cell02 (cell (posn 0 2) empty 0 true false))
(define cell12 (cell (posn 1 2) empty 0 true false))
(define cell22 (cell (posn 2 2) empty 0 true false))

(define initboard11 (vector (vector cell00)))
(define initboard22
  (vector (vector cell00 cell10)
          (vector cell01 cell11)))
(define initboard33
  (vector (vector cell00 cell10 cell20)
          (vector cell01 cell11 cell21)
          (vector cell02 cell12 cell22)))
(define initboard21
  (vector (vector cell00 cell10)))
(define initboard23
  (vector (vector cell00 cell10)
          (vector cell01 cell11)
          (vector cell02 cell12)))

(define board+neighbors22
  (vector (vector (cell (posn 0 0) (list (posn 0 1) (posn 1 0) (posn 1 1)) 0 true false)
                  (cell (posn 1 0) (list (posn 0 0) (posn 0 1) (posn 1 1)) 0 true false))
          (vector (cell (posn 0 1) (list (posn 0 0) (posn 1 0) (posn 1 1)) 0 true false)
                  (cell (posn 1 1) (list (posn 0 0) (posn 0 1) (posn 1 0)) 0 true false))))

(define neighbor00 (list (posn 0 1) (posn 1 0) (posn 1 1)))
(define neighbor10 (list (posn 0 0) (posn 0 1) (posn 1 1) (posn 2 0) (posn 2 1)))
(define neighbor20 (list (posn 1 0) (posn 1 1) (posn 2 1)))
(define neighbor01 (list (posn 0 0) (posn 0 2) (posn 1 0) (posn 1 1) (posn 1 2)))
(define neighbor11 (list (posn 0 0) (posn 0 1) (posn 0 2) (posn 1 0) 
                         (posn 1 2) (posn 2 0) (posn 2 1) (posn 2 2)))
(define neighbor21 (list (posn 1 0) (posn 1 1) (posn 1 2) (posn 2 0) (posn 2 2)))
(define neighbor02 (list (posn 0 1) (posn 1 1) (posn 1 2)))
(define neighbor12 (list (posn 0 1) (posn 0 2) (posn 1 1) (posn 2 1) (posn 2 2)))
(define neighbor22 (list (posn 1 1) (posn 1 2) (posn 2 1)))

(define cell00+neigh (cell (posn 0 0) neighbor00 0 #t #f))
(define cell10+neigh (cell (posn 1 0) neighbor10 0 #t #f))
(define cell20+neigh (cell (posn 2 0) neighbor20 0 #t #f))
(define cell01+neigh (cell (posn 0 1) neighbor01 0 #t #f))
(define cell11+neigh (cell (posn 1 1) neighbor11 0 #t #f))
(define cell21+neigh (cell (posn 2 1) neighbor21 0 #t #f))
(define cell02+neigh (cell (posn 0 2) neighbor02 0 #t #f))
(define cell12+neigh (cell (posn 1 2) neighbor12 0 #t #f))
(define cell22+neigh (cell (posn 2 2) neighbor22 0 #t #f))


(define cell11+neigh-hidden (cell (posn 1 1) neighbor11 0 true false))
(define cell11+neigh-revealed (cell (posn 1 1) neighbor11 0 false false))
(define cell11+neigh-flagged (cell (posn 1 1) neighbor11 0 false true))
(define cell11+neigh-unflagged (cell (posn 1 1) neighbor11 0 false false))
(define cell11+neigh-mine (cell (posn 1 1) neighbor11 'mine true false))


(define cell00+neigh-one (cell (posn 0 0) neighbor00 1 #t #f))
(define cell10+neigh-one (cell (posn 1 0) neighbor10 1 #t #f))
(define cell20+neigh-one (cell (posn 2 0) neighbor20 1 #t #f))
(define cell01+neigh-one (cell (posn 0 1) neighbor01 1 #t #f))
(define cell11+neigh-one (cell (posn 1 1) neighbor11 1 #t #f))
(define cell21+neigh-one (cell (posn 2 1) neighbor21 1 #t #f))
(define cell02+neigh-one (cell (posn 0 2) neighbor02 1 #t #f))
(define cell12+neigh-one (cell (posn 1 2) neighbor12 1 #t #f))
(define cell22+neigh-one (cell (posn 2 2) neighbor22 1 #t #f))


(define board+neighbors33
  (vector
   (vector cell00+neigh cell10+neigh cell20+neigh)
   (vector cell01+neigh cell11+neigh cell21+neigh)
   (vector cell02+neigh cell12+neigh cell22+neigh)))

(define ms-ex (ms board+neighbors33 false true false))


(define plist (list (posn 0 0) (posn 0 1) (posn 0 2) (posn 1 0) (posn 1 2) (posn 2 0) (posn 2 1) (posn 2 2)))


(module+ test
  (require rackunit)

  (check-equal? (initial-cell 0 0) cell00)
  (check-equal? (initial-cell 1 2) cell12)
  (check-equal? (initial-cell 2 1) cell21)
  (check-equal? (initial-cell 2 2) cell22)

  (check-equal? (initial-board 1 1) initboard11)
  (check-equal? (initial-board 2 2) initboard22)
  (check-equal? (initial-board 3 3) initboard33)
  (check-equal? (initial-board 2 1) initboard21)
  (check-equal? (initial-board 2 3) initboard23)

  (check-equal? (posn=? (posn 1 2) (posn 1 2)) true)
  (check-equal? (posn=? (posn 1 1) (posn 0 1)) false)

  (check-equal? (cartesian-product empty empty) empty)
  (check-equal? (cartesian-product empty '(1 2 3)) empty)
  (check-equal? (cartesian-product '(1 2 3) empty) empty)
  (check-equal? (cartesian-product '(1) '(1)) (list (posn 1 1)))
  (check-equal? (cartesian-product '(1) '(1 2)) (list (posn 1 1) (posn 1 2)))
  (check-equal? (cartesian-product '(1 2) '(1)) (list (posn 1 1) (posn 2 1)))
  (check-equal? (cartesian-product '(1 2) '(1 2)) (list (posn 1 1) (posn 1 2) (posn 2 1) (posn 2 2)))
  (check-equal? (cartesian-product '(1 2 3) '(1 2 3))
                (list (posn 1 1) (posn 1 2) (posn 1 3) (posn 2 1) (posn 2 2)
                      (posn 2 3) (posn 3 1) (posn 3 2) (posn 3 3)))

  (check-equal? (get-neighbors 3 3 (posn 0 0)) (list (posn 0 1) (posn 1 0) (posn 1 1)))
  (check-equal? (get-neighbors 3 3 (posn 1 0))
                (list (posn 0 0) (posn 0 1) (posn 1 1) (posn 2 0) (posn 2 1)))
  (check-equal? (get-neighbors 3 3 (posn 1 1))
                (list (posn 0 0) (posn 0 1) (posn 0 2) (posn 1 0)
                      (posn 1 2) (posn 2 0) (posn 2 1) (posn 2 2)))

  (check-equal? (add-neighbors 2 2 initboard22) board+neighbors22)
  (check-equal? (add-neighbors 3 3 initboard33) board+neighbors33) 

  (check-equal? (initial-ms 3 3) ms-ex)
  (check-equal? (initial-ms 2 2) (ms board+neighbors22 false true false))


  (check-equal?
   (increment-cells
    (list (posn 1 2))
    (ms
     (vector
      (vector
       (cell (posn 0 0) (list (posn 0 1) (posn 1 0) (posn 1 1)) 0 #t #f)
       (cell (posn 1 0) (list (posn 0 0) (posn 0 1) (posn 1 1) (posn 2 0) (posn 2 1)) 0 #t #f)
       (cell (posn 2 0) (list (posn 1 0) (posn 1 1) (posn 2 1)) 0 #t #f))
      (vector
       (cell (posn 0 1) (list (posn 0 0) (posn 0 2) (posn 1 0) (posn 1 1) (posn 1 2)) 0 #t #f)
       (cell (posn 1 1) (list (posn 0 0) (posn 0 1) (posn 0 2) (posn 1 0) (posn 1 2) (posn 2 0) (posn 2 1) (posn 2 2)) 0 #t #f)
       (cell (posn 2 1) (list (posn 1 0) (posn 1 1) (posn 1 2) (posn 2 0) (posn 2 2)) 0 #t #f))
      (vector
       (cell (posn 0 2) (list (posn 0 1) (posn 1 1) (posn 1 2)) 0 #t #f)
       (cell (posn 1 2) (list (posn 0 1) (posn 0 2) (posn 1 1) (posn 2 1) (posn 2 2)) 'mine #t #f)
       (cell (posn 2 2) (list (posn 1 1) (posn 1 2) (posn 2 1)) 0 #t #f)))
     #f
     #f
     #f))
   (ms
    (vector
     (vector
      (cell (posn 0 0) (list (posn 0 1) (posn 1 0) (posn 1 1)) 0 #t #f)
      (cell (posn 1 0) (list (posn 0 0) (posn 0 1) (posn 1 1) (posn 2 0) (posn 2 1)) 0 #t #f)
      (cell (posn 2 0) (list (posn 1 0) (posn 1 1) (posn 2 1)) 0 #t #f))
     (vector
      (cell (posn 0 1) (list (posn 0 0) (posn 0 2) (posn 1 0) (posn 1 1) (posn 1 2)) 1 #t #f)
      (cell (posn 1 1) (list (posn 0 0) (posn 0 1) (posn 0 2) (posn 1 0) (posn 1 2) (posn 2 0) (posn 2 1) (posn 2 2)) 1 #t #f)
      (cell (posn 2 1) (list (posn 1 0) (posn 1 1) (posn 1 2) (posn 2 0) (posn 2 2)) 1 #t #f))
     (vector
      (cell (posn 0 2) (list (posn 0 1) (posn 1 1) (posn 1 2)) 1 #t #f)
      (cell (posn 1 2) (list (posn 0 1) (posn 0 2) (posn 1 1) (posn 2 1) (posn 2 2)) 'mine #t #f)
      (cell (posn 2 2) (list (posn 1 1) (posn 1 2) (posn 2 1)) 1 #t #f)))
    #f
    #f
    #f))

  (check-equal?
   (increment-cells
    (list (posn 1 1))
    (ms
     (vector
      (vector
       (cell (posn 0 0) (list (posn 0 1) (posn 1 0) (posn 1 1)) 0 #t #f)
       (cell (posn 1 0) (list (posn 0 0) (posn 0 1) (posn 1 1) (posn 2 0) (posn 2 1)) 0 #t #f)
       (cell (posn 2 0) (list (posn 1 0) (posn 1 1) (posn 2 1)) 0 #t #f))
      (vector
       (cell (posn 0 1) (list (posn 0 0) (posn 0 2) (posn 1 0) (posn 1 1) (posn 1 2)) 0 #t #f)
       (cell (posn 1 1) (list (posn 0 0) (posn 0 1) (posn 0 2) (posn 1 0) (posn 1 2) (posn 2 0) (posn 2 1) (posn 2 2)) 'mine #t #f)
       (cell (posn 2 1) (list (posn 1 0) (posn 1 1) (posn 1 2) (posn 2 0) (posn 2 2)) 0 #t #f))
      (vector
       (cell (posn 0 2) (list (posn 0 1) (posn 1 1) (posn 1 2)) 0 #t #f)
       (cell (posn 1 2) (list (posn 0 1) (posn 0 2) (posn 1 1) (posn 2 1) (posn 2 2)) 0 #t #f)
       (cell (posn 2 2) (list (posn 1 1) (posn 1 2) (posn 2 1)) 0 #t #f)))
     #f
     #f
     #f))
   (ms
    (vector
     (vector
      (cell (posn 0 0) (list (posn 0 1) (posn 1 0) (posn 1 1)) 1 #t #f)
      (cell (posn 1 0) (list (posn 0 0) (posn 0 1) (posn 1 1) (posn 2 0) (posn 2 1)) 1 #t #f)
      (cell (posn 2 0) (list (posn 1 0) (posn 1 1) (posn 2 1)) 1 #t #f))
     (vector
      (cell (posn 0 1) (list (posn 0 0) (posn 0 2) (posn 1 0) (posn 1 1) (posn 1 2)) 1 #t #f)
      (cell (posn 1 1) (list (posn 0 0) (posn 0 1) (posn 0 2) (posn 1 0) (posn 1 2) (posn 2 0) (posn 2 1) (posn 2 2)) 'mine #t #f)
      (cell (posn 2 1) (list (posn 1 0) (posn 1 1) (posn 1 2) (posn 2 0) (posn 2 2)) 1 #t #f))
     (vector
      (cell (posn 0 2) (list (posn 0 1) (posn 1 1) (posn 1 2)) 1 #t #f)
      (cell (posn 1 2) (list (posn 0 1) (posn 0 2) (posn 1 1) (posn 2 1) (posn 2 2)) 1 #t #f)
      (cell (posn 2 2) (list (posn 1 1) (posn 1 2) (posn 2 1)) 1 #t #f)))
    #f
    #f
    #f))

  (check-equal?
   (increment-cells
    (list (posn 0 1) (posn 2 1))
    (ms
     (vector
      (vector
       (cell (posn 0 0) (list (posn 0 1) (posn 1 0) (posn 1 1)) 0 #t #f)
       (cell (posn 1 0) (list (posn 0 0) (posn 0 1) (posn 1 1) (posn 2 0) (posn 2 1)) 0 #t #f)
       (cell (posn 2 0) (list (posn 1 0) (posn 1 1) (posn 2 1)) 0 #t #f))
      (vector
       (cell (posn 0 1) (list (posn 0 0) (posn 0 2) (posn 1 0) (posn 1 1) (posn 1 2)) 'mine #t #f)
       (cell (posn 1 1) (list (posn 0 0) (posn 0 1) (posn 0 2) (posn 1 0) (posn 1 2) (posn 2 0) (posn 2 1) (posn 2 2)) 0 #t #f)
       (cell (posn 2 1) (list (posn 1 0) (posn 1 1) (posn 1 2) (posn 2 0) (posn 2 2)) 'mine #t #f))
      (vector
       (cell (posn 0 2) (list (posn 0 1) (posn 1 1) (posn 1 2)) 0 #t #f)
       (cell (posn 1 2) (list (posn 0 1) (posn 0 2) (posn 1 1) (posn 2 1) (posn 2 2)) 0 #t #f)
       (cell (posn 2 2) (list (posn 1 1) (posn 1 2) (posn 2 1)) 0 #t #f)))
     #f
     #f
     #f))
   (ms
    (vector
     (vector
      (cell (posn 0 0) (list (posn 0 1) (posn 1 0) (posn 1 1)) 1 #t #f)
      (cell (posn 1 0) (list (posn 0 0) (posn 0 1) (posn 1 1) (posn 2 0) (posn 2 1)) 2 #t #f)
      (cell (posn 2 0) (list (posn 1 0) (posn 1 1) (posn 2 1)) 1 #t #f))
     (vector
      (cell (posn 0 1) (list (posn 0 0) (posn 0 2) (posn 1 0) (posn 1 1) (posn 1 2)) 'mine #t #f)
      (cell (posn 1 1) (list (posn 0 0) (posn 0 1) (posn 0 2) (posn 1 0) (posn 1 2) (posn 2 0) (posn 2 1) (posn 2 2)) 2 #t #f)
      (cell (posn 2 1) (list (posn 1 0) (posn 1 1) (posn 1 2) (posn 2 0) (posn 2 2)) 'mine #t #f))
     (vector
      (cell (posn 0 2) (list (posn 0 1) (posn 1 1) (posn 1 2)) 1 #t #f)
      (cell (posn 1 2) (list (posn 0 1) (posn 0 2) (posn 1 1) (posn 2 1) (posn 2 2)) 2 #t #f)
      (cell (posn 2 2) (list (posn 1 1) (posn 1 2) (posn 2 1)) 1 #t #f)))
    #f
    #f
    #f))

  (check-equal?
   (increment-cells
    (list (posn 0 0) (posn 1 0) (posn 2 0) (posn 0 1) (posn 2 1) (posn 0 2) (posn 1 2) (posn 2 2))
    (ms
     (vector
      (vector
       (cell (posn 0 0) (list (posn 0 1) (posn 1 0) (posn 1 1)) 'mine #t #f)
       (cell (posn 1 0) (list (posn 0 0) (posn 0 1) (posn 1 1) (posn 2 0) (posn 2 1)) 'mine #t #f)
       (cell (posn 2 0) (list (posn 1 0) (posn 1 1) (posn 2 1)) 'mine #t #f))
      (vector
       (cell (posn 0 1) (list (posn 0 0) (posn 0 2) (posn 1 0) (posn 1 1) (posn 1 2)) 'mine #t #f)
       (cell (posn 1 1) (list (posn 0 0) (posn 0 1) (posn 0 2) (posn 1 0) (posn 1 2) (posn 2 0) (posn 2 1) (posn 2 2)) 0 #t #f)
       (cell (posn 2 1) (list (posn 1 0) (posn 1 1) (posn 1 2) (posn 2 0) (posn 2 2)) 'mine #t #f))
      (vector
       (cell (posn 0 2) (list (posn 0 1) (posn 1 1) (posn 1 2)) 'mine #t #f)
       (cell (posn 1 2) (list (posn 0 1) (posn 0 2) (posn 1 1) (posn 2 1) (posn 2 2)) 'mine #t #f)
       (cell (posn 2 2) (list (posn 1 1) (posn 1 2) (posn 2 1)) 'mine #t #f)))
     #f
     #f
     #f))
   (ms
    (vector
     (vector
      (cell (posn 0 0) (list (posn 0 1) (posn 1 0) (posn 1 1)) 'mine #t #f)
      (cell (posn 1 0) (list (posn 0 0) (posn 0 1) (posn 1 1) (posn 2 0) (posn 2 1)) 'mine #t #f)
      (cell (posn 2 0) (list (posn 1 0) (posn 1 1) (posn 2 1)) 'mine #t #f))
     (vector
      (cell (posn 0 1) (list (posn 0 0) (posn 0 2) (posn 1 0) (posn 1 1) (posn 1 2)) 'mine #t #f)
      (cell (posn 1 1) (list (posn 0 0) (posn 0 1) (posn 0 2) (posn 1 0) (posn 1 2) (posn 2 0) (posn 2 1) (posn 2 2)) 8 #t #f)
      (cell (posn 2 1) (list (posn 1 0) (posn 1 1) (posn 1 2) (posn 2 0) (posn 2 2)) 'mine #t #f))
     (vector
      (cell (posn 0 2) (list (posn 0 1) (posn 1 1) (posn 1 2)) 'mine #t #f)
      (cell (posn 1 2) (list (posn 0 1) (posn 0 2) (posn 1 1) (posn 2 1) (posn 2 2)) 'mine #t #f)
      (cell (posn 2 2) (list (posn 1 1) (posn 1 2) (posn 2 1)) 'mine #t #f)))
    #f
    #f
    #f))


  (check-true
   (let ([shuffled (knuth-shuffle '(1 2 3))])
     (or (equal? shuffled '(1 2 3))
         (equal? shuffled '(1 3 2))
         (equal? shuffled '(2 1 3))
         (equal? shuffled '(2 3 1))
         (equal? shuffled '(3 2 1))
         (equal? shuffled '(3 1 2)))))





  (check-equal?
   (hide (posn 1 1)
         (ms (vector
              (vector cell00+neigh cell10+neigh cell20+neigh)
              (vector cell01+neigh cell11+neigh-revealed cell21+neigh)
              (vector cell02+neigh cell12+neigh cell22+neigh))
             false false false))
   (ms (vector
        (vector cell00+neigh cell10+neigh cell20+neigh)
        (vector cell01+neigh cell11+neigh-hidden cell21+neigh)
        (vector cell02+neigh cell12+neigh cell22+neigh))
       false false false))

  (check-equal?
   (reveal (posn 1 1)
           (ms (vector
                (vector cell00+neigh cell10+neigh cell20+neigh)
                (vector cell01+neigh cell11+neigh-hidden cell21+neigh)
                (vector cell02+neigh cell12+neigh cell22+neigh))
               false false false))
   (ms (vector
        (vector cell00+neigh cell10+neigh cell20+neigh)
        (vector cell01+neigh cell11+neigh-revealed cell21+neigh)
        (vector cell02+neigh cell12+neigh cell22+neigh))
       false false false))

  (check-equal?
   (flagon (posn 1 1)
           (ms (vector
                (vector cell00+neigh cell10+neigh cell20+neigh)
                (vector cell01+neigh cell11+neigh-unflagged cell21+neigh)
                (vector cell02+neigh cell12+neigh cell22+neigh))
               false false false))
   (ms (vector
        (vector cell00+neigh cell10+neigh cell20+neigh)
        (vector cell01+neigh cell11+neigh-flagged cell21+neigh)
        (vector cell02+neigh cell12+neigh cell22+neigh))
       false false false))

  (check-equal?
   (flagoff (posn 1 1)
            (ms (vector
                 (vector cell00+neigh cell10+neigh cell20+neigh)
                 (vector cell01+neigh cell11+neigh-flagged cell21+neigh)
                 (vector cell02+neigh cell12+neigh cell22+neigh))
                false false false))
   (ms (vector
        (vector cell00+neigh cell10+neigh cell20+neigh)
        (vector cell01+neigh cell11+neigh-unflagged cell21+neigh)
        (vector cell02+neigh cell12+neigh cell22+neigh))
       false false false))



  (check-equal? (map (λ (p) (number->posn p 3)) (map (λ (p) (posn->number p 3)) plist)) plist)


  (check-equal?
   (add-mine (posn 1 1)
             (ms (vector
                  (vector cell00+neigh cell10+neigh cell20+neigh)
                  (vector cell01+neigh cell11+neigh cell21+neigh)
                  (vector cell02+neigh cell12+neigh cell22+neigh))
                 false false false))
   (ms (vector
        (vector cell00+neigh cell10+neigh cell20+neigh)
        (vector cell01+neigh cell11+neigh-mine cell21+neigh)
        (vector cell02+neigh cell12+neigh cell22+neigh))
       false false false))



  (check-equal?
   (increment-one (posn 1 1)
                  (ms (vector
                       (vector cell00+neigh cell10+neigh cell20+neigh)
                       (vector cell01+neigh cell11+neigh cell21+neigh)
                       (vector cell02+neigh cell12+neigh cell22+neigh))
                      false false false))
   (ms (vector
        (vector cell00+neigh cell10+neigh cell20+neigh)
        (vector cell01+neigh cell11+neigh-one cell21+neigh)
        (vector cell02+neigh cell12+neigh cell22+neigh))
       false false false))

  (check-equal?
   (increment-all (posn 1 1)
                  (ms (vector
                       (vector cell00+neigh cell10+neigh cell20+neigh)
                       (vector cell01+neigh cell11+neigh cell21+neigh)
                       (vector cell02+neigh cell12+neigh cell22+neigh))
                      false false false))
   (ms (vector
        (vector cell00+neigh-one cell10+neigh-one cell20+neigh-one)
        (vector cell01+neigh-one cell11+neigh cell21+neigh-one)
        (vector cell02+neigh-one cell12+neigh-one cell22+neigh-one))
       false false false))



  

  )


