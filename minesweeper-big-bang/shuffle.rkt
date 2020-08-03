#lang racket

(provide shuffle)
 
(define (swap! vec i j)
  (let ([tmp (vector-ref vec i)])
    (vector-set! vec i (vector-ref vec j))
    (vector-set! vec j tmp)))
 
(define (shuffle x)
  (if (list? x)
    (vector->list (shuffle (list->vector x)))
    (begin (for ([i (in-range (sub1 (vector-length x)) 0 -1)])
             (define r (random (+ i 1)))
             (swap! x i r))
           x)))