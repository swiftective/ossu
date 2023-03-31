#lang racket
(require test-engine/racket-tests)

(check-expect (sub 3 4) 1)
(check-expect (sub 4 3) 2)

(define (sub x y)
  (if (> x y) (- x y) (- y x)))


(test)
