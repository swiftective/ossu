#lang racket
(require test-engine/racket-tests)

;; average-starter.rkt

;
; PROBLEM:
;
; Design a function called average that consumes (listof Number) and produces the
; average of the numbers in the list.

(check-expect (average (list 1 2 3)) 2)
(check-expect (average (list 1 2 3 4 5)) 3)

(define (average lon)
  (local [(define (avg lon sum c)
            (cond [(empty? lon) (/ sum c)]
                  [else
                   (avg (rest lon)
                        (+ (first lon) sum)
                        (add1 c))]))]
    (avg lon 0 0)))



(test)
