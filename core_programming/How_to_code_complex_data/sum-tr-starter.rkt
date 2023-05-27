#lang racket
(require test-engine/racket-tests)

;; sum-tr-starter.rkt

;
; PROBLEM:
;
; (A) Consider the following function that consumes a list of numbers and produces
;     the sum of all the numbers in the list. Use the stepper to analyze the behavior
;     of this function as the list gets larger and larger.
;
; (B) Use an accumulator to design a tail-recursive version of sum.
;


;; (listof Number) -> Number
;; produce sum of all elements of lon
(check-expect (sum empty) 0)
(check-expect (sum (list 2 4 5)) 11)

(define (sum lon)
  (local [(define (sum lon acc)
            (cond [(empty? lon) acc]
                  [else
                    (sum (rest lon)
                         (+ (first lon) acc))]))]
    (sum lon 0)))

(check-expect (product (list 2 3 4)) 24)


(define (product lon)
  (local [(define (product lon acc)
            (cond [(empty? lon) acc]
                  [else
                    (product (rest lon)
                         (* (first lon) acc))]))]
    (product lon 1)))

(check-expect (avg (list 1 2 3)) 2)
(check-expect (avg (list 1 2 3 4 5)) 3)

(define (avg lon)
  (local [(define (avg lon acc index)
            (cond [(empty? lon) (/ acc index)]
                  [else
                    (avg (rest lon)
                         (+ (first lon) acc)
                         (add1 index))]))]
    (avg lon 0 0)))




(test)
