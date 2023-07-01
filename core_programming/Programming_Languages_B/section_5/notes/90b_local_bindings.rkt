#lang racket

(define (compute-fact-x-plus-one x)
  (define (fact n)
    (if (zero? n)
        1
        (* n (fact (sub1 n)))))
  ; (+ 1 2) ; be careful because returns the last expressions result
  (add1 (fact x)))

(compute-fact-x-plus-one 10)

; (fact n)  ; this is a error, bc fact is not defined
