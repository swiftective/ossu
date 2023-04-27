#lang racket
(require test-engine/racket-tests)


;; odd-from-n-starter.rkt

;  PROBLEM:
;
;  Design a function called odd-from-n that consumes a natural number n, and produces a list of all
;  the odd numbers from n down to 1.
;
;  Note that there is a primitive function, odd?, that produces true if a natural number is odd.

;; Natural -> Natural
;; produce a list of all the odd numbers from n down to 1

(check-expect (list-odds 0) empty) ; Tests
(check-expect (list-odds 1) (cons 1 empty))
(check-expect (list-odds 3) (cons 3 (cons 1 empty)))
(check-expect (list-odds 5) (cons 5 (cons 3 (cons 1 empty))))

;; (define (list-odds n) empty) ; Stub

(define (list-odds n)
  (cond
    [(zero? n) empty]
    [else
     (if (odd? n)
         (cons n (list-odds (sub1 n)))
         (list-odds (sub1 n)))]))

(test)
