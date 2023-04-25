#lang racket
(require test-engine/racket-tests)

;; CarSpeed is Number[0, 200]
;; Interp. the speed of a car in km/h: 0 means stopped, 200 is the maximum speed
;; (define CS 0)
;; (define CS2 200)
;; (define CS3 100)

#;
(define (fn-for-carspeed cs)
  (... cs)
)

;; Template rules used
;; - Atomic non-distinct: Number[0, 200]

;; CarSpeed -> Boolean
;; produces true if car's speed is over speed limit, else false

(check-expect (speeding? 0) #f) ; Tests
(check-expect (speeding? 111) #t)

;; (define (speeding? cs) true) ; Stub

;; took template from CarSpeed

(define (speeding? cs) (> cs 110))

(test)
