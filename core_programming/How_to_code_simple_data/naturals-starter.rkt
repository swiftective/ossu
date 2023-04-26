#lang racket
(require test-engine/racket-tests)

;; naturals-starter.rkt

;
; Now that we understand how to form data definitions for abitrary-sized data we can
; look at how to design functions that "count down" on natural numbers.  Here's the
; key inight - ask yourself how many natural numbers are there?
;
; A lot, many... an arbitrary number:
;
; 0                      ;0
; (add1 0)               ;1
; (add1 (add1 0))        ;2
; (add1 (add1 (add1 0))) ;3
;
; and so on
;
; What that is saying is that (add1 <some natural>) produces a natural,
; similarly (sub1 <some natural other than 0>) produces a natural.
;
; So add1 is kind of like cons, it takes a natural and makes a bigger one
; (cons makes a longer list). And sub1 is kind of like rest it takes a natural
; (other than 0) and gives the next smallest one (rest gives shorter list).
;
;
; Consider this type comment:
;


;; Natural is one of:
;;  - 0
;;  - (add1 Natural)
;; interp. a natural number
(define N0 0)         ;0
(define N1 (add1 N0)) ;1
(define N2 (add1 N1)) ;2

#;
(define (fn-for-natural n)
  (cond [(zero? n) (...)]
        [else
         (... ;n                       ;template rules wouldn't normally put this
          ;                            ;here, but we will see that we end up coming
          ;                            ;back to add it
          (fn-for-natural (sub1 n)))]))

;; Template rules used:
;;  - one-of: two cases
;;  - atomic distinct: 0
;;  - compound: (add1 Natural)
;;  - self-reference: (sub1 n) is Natural

; PROBLEMs:
;
; (A) Design a function that consumes Natural number n and produces the sum of all
;     the naturals in [0, n].

;; Natural -> Natural
;; produces the sum of all the naturals in [0, n]

(check-expect (sum 10) 55) ; Tests
(check-expect (sum 5) 15)

;; (define (sum n) 0) ; Stub

;; took template from Natural

(define (sum n)
  (cond [(zero? n) 0]
        [else
         (+ n (sum (sub1 n)))]))

; (B) Design a function that consumes Natural number n and produces a list of all
;     the naturals of the form (cons n (cons n-1 ... empty)) not including 0.

;; Natural -> ListOfNatural
;; produce a list of all the naturals of the form (cons n (cons n-1 ... empty))
;; not including 0

(check-expect (to-list 0) empty) ; Tests
(check-expect (to-list 1) (cons 1 empty)) ; Tests
(check-expect (to-list 2) (cons 2 (cons 1 empty)))

;; (define (list-natural n) empty) ; Stub

;; took template from Natural

(define (to-list n)
  (cond [(zero? n) empty]
        [else
         (cons n (to-list (sub1 n)))]))


(test)
