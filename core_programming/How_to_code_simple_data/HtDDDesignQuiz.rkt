#lang racket
(require test-engine/racket-tests)

;; HtDD Design Quiz

;; Age is Natural
;; interp. the age of a person in years
#| (define A0 18) |#
#| (define A1 25) |#

#;
(define (fn-for-age a)
  (... a))

;; Template rules used:
;; - atomic non-distinct: Natural


; Problem 1:
;
; Consider the above data definition for the age of a person.
;
; Design a function called teenager? that determines whether a person
; of a particular age is a teenager (i.e., between the ages of 13 and 19).

;; Age -> Boolean
;; produce true if Age is between the ages of 13 and 19, else false

(check-expect (teenager? 14) #t) ; Tests
(check-expect (teenager? 20) #f)
(check-expect (teenager? 8) #f)

;; (define (teenager? a) true) ; Stub

;; <use template from Age>

(define (teenager? a)
  (and
   (<= a 18)
   (>= a 14)))



; Problem 2:
;
; Design a data definition called MonthAge to represent a person's age
; in months.


;; MonthAge is Natural
;; Interp. person's age in months
;; (define MA1 100)
;; (define MA2 0.5)

#;
(define (fn-for-monthage ma)
  (... ma))

;; Template rules used
;; - atomic non-distinct: Natural



; Problem 3:
;
; Design a function called months-old that takes a person's age in years
; and yields that person's age in months.

;; Age -> MonthAge
;; produce the product of 12 and Age

(check-expect (months-old 12) 144) ; Tests
(check-expect (months-old 10) 120)
#| (check-expect (months-old 0.5) 6.0) ; No idea |#

;; (define (months-old a) ma) ; Stub

;; <use template from MonthAge>

(define (months-old ma)
  (* ma 12))



; Problem 4:
;
; Consider a video game where you need to represent the health of your
; character. The only thing that matters about their health is:
;
;   - if they are dead (which is shockingly poor health)
;   - if they are alive then they can have 0 or more extra lives
;
; Design a data definition called Health to represent the health of your
; character.

;; Health is one of:
;; "Dead"
;; Natural
;; Interp. "Dead" means character is dead, else the number of lives remaining
;; (define H1 "Dead")
;; (define H2 100)

#;
(define (fn-for-health h)
  (cond
    [(string=? h "Dead") (...)]
    [else (... h)]))

;; Template rules used
;; - one of: 2 cases
;; - Atomic distint: "Dead"
;; - Atomic non-distint: Natural

; Design a function called increase-health that allows you to increase the
; lives of a character.  The function should only increase the lives
; of the character if the character is not dead, otherwise the character
; remains dead.

;; Health -> Health
;; increases the numner of lives if the player is alive

(check-expect (increase-health "Dead") "Dead") ; Tests
(check-expect (increase-health 1) 2)

;; (define (increase-health h) h) ; Stub

;; <use template from Health>

(define (increase-health h)
  (cond
    [(string? h) "Dead"]
    [else (+ h 1)]))

(test)
