#lang racket
(require test-engine/racket-tests)

;; rocket-starter.rkt

;; =================
;; Data definitions:

;
; PROBLEM A:
;
; You are designing a program to track a rocket's journey as it descends
; 100 kilometers to Earth. You are only interested in the descent from
; 100 kilometers to touchdown. Once the rocket has landed it is done.
;
; Design a data definition to represent the rocket's remaining descent.
; Call it RocketDescent.

;; RocketDescent is one of:
;; - Natural(0, 100]
;; - "Done"
;; Interp. "Done" if the rocket has landed, otherwise number of kilometers left to Earth
;; (define RD1 100)
;; (define RD2 40)
;; (define RD3 0.5)
;; (define RD5 "Done")

#;
(define (fn-for-rocketdescent rd)
  (cond
    [(and
       (< 0 rd)
       (<= rd 100))
     (... rd)]

    [else (...)]
  )
)

;; Template rules used
;; - one of: 2 cases
;; - Atomic non-distinct: Nataral[1, 100]
;; - Atomic distinct: "Done"


;; =================
;; Functions:

;
; PROBLEM B:
;
; Design a function that will output the rocket's remaining descent distance
; in a short string that can be broadcast on Twitter.
; When the descent is over, the message should be "The rocket has landed!".
; Call your function rocket-descent-to-msg.

;; RocketDescent -> String
;; produce the rocket's remaining descent distance in short string
;; for Twitter

(check-expect (rocket-descent-to-msg 100) "Remaining distance is 100 kilometers")
(check-expect (rocket-descent-to-msg 40) "Remaining distance is 40 kilometers")
(check-expect (rocket-descent-to-msg 0.5) "Remaining distance is 0.5 kilometers")
(check-expect (rocket-descent-to-msg "Done") "The rocket has landed!")

;; (define (rocket-descent-to-msg rd) "") ; Stub

;; <use template from RocketDescent>

(define (rocket-descent-to-msg rd)
  (cond
    [(and
       (number? rd)
       (< 0 rd)
       (<= rd 100))
     (string-append "Remaining distance is " (number->string rd) " kilometers")]

    [else "The rocket has landed!"]
  )
)

(test)
