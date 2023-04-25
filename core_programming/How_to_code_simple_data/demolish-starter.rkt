#lang racket
(require test-engine/racket-tests)

;; demolish-starter.rkt

;; =================
;; Data definitions:

; PROBLEM A:
;
; You are assigned to develop a system that will classify
; buildings in downtown Vancouver based on how old they are.
; According to city guidelines, there are three different classification levels:
; new, old, and heritage.
;
; Design a data definition to represent these classification levels.
; Call it BuildingStatus.

;; BuildingStatus is one of:
;; - "new"
;; - "old"
;; - "heritage"
;; Interp. classification based on age of building
;; <examples are redundant for enumeration>

#;
(define (fn-for-buildingstatus bs)
  (cond
    [(string=? bs "new") (...)]
    [(string=? bs "old") (...)]
    [(string=? bs "heritage") (...)]
  )
)

;; Template rules used
;; - one of: 3 cases
;; - atomic distinct value: "new"
;; - atomic distinct value: "old"
;; - atomic distinct value: "heritage"

;; =================
;; Functions:

; PROBLEM B:
;
; The city wants to demolish all buildings classified as "old".
; You are hired to design a function called demolish?
; that determines whether a building should be torn down or not.

;; BuildingStatus -> Boolean
;; produce true is building status is old

(check-expect (demolish? "new") #f) ; Tests
(check-expect (demolish? "old") #t)
(check-expect (demolish? "heritage") #f)

;; (define (demolish? bs) true) ; Stub

;; took template from BuildingStatus

(define (demolish? bs)
  (cond
    [(string=? bs "new") #f]
    [(string=? bs "old") #t]
    [(string=? bs "heritage") #f]
  )
)

(test)
