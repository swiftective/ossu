#lang racket
(require test-engine/racket-tests)


;; boolean-list-starter.rkt

;; =================
;; Data definitions:

;
; PROBLEM A:
;
; Design a data definition to represent a list of booleans. Call it ListOfBoolean.


;; ListOfBoolean is one of:
;; - empty
;; - (cons Boolean ListOfBoolean)
;; Interp. a list of booleans
;; (define LOB1 empty)
;; (define LOB2 (cons true empty))
;; (define LOB2 (cons true (cons false empty)))

#;
(define (fn-for-lob lob)
  (cond
    [(empty? lob) (...)]
    [else
      (... (first lob)
           (fn-for-lob (rest lob)))
    ]
  )
)

;; Template rules used
;; - one of: 2 cases
;; - Atomic distinct: empty
;; - compound: (cons Boolean ListOfBoolean)
;; - self reference: (rest lob) is ListOfBoolean

;; =================
;; Functions:

;
; PROBLEM B:
;
; Design a function that consumes a list of boolean values and produces true
; if every value in the list is true. If the list is empty, your function
; should also produce true. Call it all-true?

;; ListOfBoolean -> Boolean
;; produce true if every value in the list is true, or produce true if list if empty

(check-expect (all-true? empty) true) ; Tests
(check-expect (all-true? (cons true empty)) true)
(check-expect (all-true? (cons false empty)) false)
(check-expect (all-true? (cons true (cons true empty))) true)
(check-expect (all-true? (cons true (cons false empty))) false)

;; (define (all-true? lob) true) ; Stub

;; took template from ListOfBoolean

(define (all-true? lob)
  (cond
    [(empty? lob) true]
    [else
      (if (false? (first lob))
        false
       (all-true? (rest lob)))
    ]
  )
)


(test)
