#lang racket
(require test-engine/racket-tests)


;; double-starter.rkt

;; =================
;; Data definitions:

;
; Remember the data definition for a list of numbers we designed in Lecture 5f:
; (if this data definition does not look familiar, please review the lecture)
;


;; ListOfNumber is one of:
;;  - empty
;;  - (cons Number ListOfNumber)
;; interp. a list of numbers
(define LON1 empty)
(define LON2 (cons 60 (cons 42 empty)))
#;
(define (fn-for-lon lon)
  (cond [(empty? lon) (...)]
        [else
         (... (first lon)
              (fn-for-lon (rest lon)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Number ListOfNumber)
;;  - self-reference: (rest lon) is ListOfNumber

;; =================
;; Functions:

;
; PROBLEM:
;
; Design a function that consumes a list of numbers and doubles every number
; in the list. Call it double-all.

;; ListOfNumber -> ListOfNumber
;; double all the numbers in the list

(check-expect (double empty) empty) ; Tests
(check-expect (double (cons 10 empty)) (cons (* 2 10) empty))
(check-expect (double (cons 10 (cons 20 empty))) (cons (* 2 10) (cons (* 2 20) empty)))

;; (define (double lon) lon) ; Stub

;; took template from ListOfNumber

(define (double lon)
  (cond [(empty? lon) empty]
        [else
          (cons (* 2 (first lon))
                (double (rest lon)))]))

(test)
