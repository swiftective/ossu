#lang racket
(require test-engine/racket-tests)


;; designing-with-lists-1-starter.rkt

;
; PROBLEM:
;
; You've been asked to design a program having to do with all the owls
; in the owlery.
;
; (A) Design a data definition to represent the weights of all the owls.
;     For this problem call it ListOfNumber.
; (B) Design a function that consumes the weights of owls and produces
;     the total weight of all the owls.
; (C) Design a function that consumes the weights of owls and produces
;     the total number of owls.
;


;; Data definitions:



;; ListOfNumber is one of:
;; - empty
;; - (cons Number ListOfNumber)
;; Interp. each numner in the list is an owl weight in ounces
;; (define LON1 empty)
;; (define LON2 (cons 10 (cons 20 empty)))

#;
(define (fn-for-lon lon)
  (cond
    [(empty? lon) (...)]
    [else (first lon)
          (fn-for-lon (rest lon))]
  )
)

;; Template rules used
;; - Atomic distinct: empty
;; - compound: (cons NUmber ListOfNumber)
;; - self-reference: (rest lon) is ListOfNumber


;; Functions

;; ListOfNumber -> Number
;; produce total weight of owls in consumed list

(check-expect (sum empty) 0) ; Tests
(check-expect (sum (cons 10 (cons 20 empty))) (+ 10 20))
(check-expect (sum (cons 500 (cons 20 (cons 200 empty)))) (+ 500 20 200))

;; (define (sum lon) 0) ; Stub

;; <use template from ListOfNumber>

(define (sum lon)
  (cond
    [(empty? lon) 0]
    [else
      (+ (first lon)
          (sum (rest lon)))]
  )
)

;; ListOfNumber -> Number
;; produce total number of weights in consumed list

(check-expect (count empty) 0) ; Tests
(check-expect (count (cons 10 empty)) (+ 1 0))
(check-expect (count (cons 10 (cons 20 empty))) (+ 1 1 0))
(check-expect (count
                (cons 10
                      (cons 20
                            (cons 30 empty))))
              (+ 1
                 (+ 1
                    ( + 1 0))))

;; (define (count lon) 0) ; Stub

;; <use template from ListOfNumber>

(define (count lon)
  (cond
    [(empty? lon) 0]
    [else
      (+  1
          (count (rest lon)))]
  )
)


(test)
