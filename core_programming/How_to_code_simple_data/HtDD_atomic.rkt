#lang racket
(require test-engine/racket-tests)

;; Data Definitions:

;; Cityname is String                 ; Type comment
;; interp. the name of a city         ; Interpretation
(define CN1 "Boston")                 ; Examples 1
(define CN2 "Vancouver")              ; Examples 2

#;
(define (fn-for-city-name)            ; Template
  (... cn))

;; Template rules used:               ; Template rules
;;    atomic non-distinct; string

;; Functions:

;; CityName -> Boolean
;; produce true if the given city is Hogsmeade
(check-expect (best? "Boston") #f)
(check-expect (best? "Hogsmeade") #t)

;; (define (best? cn) false) ; stub

;; took Template from CityName

;; (define (best? cn)                         ; Function body 1
;;   (if (string=? cn "Hogsmeade") #t #f))

(define (best? cn) (string=? cn "Hogsmeade")) ; Function body 2

(test) ; Running the tests
