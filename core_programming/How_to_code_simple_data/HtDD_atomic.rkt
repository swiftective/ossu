#lang racket
(require test-engine/racket-tests)

;; Data Definitions:


;; Cityname is String
;; interp. the name of a city
(define CN! "Boston")
(define CN2 "Vancouver")

#;
(define (fn-for-city-name)
  (... cn))

;; Template rules used:
;;    atomic non-distinct; String

;; Functions:

;; CityName -> Boolean
;; produce true if the given city is Hogsmeade
(check-expect (best? "Boston") #f)
(check-expect (best? "Hogsmeade") #t)

;; (define (best? cn) false) ; stub

;; took Template from CityName

;; (define (best? cn)
;;   (if (string=? cn "Hogsmeade") #t #f))

(define (best? cn) (string=? cn "Hogsmeade"))

(test) ; Running the tests
