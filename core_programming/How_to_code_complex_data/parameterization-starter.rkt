#lang racket
(require test-engine/racket-tests)

(* pi (sqr 4)) ;area of circle radius 4
(* pi (sqr 6)) ;area of circle radius 6


;; ====================

;; ListOfString -> Boolean
;; produce true if los includes "UBC"
(check-expect (contains-ubc? empty) false)
(check-expect (contains-ubc? (cons "McGill" empty)) false)
(check-expect (contains-ubc? (cons "UBC" empty)) true)
(check-expect (contains-ubc? (cons "McGill" (cons "UBC" empty))) true)

;(define (contains-ubc? los) false) ;stub

;<template from ListOfString>

(define (contains-ubc? los)
  (contains? "UBC" los))

;; ListOfString -> Boolean
;; produce true if los includes "McGill"
(check-expect (contains-mcgill? empty) false)
(check-expect (contains-mcgill? (cons "UBC" empty)) false)
(check-expect (contains-mcgill? (cons "McGill" empty)) true)
(check-expect (contains-mcgill? (cons "UBC" (cons "McGill" empty))) true)

;(define (contains-mcgill? los) false) ;stub

;<template from ListOfString>

(define (contains-mcgill? los)
  (contains? "McGill" los))

;; String ListOfString -> Boolean
;; produce true if String is present in the ListOfString, else false

;; (define (contains? str los) true)

(check-expect (contains? "UBC" (cons "UBC" (cons "McGill" empty))) true)
(check-expect (contains? "UBC" (cons "McGill" empty)) false)
(check-expect (contains? "UBC" (cons "UBC" empty)) true)
(check-expect (contains? "UBC" empty) false)
(check-expect (contains? "UBC" (cons "McGill" (cons "UBC" empty))) true)
(check-expect (contains? "UBC" (cons "UBC" empty)) true)
(check-expect (contains? "UBC" (cons "McGill" empty)) false)
(check-expect (contains? "UBC" empty) false)
(check-expect (contains? "Toronto" empty) false)

(define (contains? str los)
  (cond
    [(empty? los) false]
    [else
     (if (string=? (first los) str)
         true
         (contains? str (rest los)))]))


;; ====================

;; ListOfNumber -> ListOfNumber
;; produce list of sqr of every number in lon
(check-expect (squares empty) empty)
(check-expect (squares (list 3 4)) (list 9 16))

;(define (squares lon) empty) ;stub

;<template from ListOfNumber>

(define (squares lon)
  (map2 (lambda (x) (sqr x)) lon))

;; ListOfNumber -> ListOfNumber
;; produce list of sqrt of every number in lon
(check-expect (square-roots empty) empty)
(check-expect (square-roots (list 9 16)) (list 3 4))

;(define (square-roots lon) empty) ;stub

;<template from ListOfNumber>

(define (square-roots lon)
  (map2 sqrt lon))


;; given fn and (list n0 n1 n2 ...) produce (list (fn n0) (fn n1) (fn n2) ...)
(define (map2 fn lon)
  (cond [(empty? lon) empty]
        [else
         (cons (fn (first lon))
               (map2 fn (rest lon)))]))

;; ====================

;; ListOfNumber -> ListOfNumber
;; produce list with only positive? elements of lon
(check-expect (positive-only empty) empty)
(check-expect (positive-only (list 1 -2 3 -4)) (list 1 3))

;(define (positive-only lon) empty) ;stub

;<template from ListOfNumber>

(define (positive-only lon)
  (filter2 positive? lon))


;; ListOfNumber -> ListOfNumber
;; produce list with only negative? elements of lon
(check-expect (negative-only empty) empty)
(check-expect (negative-only (list 1 -2 3 -4)) (list -2 -4))

;(define (negative-only lon) empty) ;stub

;<template from ListOfNumber>

(define (negative-only lon)
  (filter2 negative? lon))


;; Function ListOfNumber -> ListOfNumber
;; given fn and lon
;; produce a filterd lon with each number producing true when passed to fn
(define (filter2 fn lon)
  (cond [(empty? lon) empty]
        [else
          (if (fn (first lon))
            (cons (first lon)
                  (filter2 fn (rest lon)))
            (filter2 fn (rest lon)))]))

(test)
