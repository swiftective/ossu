#lang racket
(require test-engine/racket-tests)

;; skip1-starter.rkt

;
; PROBLEM:
;
; Design a function that consumes a list of elements and produces the list
; consisting of only the 1st, 3rd, 5th and so on elements of its input.
;
;    (skip1 (list "a" "b" "c" "d")) should produce (list "a" "c")


; (listof X) -> (listof X)
; produce list consisting of only the 1st, 3rd, 5th... elements of lox

(check-expect (skip1 empty) empty)

(check-expect
 (skip1 (list "a" "b" "c" "d"))  (list "a" "c"))

(check-expect
 (skip1 (list 1 2 3 4 5 6)) (list 1 3 5))

;; (define (skip1 lox) lox)

(define (skip1 lox)
  (local [(define (skip1 lox acc)
            (cond
              [(empty? lox) empty]
              [else
               (if (odd? acc)
                   (cons   (first lox) (skip1 (rest lox) (add1 acc)))
                   (skip1  (rest lox) (add1 acc)))]))]
    (skip1 lox 1)))

;; (listof String) -> (listof String)
;; append each string's position in the list to the front of the string to number the list
(check-expect (number-list empty) empty)
(check-expect (number-list (list "first" "second" "third"))
              (list "1: first" "2: second" "3: third"))

;; (define (number-list los) los)   ;stub

(define (number-list los)
  (local
    [(define (number-list los acc)
       (cond
         [(empty? los) empty]
         [else
          (cons
           (string-append
            (number->string acc) ": " (first los))
           (number-list (rest los) (add1 acc)))]))]
    (number-list los 1)))



(test)
