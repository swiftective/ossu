#lang racket
(require test-engine/racket-tests)


;; skipn-starter.rkt

;
; PROBLEM:
;
; Design a function that consumes a list of elements lox and a natural number
; n and produces the list formed by including the first element of lox, then
; skipping the next n elements, including an element, skipping the next n
; and so on.
;
;  (skipn (list "a" "b" "c" "d" "e" "f") 2) should produce (list "a" "d")

;; (listof X) -> (listof X)
;; produce the list formed by including the first element of lox, then
;; skipping the next n elements, including an element ....

(check-expect (skipn (list "a" "b" "c" "d" "e" "f") 2)
              (list "a" "d"))

(check-expect (skipn (list "a" "b" "c") 1)
              (list "a" "c"))

(check-expect (skipn (list "a" "b" "c" "d" "e" "f") 3)
              (list "a" "e"))

(check-expect (skipn (list "a" "b" "c" "d" "e" "f") 0)
              (list "a" "b" "c" "d" "e" "f"))

;; (define (skipn lsta skip) lsta) ; Stub

(define (skipn lsta skip)
  (local
    [(define (skipn lsta acc)
       (cond
         [(empty? lsta) empty]
         [else
          (if
           (zero? acc)
           (cons (first lsta)
                 (skipn (rest lsta) skip))
           (skipn (rest lsta) (sub1 acc)))]))]
    (skipn lsta 0)))


(test)
