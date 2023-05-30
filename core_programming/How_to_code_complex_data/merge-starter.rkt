#lang racket
(require test-engine/racket-tests)

;; merge-starter.rkt

; Problem:
;
; Design the function merge. It consumes two lists of numbers, which it assumes are
; each sorted in ascending order. It produces a single list of all the numbers,
; also sorted in ascending order.
;
; Your solution should explicitly show the cross product of type comments table,
; filled in with the values in each case. Your final function should have a cond
; with 3 cases. You can do this simplification using the cross product table by
; recognizing that there are subtly equal answers.
;
; Hint: Think carefully about the values of both lists. You might see a way to
; change a cell content so that 2 cells have the same value.


;; ListOfNumbers ListOfNumbers -> ListOfNumbers
;; merge both list of numbers, and make sure the return a sorted list of numbers

(check-expect (merge empty empty) empty)
(check-expect (merge (list 1 2 3) empty) (list 1 2 3))
(check-expect (merge  empty (list 1 2 3)) (list 1 2 3))
(check-expect (merge (list 1 4 6) (list 2 5 8))
              (list 1 2 4 5 6 8))
(check-expect (merge (list 1 1 1 1) (list 2 3 4 5))
              (list 1 1 1 1 2 3 4 5))
(check-expect (merge (list 1 1 1 1 10) (list 2 3 4 5))
              (list 1 1 1 1 2 3 4 5 10))
(check-expect (merge empty (list 1 2 3)) (list 1 2 3))

;; (define (merge lsta lstb) empty) ; Stub

(define (merge lsta lstb)
  (local
    [(define (merge lsta lstb acc)
       (cond
         [(empty? lsta) (append (reverse acc) lstb)]
         [(empty? lstb) (append (reverse acc) lsta)]
         [else
          (if (< (first lsta) (first lstb))
              (merge (rest lsta) lstb
                     (cons (first lsta) acc))
              (merge lsta (rest lstb)
                     (cons (first lstb) acc)))]))]
    (merge lsta lstb empty)))


(test)
