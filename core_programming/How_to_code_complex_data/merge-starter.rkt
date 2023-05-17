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


;; .-----------------------------.---------.----------------------------------------.
;; |        lsta / lstb          |  empty  |       (cons Number ListOfNumber)       |
;; :-----------------------------+---------+----------------------------------------:
;; | empty                       |  empty  |  lstb                                  |
;; :-----------------------------+---------+----------------------------------------:
;; | (cons Number ListOfNumber)  |  lsta   |  (append fn((first lsta) (rest lsta))  |
;; :                             |         |    fn-for-los(first lsta) (rest lstb)) :
;; '-----------------------------'---------'----------------------------------------'

;; ListOfNumbers ListOfNumbers -> ListOfNumbers
;; merge both list of numbers, and make sure the return a sorted list of numbers

(check-expect (merge empty empty) empty)
(check-expect (merge (list 1 2 3) empty) (list 1 2 3))
(check-expect (merge  empty (list 1 2 3)) (list 1 2 3))
(check-expect (merge (list 1 4 6) (list 2 5 8))
              (list 1 2 4 5 6 8))

;; (define (merge lsta lstb) empty) ; Stub

(define (merge lsta lstb)
  (cond
    [(and (empty? lsta) (empty? lstb)) empty]
    [(empty? lsta) lstb]
    [(empty? lstb) lsta]
    [else
      (append
        (sort-two-nums (first lsta) (first lstb))
        (merge (rest lsta) (rest lstb)))]))

;; Number Number -> ListOfNumber
;; produce a sorted list of two consumed numbers

(check-expect (sort-two-nums 1 2) (list 1 2)) ; Tests
(check-expect (sort-two-nums 3 1) (list 1 3))

;; (define (sort-two-nums n1 n2) empty) ; Stub

(define (sort-two-nums n1 n2)
  (if
    (> n1 n2)
    (list n2 n1)
    (list n1 n2)))

(test)
