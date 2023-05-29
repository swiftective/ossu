#lang racket
(require test-engine/racket-tests)

;; dropn-starter.rkt

;
; PROBLEM:
;
; Design a function that consumes a list of elements lox and a natural number
; n and produces the list formed by dropping every nth element from lox.
;
; (dropn (list 1 2 3 4 5 6 7) 2) should produce (list 1 2 4 5 7)


;; (listof X) Natural -> (listof X)
;; produce the list by dropping every nth element from lox

(check-expect (dropn (list 1 2 3 4 5 6 7) 2) (list 1 2 4 5 7))
(check-expect (dropn (list "a" "b" "c") 2) (list "a" "b")) ; Tests

;; (define (dropn lst n) lst) ; Stub

#;
(define (dropn lst n)
  (local
    [(define (dropn lst acc)
       (cond
         [(empty? lst) empty]
         [else
          (if (zero? acc)
              (dropn (rest lst) n)
              (cons
               (first lst)
               (dropn (rest lst) (sub1 acc))))]))]
    (dropn lst n)))

(define (dropn lst n) ; Tail recursive
  (local
    [(define (dropn lsta lstb acc)
       (cond
         [(empty? lsta) lstb]
         [else
          (if (zero? acc)
              (dropn (rest lsta) lstb n)
              (dropn (rest lsta)
                     (append  lstb (list (first lsta)))
                     (sub1 acc)))]))]
    (dropn lst empty n)))

(test)
