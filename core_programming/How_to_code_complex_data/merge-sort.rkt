#lang htdp/asl
(require racket/list)
(require test-engine/racket-tests)

;; merge sort

;; (listof Number) -> (listof Number)
;; produce sorted list in ascending order using merge sort

(check-expect (merge-sort empty) empty)
(check-expect (merge-sort (list 2)) (list 2))
(check-expect (merge-sort (list 1 2)) (list 1 2))
(check-expect (merge-sort (list 3 4 3 2 3)) (list 2 3 3 3 4))
(check-expect (merge-sort (list 6 5 3 1 8 7 2 4)) (list 1 2 3 4 5 6 7 8))

;; (define (merge-sort lon) lon)


(define (merge-sort lon)
  (cond
    [(empty? lon) empty]
    [(empty? (rest lon)) lon]
    [else
     (merge
      (merge-sort (take lon (quotient (length lon) 2)))
      (merge-sort (drop lon (quotient (length lon) 2))))]))


;;   ;; (listof Number) Natural -> (listof Number)
;;   ;; produce list of first n elements of lon / list after dropping first n elements
;;   ;; Assume that lon has atleast n elements
;;
;;   (check-expect (take empty 0) empty)
;;   (check-expect (take (list 1 2 3 4) 0) empty)
;;   (check-expect (take (list 1 2 3 4) 2) (list 1 2))
;;   (check-expect (drop empty 0) empty)
;;   (check-expect (drop (list 1 2 3 4) 0) (list 1 2 3 4))
;;   (check-expect (drop (list 1 2 3 4) 2) (list 3 4))
;;
;;   (define (take lon n) empty)
;;   (define (drop lon n) empty)

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

(define (merge lona lonb)
  (local
    [(define (merge lona lonb acc)
       (cond
         [(empty? lona) (append (reverse acc) lonb)]
         [(empty? lonb) (append (reverse acc) lona)]
         [else
          (if (< (first lona) (first lonb))
              (merge (rest lona) lonb
                     (cons (first lona) acc))
              (merge lona (rest lonb)
                     (cons (first lonb) acc)))]))]
    (merge lona lonb empty)))


(test)
