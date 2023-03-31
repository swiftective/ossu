#lang racket
(require test-engine/racket-tests)

;; SeatNum is Natural[1, 32]
;; interp. Seat Number in a row, 1 and 32 are aisle seats
(define SN1 1) ;aisle
(define SN2 2) ;middle
(define SN3 3) ;aisle


(define (fn-for-seat-num sn)
  (... sn))


;; Template rules used:
;;      - atomic non-distinct: Natural[1, 32]
