#lang racket
(require test-engine/racket-tests)

;; Data Definitions:

;;Reservation is one of:
;; Natural [1, 100]
;; "standby"
;; interp.
;; - Natural [1, 100] means a guaranteed seat for dinner where the number
;; corresponds to which reservation (not which seat). means a standby spot,
;; if all the reservations show up this person will not be seated. "standby"
;; (define R1 70)
;; (define R2 "standby")

#;
(define (fn-for-reservation r)
  (cond [ (number? r) (... r) ]
        [ else (...) ]
  )
)

;; Template rules used:
;; - one of: 2 cases
;; - atomic non-distinct: Natural [1, 100]
;; - atomic distinct: "standby"


(test)
