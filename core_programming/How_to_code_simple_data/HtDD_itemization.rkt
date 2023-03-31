#lang racket

;; CountDown is one of:
;; - false
;; - Natural[1, 10]
;; - "complete"
;; interp.
;;    false mean countdown has not yet started
;;    Natural[1, 10] means countdown is running and how many seconds left
;;    "complete" means countdown is over
(define CD1 false)
(define CD2 10)
(define CD3 1)
(define CD4 "complete")

#;
(define fun-for-count-down c
  (cond
    [(false? c) (...)]
    [(and (number? c) (>= c 1) (<= c 10)) (... x)]
    [else (...)]
  )
)

;; Templates rules used:
;; one of: 3 cases
;; - atomic distinct: false
;; - atomic non-distinct: Natural[1, 10]
;; - atomic distinct: "complete"
