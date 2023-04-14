#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)

;; Data Definitions:

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
(define (fun-for-count-down c)
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

;; Functions:

;; CountDown -> Image
;; produce nice image of current state of countdown

(check-expect (countdown->image false) (square 0 "solid" "white")) ; Tests
(check-expect (countdown->image 5) (text (number->string 5) 24 "black"))
(check-expect (countdown->image "complete") (text "Happy New Year!!!" 24 "black"))

;; (define (countdown-to-image c) (square 0 "solid" "white")) ; Stub

;; <use template from CountDown>

(define (countdown->image c)
  (cond
    [(false? c) (square 0 "solid" "white")]
    [(and (number? c) (>= c 1) (<= c 10)) (text (number->string c) 24 "black")]
    [else (text "Happy New Year!!!" 24 "black")]
  )
)

(test)
