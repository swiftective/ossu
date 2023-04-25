#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)

;; PROBLEM:
;;
;; Design an animation of a traffic light.
;;
;; Your program should show a traffic light that is red, then green,
;; then yellow, then red etc. For this program, your changing world
;; state data definition should be an enumeration.
;;
;; To make your lights change at a reasonable speed, you can use the
;; rate option to on-tick. If you say, for example, (on-tick next-color 1)
;; then big-bang will wait 1 second between calls to next-color.
;;
;; Remember to follow the HtDW recipe! Be sure to do a proper domain
;; analysis before starting to work on the code file.
;;
;; Note: If you want to design a slightly simpler version of the program,
;; you can modify it to display a single circle that changes color, rather
;; than three stacked circles.

;; ======================================
;; Constants

(define DURATION 1)
(define WIDTH 600)
(define HEIGHT 400)
(define CTR-Y (/ HEIGHT 2))
(define CTR-X (/ WIDTH 2))
(define MTS (empty-scene WIDTH HEIGHT "black"))

;; ======================================
;; Data Definitions

;; TLColor is one of:
;; "red"
;; "green"
;; "yellow"
;; Interp. the colors in a traffic light
;; <examples are redundant for enumerations>

#;
(define (fn-for-tlcolor tlc)
  (cond
    [(string=? "red" tlc)   (...)]
    [(string=? "green" tlc) (...)]
    [else (...)]
  )
)

;; Template rules used
;; - one of: 3 cases
;; - Atomic distinct value: "red"
;; - Atomic distinct value: "green"
;; - Atomic distinct value: "yellow"

;; ======================================
;; Functions:

;; TLColor -> TLColor
;; start the world with (main "red")
;;
(define (main tlc)
  (big-bang tlc                                 ; TLColor
            (on-tick   next-color DURATION)     ; TLColor -> TLColor
            (to-draw   render)))                ; TLColor -> Image


;; TLColor -> TLColor
;; produce the next color in traffic light

(check-expect (next-color "red") "green") ; Tests
(check-expect (next-color "green") "yellow")
(check-expect (next-color "yellow") "red")

;; (define (next-color tlc) "red") ; Stub

;; took template from TLColor

(define (next-color tlc)
  (cond
    [(string=? "red" tlc) "green"]
    [(string=? "green" tlc) "yellow"]
    [else "red"]
  )
)

;; TLColor -> Image
;; render image using TLColor

(check-expect (render "red") (place-image
    (circle 80 "solid" "red") CTR-X CTR-Y MTS)) ; Tests

(check-expect (render "green") (place-image
    (circle 80 "solid" "green") CTR-X CTR-Y MTS))

(check-expect (render "yellow") (place-image
    (circle 80 "solid" "yellow") CTR-X CTR-Y MTS))

;; (define (render tlc) MTS) ; Stub

(define (render tlc)
  (place-image
    (circle 80 "solid" tlc) CTR-X CTR-Y MTS))

(main "red")

(test)
