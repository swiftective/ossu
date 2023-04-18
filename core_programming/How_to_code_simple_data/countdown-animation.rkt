#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)

;; PROBLEM:
;;
;; Design an animation of a simple countdown.
;;
;; Your program should display a simple countdown, that starts at ten, and
;; decreases by one each clock tick until it reaches zero, and stays there.
;;
;; To make your countdown progress at a reasonable speed, you can use the
;; rate option to on-tick. If you say, for example,
;; (on-tick advance-countdown 1) then big-bang will wait 1 second between
;; calls to advance-countdown.
;;
;; Remember to follow the HtDW recipe! Be sure to do a proper domain
;; analysis before starting to work on the code file.
;;
;; Once you are finished the simple version of the program, you can improve
;; it by reseting the countdown to ten when you press the spacebar.

;; =================================================================
;; Constants:

(define DURATION 1)
(define COUNT 10)
(define DECREMENT 1)
(define WIDTH 600)
(define HEIGHT 400)
(define CTR-Y (/ HEIGHT 2))
(define CTR-X (/ WIDTH 2))
(define MTS (empty-scene WIDTH HEIGHT))

;; ================================================================
;; Data definitions:

;; CountDown is Natural[0, COUNT]
;; Interp. CountDown is running and how many seconds are left
;; (define CD1 1)
;; (define CD1 5)

#;
(define (fn-for-countdown cd)
  (... cd))

;; Template rules used
;; - Atomic non-distint: Natural[0, COUNT]

;;=================================================================
;; Functions:

;; CountDown -> CountDown
;; start the world with (main COUNT)

(define (main cd)
  (big-bang cd                                            ; CountDown
            (on-tick   decrease-countdown DURATION)       ; CountDown -> CountDown
            (to-draw   render)                            ; CountDown -> Image
            (on-key    handle-key)))                      ; CountDown KeyEvent -> CountDown


;; CountDown -> CountDown
;; Decrement the countdown by DECREMENT,
;; unless the input is 0, the return 0

(check-expect (decrease-countdown 10) (- 10 DECREMENT)) ; Tests
(check-expect (decrease-countdown 0) 0)

;; (define (decrease-countdown cd) 5) ; Stub

;; <use template from CountDown>

(define (decrease-countdown cd)
  (if (>= cd DECREMENT) (- cd DECREMENT) 0))

;; CountDown -> Image
;; render the countdown image at appropiate time on MTS

(check-expect (render 3)
  (place-image
    (text (number->string 3) 100 "black") CTR-X CTR-Y MTS)) ; Tests

(check-expect (render COUNT)
  (place-image
    (text (number->string COUNT) 100 "black") CTR-X CTR-Y MTS))

;; (define (render cd) MTS) ; Stub

;; <use template from CountDown>

(define (render cd)
  (place-image
    (text (number->string cd) 100 "black") CTR-X CTR-Y MTS))

;; CountDown KeyEvent -> CountDown
;; reset the counter if space key is pressed

(check-expect (handle-key 10 " ") COUNT) ; Tests
(check-expect (handle-key 10 "a") 10)
(check-expect (handle-key 0 " ") COUNT)
(check-expect (handle-key 0 "a") 0)

;; (define (handle-key c ke) c) ; Stub

#;
(define (handle-key cd ke)
  (cond [(key=? ke " ") (... cd)]
        [else
         (... cd)]))


(define (handle-key cd ke)
  (cond [(key=? ke " ") COUNT]
        [else cd]))

#| (main COUNT) |#

(test)
