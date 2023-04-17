#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)

;; PROBLEM:
;;
;; Use the How to Design Worlds recipe to design an interactive
;; program in which a cat starts at the left edge of the display
;; and then walks across the screen to the right. When the cat
;; reaches the right edge it should just keep going right off
;; the screen.
;;
;; Once your design is complete revise it to add a new feature,
;; which is that pressing the space key should cause the cat to
;; go back to the left edge of the screen. When you do this, go
;; all the way back to your domain analysis and incorporate the
;; new feature.
;;
;; To help you get started, here is a picture of a cat, which we
;; have taken from the 2nd edition of the How to Design Programs
;; book on which this course is based.

;; A cat that walks from left to right across the screen

;; =================
;; Constants:

(define WIDTH 600)
(define HEIGHT 400)
(define CTR-Y (/ HEIGHT 2))
(define MTS (empty-scene WIDTH HEIGHT))
(define CAT-IMG (circle 30 "solid" "brown"))
(define SPEED 5)

;; =================
;; Data definitions:

;; Cat is Number
;; Interp. x position of the cat in screen coordinates
;; (define C1 0)            ;left edge
;; (define C2 (/ WIDTH 2))  ;middle
;; (define C3 WIDTH)        ;rigth edge

#;
(define (fn-for-cat c)
  (... c)
)

;; Template rules used
;; - Atmoic non distinct: Number

;; =================
;; Functions:

;; Cat -> Cat
;; start the world with (main 0)

(define (main c)
  (big-bang c                           ; Cat
            (on-tick   advance-cat)     ; Cat -> Cat
            (to-draw   render)          ; Cat -> Image
            (on-key    handle-key)      ; Cat KeyEvent -> Cat
            (on-mouse  handle-mouse)))  ; Cat MouseEvent -> Cat

;; Cat -> Cat
;; produce the next cat, by advancing it SPEED pixel(s) to right
(check-expect (advance-cat 3) (+ 3 SPEED))
;; (define (advance-cat c) 0)

;; <use template from Cat>

(define (advance-cat c)
  (+ c SPEED)
)

;; Cat -> Image
;; render the cat image at appropiate place on MTS
(check-expect (render 4)
      (place-image CAT-IMG 4 CTR-Y MTS)
)

;; (define (render c) MTS)

;; <use template from Cat>

(define (render c)
  (place-image CAT-IMG c CTR-Y MTS)
)

;; Cat KeyEvent -> Cat
;; reset cat to left edge when space key is pressed

(check-expect (handle-key 10 " ") 0) ; Tests
(check-expect (handle-key 10 "a") 10)
(check-expect (handle-key 0 " ") 0)
(check-expect (handle-key 0 "a") 0)

;; (define (handle-key c ke) 0) ; Stub

#;
(define (handle-key c ke)
  (cond [(key=? ke " ") (... c)]
        [else
         (... c)]))

(define (handle-key c ke)
  (cond [(key=? ke " ") 0]
        [else c]))

;; Cat MouseEvent -> Cat
;; change cat x coordinate based on position of mouse click

(check-expect (handle-mouse 10 2 30 "button-up") 10) ; Tests
(check-expect (handle-mouse 10 2 30 "button-down") 2)

;; (define (handle-mouse c x) 10) ; Stub

#;
(define (handle-mouse ws x y me)
  (cond [(mouse=? me "button-down") (... ws x y)]
        [else
         (... ws x y)]))

(define (handle-mouse c x _ me)
  (cond [(mouse=? me "button-down") x]
        [else c]))

(main 0) ; Starts world

(test)
