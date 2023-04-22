#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)

;; water-balloon-starter.rkt

; PROBLEM:
; ;
; ; In this problem, we will design an animation of throwing a water balloon.
; ; When the program starts the water balloon should appear on the left side
; ; of the screen, half-way up.  Since the balloon was thrown, it should
; ; fly across the screen, rotating in a clockwise fashion. Pressing the
; ; space key should cause the program to start over with the water balloon
; ; back at the left side of the screen.
; ;
; ; NOTE: Please include your domain analysis at the top in a comment box.
; ;
; ; NOTE: The rotate function wants an angle in degrees as its first
; ; argument. By that it means Number[0, 360). As time goes by your balloon
; ; may end up spinning more than once, for example, you may get to a point
; ; where it has spun 362 degrees, which rotate won't accept.
; ; The solution to that is to use the modulo function as follows:
; ; (rotate (modulo ... 360) (text "hello" 30 "black"))
; ; where ... should be replaced by the number of degrees to rotate.
; ;
; ; NOTE: It is possible to design this program with simple atomic data,
; ; but we would like you to use compound data.

;; ===========================
;; Constants
(define SPEED 3)
(define WIDTH 400)
(define HEIGHT 200)
(define CTR-Y (/ HEIGHT 2))
(define WATER-BALLOON (ellipse 45 60 "solid" "yellow"))
(define MTS (empty-scene WIDTH HEIGHT))

;; ===========================
;; Data Defnitions
(define-struct balloon (x a) #:transparent)
;; Balloon is (make-balloon Natural Natural)
;; Interp. (make-balloon x a) is a balloon with
;;                      x as x coordinate
;;                      a as the angle of rotation
(define B1 (make-balloon 20 200))
(define B2 (make-balloon 0 40999))
(define B3 (make-balloon WIDTH 40999))

#;
(define (fn-for-balloon b)
  (... (balloon-x b)
       (balloon-a b)))

;; Template rules used
;; - Compund: 2 fields

;; ===========================
;; Functions:

;; Balloon -> Balloon
;; start the world with (main (make-balloon 0 0))
;;
(define (main b)
  (big-bang b                           ; Balloon
            (on-tick   next-balloon)    ; Balloon -> Balloon
            (to-draw   render)          ; Balloon -> Image
            (on-key    handle-key)))    ; Balloon KeyEvent -> Balloon


;; Balloon -> Balloon
;; increase balloon x by SPEED and change balloon a by 10
(check-expect (next-balloon B1) (make-balloon (+ (balloon-x B1) SPEED) (+ (balloon-a B1) 10)))
(check-expect (next-balloon B2) (make-balloon (+ (balloon-x B2) SPEED) (+ (balloon-a B2) 10)))
(check-expect (next-balloon B3) (make-balloon (+ (balloon-x B3) SPEED) (+ (balloon-a B3) 10)))
;; (define (next-balloon b) b) ;stub

(define (next-balloon b)
  (make-balloon (+ (balloon-x b) SPEED) (+ (balloon-a b) 10)))

;; Balloon -> Image
;; produce appropiate balloon image which is
;; rotated (balloon-a b) and placed on MTS at (balloon-x b) and CTR-Y
(check-expect (render B1)
      (place-image (rotate-image WATER-BALLOON (balloon-a B1)) (balloon-x B1) CTR-Y MTS))
(check-expect (render B2)
      (place-image (rotate-image WATER-BALLOON (balloon-a B2)) (balloon-x B2) CTR-Y MTS))
;; (define (render b) MTS) ;stub

; took template from Balloon

(define (render b)
     (place-image (rotate-image WATER-BALLOON (balloon-a b)) (balloon-x b) CTR-Y MTS))

;; Image Natural -> Image
;; produce a rotated image by the angle provided
(check-expect (rotate-image WATER-BALLOON 10) (rotate (modulo 10 360) WATER-BALLOON))
;; (define (rotate-image img a) img) ;stub

; took template from Balloon

(define (rotate-image img a)
  (rotate (modulo a 360) img))

;; Balloon KeyEvent -> Balloon
;; reset the balloon if the space bar is pressed
(check-expect (handle-key B1 " ") (make-balloon 0 0))
(check-expect (handle-key B1 "a") B1)
;; (define (handle-key c ke) c) ;stub

#;
(define (handle-key b ke)
  (cond [(key=? ke " ") (... b)]
        [else
         (... b)]))

(define (handle-key b ke)
  (cond [(key=? ke " ") (make-balloon 0 0)]
        [else b]))

(main (make-balloon 0 0))

(test)
