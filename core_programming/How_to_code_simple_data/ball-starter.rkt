#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)


;; ===========================
;; Constants
(define WIDTH 1000)
(define HEIGHT 500)
(define MTS (empty-scene WIDTH HEIGHT))
(define COLOR "red")
(define SCALE 10)

;; ===========================
;; Data Definition:
(define-struct ball (x y s render?) #:transparent)
;; Ball is (make-ball Natural[0, WIDTH] Natural[0, HEIGHT] Natural Boolean)
;; Interp. (make-ball x y s render?) is a ball with
;;         x as the x coordinate
;;         y as the y coordinate
;;         s as the size of the ball
;;         render? is whether to render the ball or not
(define B1 (make-ball 10 20 20 true))
(define B2 (make-ball 10 20 20 false))
(define B3 (make-ball 0 0 0 true))

#;
(define (fn-for-ball b)
  (... (ball-x b)
       (ball-y b)
       (ball-s b)
       (ball-render? b)))

;; Template rules used
;; - Compound: 4 fields

;; ==========================
;; Functions:

;; Ball -> Ball
;; start the world with (main (make-ball 0 0 0 false))
;;
(define (main b)
  (big-bang b                               ; Ball
            (on-tick   next-ball)           ; Ball -> Ball
            (to-draw   render-ball)         ; Ball -> Image
            (on-mouse  handle-mouse)))      ; Ball Integer Integer MouseEvent -> Ball

;; Ball -> Ball
;; increase the ball-s by SCALE
(check-expect (next-ball B1)
        (make-ball
          (ball-x B1)
          (ball-y B1)
          (+ (ball-s B1) SCALE)
          (ball-render? B1)))
;; (define (next-ball b) b) ;stub

; took template from Ball

(define (next-ball b)
        (make-ball
          (ball-x b)
          (ball-y b)
          (+ (ball-s b) SCALE)
          (ball-render? b)))

;; Ball -> Image
;; render the appropiate image on MTS
(check-expect
  (render-ball B1)
  (place-image (circle (ball-s B1) "solid" COLOR) (ball-x B1) (ball-y B1) MTS))
(check-expect (render-ball B2) MTS)
;; (define (render-ball b) MTS) ;stub

; took template from Ball

(define (render-ball b)
  (if (ball-render? b)
    (place-image (circle (ball-s b) "solid" COLOR) (ball-x b) (ball-y b) MTS)
    MTS
  )
)

;; Ball Integer Integer MouseEvent -> Ball
;; reset the ball based on the x, y coordinates of the mouse click
(check-expect (handle-mouse B1 2 30 "button-up") B1) ; Tests
(check-expect (handle-mouse B2 2 30 "button-down") (make-ball 2 30 0 true))
(check-expect (handle-mouse B3 2 30 "button-down") (make-ball 2 30 0 true))
;; (define (handle-mouse b x y me) b) ;stub

#;
(define (handle-mouse b x y me)
  (cond [(mouse=? me "button-down") (... b x y)]
        [else
         (... b x y)]))

(define (handle-mouse b x y me)
  (cond [(mouse=? me "button-down") (make-ball x y 0 true)]
        [else b]))

(main (make-ball 0 0 0 false))

(test)
