#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)

;; Cat is Number
;; Interp. x coordinate of cat
;; (define C1 0)
;; (define C2 (/ WIDTH 2))

#;
(define (fn-for-cat c)
  (... c))

;; Template rules used
;; - Atomic Non distinct: Number

;; ======================================
;;
;; Functions:

;; Cat -> Cat
;; increases cat x position by SPEED

(define SPEED 5)
(check-expect (next-cat 0) SPEED) ; Tests
(check-expect (next-cat 100) (+ 100 SPEED))

;; (define (next-cat c) 1) ; Stub

;; <use template from Cat>

(define (next-cat c)
  (+ c SPEED))

;; Cat -> Image
;; add CAT-IMG to MTS at proper x coordinate and CTR-Y

(define CAT-IMG (circle 30 "solid" "brown"))
(define WIDTH 1000)
(define HEIGHT 500)
(define CTR-Y (/ HEIGHT 2))
(define MTS (empty-scene WIDTH HEIGHT))
(check-expect (render-cat 100)
              (place-image CAT-IMG 100 CTR-Y MTS)) ; Tests

;; (define (render-cat c) MTS) ; Stub

;; <use template from Cat>

(define (render-cat c)
  (place-image CAT-IMG c CTR-Y MTS))


(big-bang 0
          (on-tick next-cat)
          (to-draw render-cat))

(test)
