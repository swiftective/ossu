#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)

;; wide-only-starter.rkt

(define I1 (rectangle 10 20 "solid" "red"))
(define I2 (rectangle 30 20 "solid" "yellow"))
(define I3 (rectangle 40 50 "solid" "green"))
(define I4 (rectangle 60 50 "solid" "blue"))
(define I5 (rectangle 90 90 "solid" "orange"))

;
; PROBLEM:
;
; Use the built in version of filter to design a function called wide-only
; that consumes a list of images and produces a list containing only those
; images that are wider than they are tall.

(check-expect (wide-only (list I1 I2 I3 I4 I5)) (list I2 I4))

(define (wide-only loi)
  (filter
    (λ (i) (> (image-width i) (image-height i)))
    loi))

(test)
