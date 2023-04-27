#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)

;; decreasing-image-starter.rkt

;  PROBLEM:
;
;  Design a function called decreasing-image that consumes a Natural n and produces an image of all the numbers
;  from n to 0 side by side.
;
;  So (decreasing-image 3) should produce .

;; Constants:
(define FONT-SIZE 24)
(define FONT-COLOR "black")
(define SPACING (text " " FONT-SIZE FONT-COLOR))

;; Functions:

;; Natural -> Natural
;; produce a image of the naturals from n to 0 side by side

(check-expect (decreasing-image 0) (text "0" FONT-SIZE FONT-COLOR)) ; Tests
(check-expect (decreasing-image 1) (beside (text "1" FONT-SIZE FONT-COLOR)
                                           SPACING
                                           (text "0" FONT-SIZE FONT-COLOR)))

(check-expect (decreasing-image 2) (beside (text "2" FONT-SIZE FONT-COLOR)
                                           SPACING
                                           (beside (text "1" FONT-SIZE FONT-COLOR)
                                                   SPACING
                                                   (text "0" FONT-SIZE FONT-COLOR))))

;; (define (decreasing-image n) (text (number->string 0) FONT-SIZE FONT-COLOR)) ; Stub

#;
(define (decreasing-image n)
  (cond
    [(zero? n) (...)]
    [else (... n
               (decreasing-image (sub1 n)))]
  )
)

(define (decreasing-image n)
  (cond
    [(zero? n) (text (number->string 0) FONT-SIZE FONT-COLOR)]
    [else
      (beside
        (text (number->string n) FONT-SIZE FONT-COLOR)
        SPACING
        (decreasing-image (sub1 n)))]
  )
)

(test)
