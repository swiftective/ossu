#lang racket
(require test-engine/racket-tests)

(require 2htdp/image)

;; image-list-starter.rkt

;; =================
;; Data definitions:

;
; PROBLEM A:
;
; Design a data definition to represent a list of images. Call it ListOfImage.


;; ListOfImage is one of:
;; - empty
;; - (cons Image ListOfImage)
;; Interp. a list of images
;; (define LOI1 empty)
;; (define LOI2 (cons (square 10 "solid" "red") empty))

#;
(define (fn-for-loi loi)
  (cond
    [(empty? loi) (...)]
    [else
      (... (first loi)
          (fn-for-loi (rest loi)))
    ]
  )
)

;; Template rules used
;; - Atomic distict: empty
;; - Compound: (cons Image ListOfImage)
;; - self reference: (rest loi) is ListOfImage

;; =================
;; Functions:

;
; PROBLEM B:
;
; Design a function that consumes a list of images and produces a number
; that is the sum of the areas of each image. For area, just use the image's
; width times its height.

;; ListOfImage -> Number
;; produce the sum of every image's area in the consumed list

(check-expect (sum-of-area empty) 0) ; Tests
(check-expect (sum-of-area (cons (square 10 "solid" "red") empty)) 100)
(check-expect (sum-of-area (cons (square 10 "solid" "red")
                                 (cons (square 10 "solid" "red") empty))) 200)

;; (define (sum-of-area loi) 10) ; Stub

;; took template from ListOfImage

(define (sum-of-area loi)
  (cond
    [(empty? loi) 0]
    [else
      (+ (* (image-height (first loi))
            (image-width (first loi)))
          (sum-of-area (rest loi)))
    ]
  )
)

(test)
