#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)

;; arrange-images-starter.rkt (problem statement)

;
; PROBLEM:
;
; In this problem imagine you have a bunch of pictures that you would like to
; store as data and present in different ways. We'll do a simple version of that
; here, and set the stage for a more elaborate version later.

;; Constants:
(define BLANK (square 0 "solid" "white"))

;; Data Definitions:

; (A) Design a data definition to represent an arbitrary number of images.

;; ListOfImages is one of:
;; - empty
;; - (cons Image ListOfImages)
;; Interp. list of images
;; (define LOI1 empty)
;; (define LOI2 (cons (square 10 "solid" "black") empty))

#;
(define (fn-for-loi loi)
  (cond
    [(empty? loi) (...)]
    [else
     (... (first loi)
          (fn-for-loi (rest loi)))]
    )
  )

;; Template rules used
;; - one of: 3 cases
;; - Atomic distinct: empty
;; - Compound: (cons Image ListOfImages)
;; - self-reference: (rest loi) is ListOfImages

;; Functions:

; (B) Design a function called arrange-images that consumes an arbitrary number
;     of images and lays them out left-to-right in increasing order of size.

;; ListOfImages -> Image
;; lay out images left to right in increasing order of size
;; sort images in increasing order in size
;; then lay them out left to right

(check-expect (arrange-images (cons (square 10 "solid" "black") empty))
              (beside (square 10 "solid" "black")
                      BLANK))
(check-expect (arrange-images (cons (square 100 "solid" "black")
                                   (cons (square 10 "solid" "black") empty)))
              (beside (square 10 "solid" "black")
                      (beside (square 100 "solid" "black") BLANK)))

;; (define (arrange-images loi) BLANK) ; Stub

(define (arrange-images loi)
  (layout-images (sort-images loi)))


;; ListOfImages -> Image
;; place images beside each other in order of list
(check-expect (layout-images empty) BLANK) ; Tests
(check-expect (layout-images (cons (square 10 "solid" "black") empty))
              (beside (square 10 "solid" "black")
                      BLANK))
(check-expect (layout-images (cons (square 100 "solid" "black")
                                   (cons (square 10 "solid" "black") empty)))
              (beside (square 100 "solid" "black")
                      (beside (square 10 "solid" "black") BLANK)))

;; (define (layout-images loi) BLANK) ;stub

;; took template from ListOfImages

(define (layout-images loi)
  (cond
    [(empty? loi) BLANK]
    [else
     (beside (first loi)
             (layout-images (rest loi)))]
    )
  )


;; ListOfImages -> ListOfImages
;; sort images in the list in increasing order in size
(check-expect (sort-images empty) empty)
(check-expect (sort-images (cons (square 100 "solid" "black")
                                 (cons (square 10 "solid" "black") empty)))
              (cons (square 10 "solid" "black")
                    (cons (square 100 "solid" "black") empty)))

;; (define (sort-images loi) loi) ;stub

;; took template from ListOfImages

(define (sort-images loi)
  (cond
    [(empty? loi) empty]
    [else
     (insert (first loi)
             (sort-images (rest loi)))]
    )
  )

;; Image ListOfImages -> ListOfImages
;; insert img in proper place in lst (in increasing order of size)
;; ASSUME: lst is already sorted

(check-expect (insert BLANK empty) (cons BLANK empty))
(check-expect (insert (square 100 "solid" "black")
                      (cons (square 10 "solid" "black") empty))
              (cons (square 10 "solid" "black")
                    (cons (square 100 "solid" "black") empty)))

;; (define (insert img lst) lst) ;stub

(define (insert img lst)
  (cond
    [(empty? lst) (cons img empty)]
    [else
      (if (larger? img (first lst))
        (cons (first lst)
              (insert img (rest lst)))
        (cons img (rest lst)))]
    )
  )

;; Image Image -> Boolean
;; produce true if the first image is bigger than the second image else false

(check-expect (larger? (square 100 "solid" "black") (square 10 "solid" "black")) true)
(check-expect (larger? (square 10 "solid" "black") (square 100 "solid" "black")) false)

;; (define (bigger? img img2) true) ;stub


(define (larger? img img2)
  (>
    (* (image-width img) (image-height img))
    (* (image-width img2) (image-height img2))))


(test)
