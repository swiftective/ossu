#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)

; PROBLEM:
;
; In this problem you will design another world program. In this program the changing
; information will be more complex - your type definitions will involve arbitrary
; sized data as well as the reference rule and compound data. But by doing your
; design in two phases you will be able to manage this complexity. As a whole, this problem
; will represent an excellent summary of the material covered so far in the course, and world
; programs in particular.
;
; This world is about spinning bears. The world will start with an empty screen. Clicking
; anywhere on the screen will cause a bear to appear at that spot. The bear starts out upright,
; but then rotates counterclockwise at a constant speed. Each time the mouse is clicked on the
; screen, a new upright bear appears and starts spinning.
;
; So each bear has its own x and y position, as well as its angle of rotation. And there are an
; arbitrary amount of bears.
;
; To start, design a world that has only one spinning bear. Initially, the world will start
; with one bear spinning in the center at the screen. Clicking the mouse at a spot on the
; world will replace the old bear with a new bear at the new spot. You can do this part
; with only material up through compound.
;
; Once this is working you should expand the program to include an arbitrary number of bears.

;; Constants
(define BEAR-IMG (rotate 30 (wedge 30 300 "solid" "brown")))
(define SPEED 10)
(define WIDTH 600)
(define HEIGHT 400)
(define CTR-Y (/ HEIGHT 2))
(define CTR-X (/ WIDTH 2))
(define MTS (empty-scene WIDTH HEIGHT))


;; Data Definitions:

(define-struct bear (x y a) #:transparent)
;; Bear is (make-bear Natural Natural Natural)
;; Interp. (make-bear x y a) is a bear with
;; -       x as the x coordinate
;; -       y as the y coordinate
;; -       a as the angle the bear image has rotated
;; (define B1 (make-bear 10 20 1000))
;; (define B2 (make-bear 20 40 200))

#;
(define (fn-for-bear b)
  (... (bear-x b)
       (bear-y b)
       (bear-a b)))

;; Template rules used
;; - Compound: 3 fields


;; ListOfBear is one of:
;; - empty
;; - (cons Bear ListOfBear)
;; Interp. list of bears
;; (define LOB1 empty)
;; (define LOB2 (cons (make-bear 10 20 1000) empty))
;; (define LOB3 (cons (make-bear 10 20 1000)
;;              (cons (make-bear 10 20 2000) empty)))

#;
(define (fn-for-lob lob)
  (cond
    [(empty? lob) (...)]
    [else (... (fn-for-bear (first lob))
               (fn-for-lob  (rest lob)))]
  )
)

;; Template rules used
;; - one of: 2 cases
;; - Atomic distinct: empty
;; - Compound: (cons Bear ListOfBear)
;; - reference: (first lob) is Bear
;; - self-reference: (rest lob) is ListOfBear

;; Functions:

;; ListOfBear -> ListOfBear
;; start the world with (main empty)
;;
(define (main b)
  (big-bang b                               ; Bear
            (on-tick   next-bears)          ; Bear -> Bear
            (to-draw   render)              ; Bear -> Image
            (on-mouse  handle-mouse)))      ; Bear Integer Integer MouseEvent -> Bear



;; ListOfBear -> ListOfBear
;; increase bear-a by SPEED in every bear in list
;; if empty return empty
(check-expect (next-bears empty) empty)
(check-expect (next-bears (cons (make-bear 10 20 30) empty))
                          (cons (make-bear 10 20 (+ 30 SPEED)) empty))
(check-expect (next-bears (cons (make-bear 10 20 50)
                                (cons (make-bear 20 40 100) empty)))
                          (cons (make-bear 10 20 (+ 50 SPEED))
                                (cons (make-bear 20 40 (+ 100 SPEED)) empty)))

;; (define (next-bears lob) lob) ;stub

;; took template from ListOfBear

(define (next-bears lob)
  (cond
    [(empty? lob) empty]
    [else (cons (next-bear    (first lob))
                (next-bears   (rest lob)))]
  )
)

;; Bear -> Bear
;; increase bear-a by SPEED
(check-expect (next-bear (make-bear 10 20 30)) (make-bear 10 20 (+ 30 SPEED)))
(check-expect (next-bear (make-bear 10 20 50)) (make-bear 10 20 (+ 50 SPEED)))
;; (define (next-bear b) b) ;stub

;; took template from Bear

(define (next-bear b)
  (make-bear  (bear-x b)
              (bear-y b)
              (+ (bear-a b) SPEED)))

;; ListOfBear -> Image
;; place appropiate bear images on MTS on appropiate x, y coordinates
(check-expect (render empty) MTS)
(check-expect (render (cons (make-bear 10 20 1000) empty))
              (place-image (rotate (modulo 1000 360) BEAR-IMG) 10 20 MTS))
(check-expect (render (cons (make-bear 10 20 1000)
                            (cons (make-bear 20 50 2000) empty)))
              (place-image (rotate (modulo 1000 360) BEAR-IMG) 10 20
              (place-image (rotate (modulo 2000 360) BEAR-IMG) 20 50 MTS)))

;; (define (render lob) MTS) ;stub

;; took template from ListOfBear

(define (render lob)
  (cond
    [(empty? lob) MTS]
    [else
      (place-image  (rotate-bear (first lob))
                    (bear-x (first lob))
                    (bear-y (first lob))
                    (render (rest lob)))]
  )
)


;; Bear -> Image
;; produce a rotated image of BEAR-IMG using the angle in (bear-a b)

(check-expect (rotate-bear (make-bear 10 20 0)) BEAR-IMG) ; Tests
(check-expect (rotate-bear (make-bear 10 20 1000))
              (rotate (modulo 1000 360) BEAR-IMG))

;; (define (rotate-bear b) BEAR-IMG) ; Stub

;; took template from Bear

(define (rotate-bear b)
  (rotate (modulo (bear-a b) 360) BEAR-IMG))


;; Bear -> Image

;; ListOfBear KeyEvent -> ListOfBear
;; produce a new list of bears with a new bear added with the x, y coordinates
;; of the mouse click
(check-expect (handle-mouse empty 10 20 "button-down")
              (cons (make-bear 10 20 0) empty))

(check-expect (handle-mouse empty 10 20 "button-up") empty)

(check-expect (handle-mouse (cons (make-bear 30 80 2000)
                                  (cons (make-bear 30 40 1000) empty))
                            10 20 "button-down")
              (cons (make-bear 10 20 0)
                    (cons (make-bear 30 80 2000)
                    (cons (make-bear 30 40 1000) empty))))

;; (define (handle-mouse lob x y me) lob) ;stub

(define (handle-mouse lob x y me)
  (cond [(mouse=? me "button-down") (cons (make-bear x y 0) lob)]
        [else lob]))

(main empty)

(test)
