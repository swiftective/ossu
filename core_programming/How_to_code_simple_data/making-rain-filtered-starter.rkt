#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)

;; making-rain-filtered-starter.rkt

;
; PROBLEM:
;
; Design a simple interactive animation of rain falling down a screen. Wherever we click,
; a rain drop should be created and as time goes by it should fall. Over time the drops
; will reach the bottom of the screen and "fall off". You should filter these excess
; drops out of the world state - otherwise your program is continuing to tick and
; and draw them long after they are invisible.
;
; In your design pay particular attention to the helper rules. In our solution we use
; these rules to split out helpers:
;   - function composition
;   - reference
;   - knowledge domain shift
;
;
; NOTE: This is a fairly long problem.  While you should be getting more comfortable with
; world problems there is still a fair amount of work to do here. Our solution has 9
; functions including main. If you find it is taking you too long then jump ahead to the
; next homework problem and finish this later.
;
;


;; Make it rain where we want it to.

;; =================
;; Constants:

(define WIDTH  300)
(define HEIGHT 300)

(define SPEED 1)

(define DROP (ellipse 4 8 "solid" "blue"))

(define MTS (rectangle WIDTH HEIGHT "solid" "light blue"))

;; =================
;; Data definitions:

(define-struct drop (x y) #:transparent)
;; Drop is (make-drop Integer Integer)
;; interp. A raindrop on the screen, with x and y coordinates.

(define D1 (make-drop 10 30))

#;
(define (fn-for-drop d)
  (... (drop-x d)
       (drop-y d)))

;; Template Rules used:
;; - compound: 2 fields


;; ListOfDrop is one of:
;;  - empty
;;  - (cons Drop ListOfDrop)
;; interp. a list of drops

(define LOD1 empty)
(define LOD2 (cons (make-drop 10 20) (cons (make-drop 3 6) empty)))

#;
(define (fn-for-lod lod)
  (cond [(empty? lod) (...)]
        [else
          (... (fn-for-drop (first lod))
               (fn-for-lod (rest lod)))]))

;; Template Rules used:
;; - one-of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Drop ListOfDrop)
;; - reference: (first lod) is Drop
;; - self reference: (rest lod) is ListOfDrop

;; =================
;; Functions:

;; ListOfDrop -> ListOfDrop
;; start rain program by evaluating (main empty)
(define (main lod)
  (big-bang lod
            (on-mouse handle-mouse)   ; ListOfDrop Integer Integer MouseEvent -> ListOfDrop
            (on-tick  next-drops)     ; ListOfDrop -> ListOfDrop
            (to-draw  render-drops))) ; ListOfDrop -> Image


;; ListOfDrop Integer Integer MouseEvent -> ListOfDrop
;; if mevt is "button-down" add a new drop at that position
(check-expect (handle-mouse empty 10 20 "button-down") (cons (make-drop 10 20) empty))
(check-expect (handle-mouse empty 10 20 "button-up") empty)
(check-expect (handle-mouse (cons (make-drop 20 10) empty) 10 20 "button-down")
              (cons (make-drop 10 20) (cons (make-drop 20 10) empty)))
;; (define (handle-mouse lod x y mevt) empty) ; stub

; template from MouseEvent

(define (handle-mouse lod x y mevt)
  (cond [(mouse=? mevt "button-down") (cons (make-drop x y) lod)]
        [else lod]))


;; ListOfDrop -> ListOfDrop
;; produce filtered and ticked list of drops

(check-expect (next-drops empty) empty)

(check-expect (next-drops (cons (make-drop 10 20) empty))
              (cons (make-drop 10 21) empty))

(check-expect (next-drops (cons (make-drop 10 HEIGHT) empty)) empty)


;; (define (next-drops lod) empty) ; stub

; template as function composition

(define (next-drops lod)
  (filter-drops (tick-drops lod)))

;; ListOfDrop -> ListOfDrop
;; increment drop-y value of every drop in list by SPEED

(check-expect (tick-drops empty) empty)
(check-expect (tick-drops (cons (make-drop 10 20) empty))
              (cons (make-drop 10 (+ 20 SPEED)) empty))
(check-expect (tick-drops (cons (make-drop 10 0) empty))
              (cons (make-drop 10 (+ 0 SPEED)) empty))

;; (define (tick-drops lod) lod) ;stub

;; took template from ListOfDrop

(define (tick-drops lod)
  (cond [(empty? lod) empty]
        [else
          (cons (tick-drop  (first lod))
                (tick-drops (rest lod)))]))

;; Drop -> Drop
;; increment drop-y by SPEED

(check-expect (tick-drop (make-drop 10 20))
              (make-drop 10 (+ 20 SPEED)))

;; (define (tick-drop d) d) ;stub

; took template from Drop

(define (tick-drop d)
  (make-drop (drop-x d)
             (+ (drop-y d) SPEED)))


;; ListOfDrop -> ListOfDrop
;; filter all drops in the list that has the drop-y value
;; mare than HEIGHT

(check-expect (filter-drops empty) empty)
(check-expect
  (filter-drops (cons (make-drop 10 20) empty))
  (cons (make-drop 10 20) empty))
(check-expect
  (filter-drops (cons (make-drop 10 (+ 1 HEIGHT)) empty)) empty)

;; (define (filter-drops lod) lod) ;stub

; took template from ListOfDrop

(define (filter-drops lod)
  (cond [(empty? lod) empty]
        [else
          (if (onscreen? (first lod))
            (cons (first lod) (filter-drops (rest lod)))
            (filter-drops (rest lod)))]))

;; Drop -> Drop
;; produce true if drop-y value is less than HEIGHT, else false

(check-expect (onscreen? (make-drop 10 (+ 1 HEIGHT)))   false)
(check-expect (onscreen? (make-drop 10            0))   true)

;; (define (fall? d) true) ;stub

(define (onscreen? d)
  (< (drop-y d) HEIGHT))

;; ListOfDrop -> Image
;; Render the drops onto MTS
(check-expect (render-drops empty) MTS)
(check-expect (render-drops (cons (make-drop 10 20) empty))
              (place-image DROP 10 20 MTS))
(check-expect (render-drops (cons (make-drop 10 20)
                                  (cons (make-drop 20 10) empty)))
              (place-image DROP 10 20
                           (place-image DROP 20 10 MTS)))

;; (define (render-drops lod) MTS) ; stub

; took template from ListOfDrop


(define (render-drops lod)
  (cond [(empty? lod) MTS]
        [else
          (render-drop (first lod)
                       (render-drops (rest lod)))]))

;; Drop -> Image
;; place DROP on bg (background) on appropiate x, y coordinates

(check-expect (render-drop (make-drop 10 20) MTS)
              (place-image DROP 10 20 MTS))

;; (define (render-drop d bg) bg) ;stub

(define (render-drop d bg)
  (place-image DROP (drop-x d) (drop-y d) bg))

(main empty)

(test)
