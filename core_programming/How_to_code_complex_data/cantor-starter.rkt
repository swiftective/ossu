#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)

;; cantor-starter.rkt

; PROBLEM:
;
; A Cantor Set is another fractal with a nice simple geometry.
; The idea of a Cantor set is to have a bar (or rectangle) of
; a certain width w, then below that are two recursive calls each
; of 1/3 the width, separated by a whitespace of 1/3 the width.
;
; So this means that the
;   width of the whitespace   wc  is  (/ w 3)
;   width of recursive calls  wr  is  (/ (- w wc) 2)
;
; To make it look better a little extra whitespace is put between
; the bars.
;
;
; Here are a couple of examples (assuming a reasonable CUTOFF)
;
; (cantor CUTOFF) produces:
;
; (cantor (* CUTOFF 3)) produces:
;
; And that keeps building up to something like the following. So
; as it goes it gets wider and taller of course.
;
;
; PROBLEM A:
;
; Design a function that consumes a width and produces a cantor set of
; the given width.

; Number -> Image
; produce a cantor set of the given width w

(define CUTOFF 10)
(define HEIGHT 20)

(check-expect (cantor CUTOFF)
              (rectangle CUTOFF HEIGHT "solid" "blue"))

(check-expect (cantor (* 3 CUTOFF))
                      (above
                        (rectangle (* 3 CUTOFF) HEIGHT "solid" "blue")
                        (rectangle (* 3 CUTOFF) HEIGHT "solid" "black")
                        (local [(define sub (rectangle CUTOFF HEIGHT "solid" "blue"))]
                          (beside sub
                                  (rectangle CUTOFF 0 "solid" "black")
                                  sub))))

#;
(define (genrec-fn d) ; template
  (cond [(trivial? d) (trivial-answer d)]
        [else
         (... d
              (genrec-fn (next-problem d)))]))


; Three part termination argument.
;
; Base case: (<= w CUTOFF)
;
; Reduction step: (/ w 3)
;
; Argument that repeated application of reduction step will eventually
; reach the base case:
;
; As long as the CUTOFF is > 0 and s starts >=0 repeated division by 2
; will eventually be less than CUTOFF.

(define (cantor w)
  (cond
    [(<= w CUTOFF) (rectangle w HEIGHT "solid" "blue")]
    [else
      (above
        (rectangle w HEIGHT "solid" "blue")
        (rectangle w HEIGHT "solid" "black")
        (local [(define sub (cantor (/ w 3)))]
          (beside sub
                  (rectangle (/ w 3) 0 "solid" "black")
                  sub)))]))

; PROBLEM B:
;
; Add a second parameter to your function that controls the percentage
; of the recursive call that is white each time. Calling your new function
; with a second argument of 1/3 would produce the same images as the old
; function.

(define (cantor2 w p)
  (local [(define CUTOFF 10)
          (define HEIGHT 20)]
    (cond
      [(<= w CUTOFF) (rectangle w HEIGHT "solid" "blue")]
      [else
        (above
          (rectangle w HEIGHT "solid" "blue")
          (rectangle w HEIGHT "solid" "black")
          (local [(define sub (cantor2 (* w p) p))]
            (beside sub
                    (rectangle (* w p) 0 "solid" "black")
                    sub)))])))

;
; PROBLEM C:
;
; Now you can make a fun world program that works this way:
;   The world state should simply be the most recent x coordinate of the mouse.
;
;   The to-draw handler should just call your new cantor function with the
;   width of your MTS as its first argument and the last x coordinate of
;   the mouse divided by that width as its second argument.
;

(define WIDTHMTS 1000)
(define HEIGHTMTS 500)
(define MTS (empty-scene WIDTHMTS HEIGHTMTS "black"))


;; Can is one of:
;; - false
;; - Natural
;; Interp. most recent x coordinate or not started


;; Can -> Can
;; start the world with (main false)

(define (main c)
  (big-bang c                               ; Can
            (to-draw   render)              ; Can -> Image
            (on-mouse  handle-mouse)))      ; Can Integer Integer MouseEvent -> Can


;; Can -> Image
;; produce image is Can is not false

;; (define (render c) MTS) ;stub

(define (render c)
  (cond
    [(false? c) MTS]
    [else
      (place-image
        (cantor2 WIDTHMTS (/ (modulo c (/ WIDTHMTS 2)) WIDTHMTS))
        (/ WIDTHMTS 2)
        (/ HEIGHTMTS 2) MTS)]))

;; Can Integer Integer MouseEvent -> Can
;; produce x coordinate when mouse clicked

;; (define (handle-mouse c x y me) x) ;stub

  (define (handle-mouse c x _ me)
    (if (mouse=? me "button-down") x c))


(main false)

(test)
