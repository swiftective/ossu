#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)

;; fractals-starter.rkt

;
; PROBLEM:
;
; Design a function that consumes a number and produces a Sierpinski
; triangle of that size. Your function should use generative recursion.
;
; One way to draw a Sierpinski triangle is to:
;
;  - start with an equilateral triangle with side length s
;
;  - inside that triangle are three more Sierpinski triangles
;
;  - and inside each of those... and so on
;

;
; Note that in the 2nd picture above the inner triangles are drawn in
; black and slightly smaller just to make them clear. In the real
; Sierpinski triangle they should be in the same color and of side
; length s/2. Also note that the center upside down triangle is not
; an explicit triangle, it is simply formed from the other triangles.

(define CUTOFF 2)

; Number -> Image
; produce a Sierpinski triangle of the given size

(check-expect (stri CUTOFF) (triangle CUTOFF "outline" "red") )
(check-expect (stri (* CUTOFF 2))
              (overlay
                (triangle (* CUTOFF 2) "outline" "red")
                (local [(define sub (triangle CUTOFF "outline" "red"))]
                  (above sub
                         (beside sub sub)))))

;; (define (stri s) (square 0 "solid" "white")) ;stub

#;
(define (genrec-fn d) ; template
  (cond [(trivial? d) (trivial-answer d)]
        [else
         (... d
              (genrec-fn (next-problem d)))]))


; Three part termination argument.
;
; Base case: (<- s CUTOFF)
;
; Reduction step: (/ s 2)
;
; Argument that repeated application of reduction step will eventually
; reach the base case:
;
; As long as the CUTOFF is > 0 and s starts >=0 repeated division by 2
; will eventually be less than CUTOFF.

(define (stri s)
  (cond [(<= s CUTOFF) (triangle s "outline" "red")]
        [else
          (overlay
            (triangle s "outline" "red")
            (local [(define sub (stri (/ s 2)))]
              (above sub
                     (beside sub sub))))]))


; PROBLEM:
;
; Design a function to produce a Sierpinski carpet of size s.


; Number -> Image
; produce a Sierpinski carpet of size s

; (define (scar s) (square 0 "outline" "white")) ;stub

(check-expect (scar CUTOFF)
              (square CUTOFF "outline" "red"))

(check-expect (scar (* 3 CUTOFF))
              (overlay
                (square (* 3 CUTOFF) "outline" "red")
                (local
                  [(define sub (square CUTOFF "outline" "red"))]
                  (above
                    (beside sub sub sub)
                    (beside sub (square CUTOFF "solid" "white") sub)
                    (beside sub sub sub)))))


#;
(define (genrec-fn d) ; template
  (cond [(trivial? d) (trivial-answer d)]
        [else
         (... d
              (genrec-fn (next-problem d)))]))

; Three part termination argument.
;
; Base case: (<- s CUTOFF)
;
; Reduction step: (/ s 3)
;
; Argument that repeated application of reduction step will eventually
; reach the base case:
;
; As long as the CUTOFF is > 0 and s starts >=0 repeated division by 3
; will eventually be less than CUTOFF.

(define (scar s)
  (cond [(<= s CUTOFF) (square s "outline" "red")]
        [else
         (local
           [(define sub (scar (/ s 3)))]
           (above
             (beside sub sub sub)
             (beside sub (square (/ s 3) "solid" "white") sub)
             (beside sub sub sub)))]))


(test)
