#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)

;; tuition-graph-starter.rkt  (just the problem statements)

; PROBLEM:
;
; Eva is trying to decide where to go to university. One important factor for her is
; tuition costs. Eva is a visual thinker, and has taken Systematic Program Design,
; so she decides to design a program that will help her visualize the costs at
; different schools. She decides to start simply, knowing she can revise her design
; later.
;
; The information she has so far is the names of some schools as well as their
; international student tuition costs. She would like to be able to represent that
; information in bar charts like this one:
;
; (A) Design data definitions to represent the information Eva has.
; (B) Design a function that consumes information about schools and their
;     tuition and produces a bar chart.
; (C) Design a function that consumes information about schools and produces
;     the school with the lowest international student tuition.

;; Constants:

(define FONT-SIZE 24)
(define FONT-COLOR "black")

(define Y-SCALE 1/200)
(define BAR-WIDTH 30)
(define BAR-COLOR "blue")

;; Data definitions:

(define-struct school (name tuition) #:transparent)
;; School is (make-school String Natural)
;; Interp. (make-school name tuition) is a school with:
;;          - name as the name of a school
;;          - tuition as the tuition cost of the school in USD
;; (define S1 (make-school "Oxford" 2000))
;; (define S2 (make-school "Harvard" 4000))

#;
(define (fn-for-school s)
  (... (school-name s)        ; String
       (school-tuition s)))   ; Number

;; Template rules used
;; - Compound: (make-school String Natural)


;; ListOfSchool is one of:
;; - empty
;; - (cons School ListOfSchool)
;; Interp. a list of schools
;; (define LOS1 empty)
;; (define LOS2 (cons (make-school "Oxford" 2000) empty))
;; (define LOS3
;; (cons (make-school "Oxford" 2000)
;;   (cons (make-school "Harvard" 8000)
;;     (cons (make-school "Standford" 12000) empty))))

#;
(define (fn-for-los los)
  (cond
    [(empty? los) (...)]
    [else
      (... (fn-for-school (first los))
           (fn-for-los (rest los)))]
  )
)

;; Template rules used
;; - Atomic distinct: empty
;; - Compound: (cons School ListOfSchool)
;; - reference: (first los)
;; - self reference: (rest los) is ListOfSchool

;; Functions:

;; ListOfSchool -> Image
;; produce bar chart based on the tuition of the schools in the list consumed

(check-expect (chart empty) (square 0 "solid" "white")) ; Tests
(check-expect (chart (cons (make-school "S1" 8000) empty))
              (beside/align "bottom"
                            (overlay/align "center" "bottom"
                                           (rotate 90 (text "S1" FONT-SIZE FONT-COLOR))
                                           (rectangle BAR-WIDTH (* 8000 Y-SCALE) "outline" "black")
                                           (rectangle BAR-WIDTH (* 8000 Y-SCALE) "solid" BAR-COLOR))
                            (square 0 "solid" "white")))
(check-expect (chart (cons (make-school "S2" 12000) (cons (make-school "S1" 8000) empty)))
              (beside/align "bottom"
                            (overlay/align "center" "bottom"
                                           (rotate 90 (text "S2" FONT-SIZE FONT-COLOR))
                                           (rectangle BAR-WIDTH (* 12000 Y-SCALE) "outline" "black")
                                           (rectangle BAR-WIDTH (* 12000 Y-SCALE) "solid" BAR-COLOR))
                            (overlay/align "center" "bottom"
                                           (rotate 90 (text "S1" FONT-SIZE FONT-COLOR))
                                           (rectangle BAR-WIDTH (* 8000 Y-SCALE) "outline" "black")
                                           (rectangle BAR-WIDTH (* 8000 Y-SCALE) "solid" BAR-COLOR))
                            (square 0 "solid" "white")))

;; (define (chart los) (square 0 "solid" "white")) ; Stub

;; took template from ListOfSchool

(define (chart los)
  (cond
    [(empty? los) (square 0 "solid" "white")]
    [else
      (beside/align "bottom" (make-bar (first los))
                             (chart (rest los)))]
  )
)

;; School -> Image
;; produce the bar for a single school in the bar chart

(check-expect (make-bar (make-school "Oxford" 8000))
              (overlay/align "center" "bottom"
                                   (rotate 90 (text "Oxford" FONT-SIZE FONT-COLOR))
                                   (rectangle BAR-WIDTH (* 8000 Y-SCALE) "outline" "black")
                                   (rectangle BAR-WIDTH (* 8000 Y-SCALE) "solid" BAR-COLOR)))

;; (define (make-bar s) (square 10 "solid" "white")) ;stub

;; took template from School

(define (make-bar s)
    (overlay/align "center" "bottom"
                         (rotate 90 (text (school-name s) FONT-SIZE FONT-COLOR))
                         (rectangle BAR-WIDTH (* (school-tuition s) Y-SCALE) "outline" "black")
                         (rectangle BAR-WIDTH (* (school-tuition s) Y-SCALE) "solid" BAR-COLOR)))


(test)
