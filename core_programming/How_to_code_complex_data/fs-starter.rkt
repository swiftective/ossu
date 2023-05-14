#lang racket
(require 2htdp/image)
(require test-engine/racket-tests)

;; fs-starter.rkt (type comments and examples)

;; Constants

(define TEXT-SIZE 14)
(define TEXT-COLOR "black")

(define KEY-VAL-SEPERATION ":")

(define VSPACE (rectangle 0 10 "solid" "white"))
(define HSPACE (rectangle 10 0 "solid" "white"))
(define MTTREE (square 0 "solid" "white"))

;; Data definitions:

(define-struct elt (name data subs))
;; Element is (make-elt String Integer ListOfElement)
;; interp. An element in the file system, with name, and EITHER data or subs.
;;         If data is 0, then subs is considered to be list of sub elements.
;;         If data is not 0, then subs is ignored.

;; ListOfElement is one of:
;;  - empty
;;  - (cons Element ListOfElement)
;; interp. A list of file system Elements

(define F1 (make-elt "F1" 1 empty))
(define F2 (make-elt "F2" 2 empty))
(define F3 (make-elt "F3" 3 empty))
(define D4 (make-elt "D4" 0 (list F1 F2)))
(define D5 (make-elt "D5" 0 (list F3)))
(define D6 (make-elt "D6" 0 (list D4 D5)))

#;
(define (fn-for-element e)
  (... (elt-name e)
       (elt-data e)
       (fn-for-loe (elt-subs e))))

#;
(define (fn-for-loe loe)
  (cond
    [(empty? loe) (...)]
    [else
      (...
        (fn-for-element (first loe))
        (fn-for-loe (rest loe)))]))

;; Functions:

; Design a function that consumes Element and produces the sum of all the file data in the tree


;; Element -> Integer
;; ListOfElement -> Integers
;; produce the sum of all the file data in the tree

(check-expect (sum-data--element F1) 1) ; Tests
(check-expect (sum-data--element F2) 2)
(check-expect (sum-data--loe empty) 0)
(check-expect (sum-data--loe (elt-subs D4)) 3)
(check-expect (sum-data--loe (elt-subs D6)) 6)
(check-expect (sum-data--element D6) 6)


;; (define (sum-data--element e) 0) ; Stub
;; (define (sum-data--loe e) 0) ; Stub


(define (sum-data--element e)
  (if (zero? (elt-data e))
    (sum-data--loe (elt-subs e))
    (elt-data e)))

#| (define (sum-data--element e) |#
#|   (+ (elt-data e) |#
#|      (sum-data--loe (elt-subs e)))) |#

(define (sum-data--loe loe)
  (cond
    [(empty? loe) 0]
    [else
       (+
         (sum-data--element (first loe))
         (sum-data--loe (rest loe)))]))

; Design a function that consumes Element and produces a list of the names of all the elements in the tree

;; Element -> ListOfString
;; ListOfElement -> ListOfString
;; produce a list of the names of all the elements in the tree
;; produce a list of names of all the elements in the consumed list

(check-expect (list-names--element F1) (list "F1")) ; Tests
(check-expect (list-names--element F2) (list "F2"))
(check-expect (list-names--loe (list F2)) (list "F2"))
(check-expect (list-names--loe (list F2)) (list "F2"))
(check-expect (list-names--loe (elt-subs D6))
              (append (list "D4" "F1" "F2") (list "D5" "F3")))
(check-expect (list-names--element D6)
              (append (list "D6" "D4" "F1" "F2") (list "D5" "F3")))

;; (define (list-names--element e) empty) ; Stub
;; (define (list-names--loe loe) empty) ; Stub

(define (list-names--element e)
  (cons (elt-name e)
        (list-names--loe (elt-subs e))))

(define (list-names--loe loe)
  (cond
    [(empty? loe) empty]
    [else
      (append
        (list-names--element (first loe))
        (list-names--loe (rest loe)))]))


;; Problem 2
; Design a function that consumes String and Element and looks for a data element with the given
; name. If it finds that element it produces the data, otherwise it produces false.

;; String Element -> Integer or false
;; String ListOfElement -> Integer or false
;; produce the (elt data) of Element if (elt name) == String

(check-expect (find--element "F1" F1) 1) ; Tests
(check-expect (find--element "F2" D4) 2) ; Tests
(check-expect (find--element "F3" D4) false) ; Tests
(check-expect (find--element "G2" F1) false)
(check-expect (find--loe "G2" empty) false)
(check-expect (find--loe "G2" (list F1 F2)) false)
(check-expect (find--loe "F2" (list F1 F2)) 2)
(check-expect (find--loe "F1" (list F1 F2)) 1)
(check-expect (find--element "G2" D6) false)
(check-expect (find--element "F1" D6) 1)
(check-expect (find--element "F3" D6) 3)
(check-expect (find--element "D4" D6) 0)

;; (define (find-name--element str e) false) ; Stub
;; (define (find-name--loe str loe) false) ; Stub

(define (find--element str e)
  (if (string=? (elt-name e) str)
       (elt-data e)
       (find--loe str (elt-subs e))))

(define (find--loe str loe)
  (cond
    [(empty? loe) false]
    [else
      (or (find--element str (first loe))
          (find--loe str (rest loe)))]))


;; Problem 3
;; Design a function that consumes Element and produces a rendering of the tree. For example:
;;
;; (render-tree D6) should produce something like the following.
;;
;; HINTS:
;;   - This function is not very different than the first two functions above.
;;   - Keep it simple! Start with a not very fancy rendering like the one above.
;;     Once that works you can make it more elaborate if you want to.
;;   - And... be sure to USE the recipe. Not just follow it, but let it help you.
;;     For example, work out a number of examples BEFORE you try to code the function.

;; Element -> Image
;; ListOfElement -> Image
;; produce rendering of Element as tree

(check-expect (render-element--element F1)
              (above (render-text (elt-name F1) (elt-data F1))
                     VSPACE)) ; Tests
(check-expect (render-element--loe empty) MTTREE)


(check-expect (render-element--loe (list F1 F2))
              (beside
                (above (render-text (elt-name F1) (elt-data F1)) VSPACE)
                HSPACE
                (beside
                  (above (render-text (elt-name F2) (elt-data F2)) VSPACE) HSPACE)))

;; (define (render-element--element e) MTTREE) ; Stub
;; (define (render-element--loe loe) MTTREE) ; Stub

(define (render-element--element e)
  (above (render-text (elt-name e) (elt-data e))
         VSPACE
         (render-element--loe (elt-subs e))))

(define (render-element--loe loe)
  (cond
    [(empty? loe) MTTREE]
    [else
      (beside
        (render-element--element (first loe))
        HSPACE
        (render-element--loe (rest loe)))]))

;; String, Number -> Image
;; produce image with String and Number seperated by KEY-VAL-SEPERATION

(check-expect (render-text "hi" 10)
              (text (string-append "hi" KEY-VAL-SEPERATION "10")
                    TEXT-SIZE
                    TEXT-COLOR)) ; Tests

;; (define (render-text str n) (text "hi:10" TEXT-SIZE TEXT-COLOR)) ; Stub

(define (render-text str n)
  (text
    (string-append str KEY-VAL-SEPERATION (number->string n))
    TEXT-SIZE
    TEXT-COLOR))


(test)
