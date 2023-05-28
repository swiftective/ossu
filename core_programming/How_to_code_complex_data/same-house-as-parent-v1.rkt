#lang racket
(require test-engine/racket-tests)


;; same-house-as-parent-v1.rkt

;
; PROBLEM:
;
; In the Harry Potter movies, it is very important which of the four houses a
; wizard is placed in when they are at Hogwarts. This is so important that in
; most families multiple generations of wizards are all placed in the same family.
;
; Design a representation of wizard family trees that includes, for each wizard,
; their name, the house they were placed in at Hogwarts and their children. We
; encourage you to get real information for wizard families from:
;    http://harrypotter.wikia.com/wiki/Main_Page
;
; The reason we do this is that designing programs often involves collection
; domain information from a variety of sources and representing it in the program
; as constants of some form. So this problem illustrates a fairly common scenario.
;
; That said, for reasons having to do entirely with making things fit on the
; screen in later videos, we are going to use the following wizard family tree,
; in which wizards and houses both have 1 letter names. (Sigh)
;

(define-struct wiz (name house children))
;; Wizard is (make-wiz String String (listof Wizard))
;; Interp. a wizard with name, house as in the Harry Potter Series, and their children

(define Wa (make-wiz "A" "S" empty))
(define Wb (make-wiz "B" "G" empty))
(define Wc (make-wiz "C" "R" empty))
(define Wd (make-wiz "D" "H" empty))
(define We (make-wiz "E" "R" empty))
(define Wf (make-wiz "F" "R" (list Wb)))
(define Wg (make-wiz "G" "S" (list Wa)))
(define Wh (make-wiz "H" "S" (list Wc Wd)))
(define Wi (make-wiz "I" "H" empty))
(define Wj (make-wiz "J" "R" (list We Wf Wg)))
(define Wk (make-wiz "K" "G" (list Wh Wi Wj)))

#;
(define (fn-for-wiz w)
  (local
    [(define (fn-for-wiz w)
       (...
        (wiz-name w)
        (wiz-house w)
        (fn-for-low (wiz-children w))))


     (define (fn-for-low low)
       (cond
         [(empty? low) (...)]
         [else
          (... (fn-for-wiz (first low))
               (fn-for-low (rest low)))]))]
    (fn-for-wiz w)))

;
; PROBLEM:
;
; Design a function that consumes a wizard and produces the names of every
; wizard in the tree that was placed in the same house as their immediate
; parent.

;; Wizard -> (listof String)
;; produce a list of Wizard names who were placed in the same house as their immediate parent

(check-expect (same-house Wa) empty)
(check-expect (same-house Wh) empty)
(check-expect (same-house Wg) (list "A")) ; Tests
(check-expect (same-house Wk) (list "E" "F" "A"))
(check-expect (same-house Wj) (list "E" "F" "A"))

;; (define (same-house w) empty) ; Stub

;; took template from Wizard

(define (same-house w)
  (local
    [(define (fn-for-wiz w ph)
       (if (string=? ph (wiz-house w))
           (cons (wiz-name w)
                 (fn-for-low (wiz-children w) (wiz-house w)))
           (fn-for-low (wiz-children w) (wiz-house w))))


     (define (fn-for-low low ph)
       (cond
         [(empty? low) empty]
         [else
          (append (fn-for-wiz (first low) ph)
                  (fn-for-low (rest low) ph))]))]
    (fn-for-wiz w "")))


;
; PROBLEM:
;
; Design a new function definition for same-house-as-parent that is tail
; recursive. You will need a worklist accumulator.
;

;; Wizard -> (listof String)
;; produce a list of Wizard names who were placed in the same house as their immediate parent

(check-expect (same-house-tail-recursive Wa) empty)
(check-expect (same-house-tail-recursive Wh) empty)
(check-expect (same-house-tail-recursive Wg) (list "A")) ; Tests
(check-expect (same-house-tail-recursive Wk) (list "A" "F" "E"))
(check-expect (same-house-tail-recursive Wj) (list "A" "F" "E"))

;; (define (same-house w) empty) ; Stub

;; took template from Wizard

; template from Wizard (arb-arity tree, wrapped in local)
; added worklist accumulator for tail recursion


(define (same-house-tail-recursive w)
  (local
    [(define-struct wle (wiz parent-house))     ; --> Worklist (make-wle Wizard String)
     (define (fn-for-wiz w ph todo acc)         ; --> Wizard String (listof Worklist) (listof String)
       (local
         [(define lowle
            (map
             (lambda (e) (make-wle e (wiz-house w)))
             (wiz-children w)))]
         (fn-for-low
          (append lowle todo)
          (if
           (string=? ph (wiz-house w))
           (cons (wiz-name w) acc) acc))))


     (define (fn-for-low todo acc)
       (cond
         [(empty? todo) acc]
         [else
          (fn-for-wiz (wle-wiz (first todo))
                      (wle-parent-house (first todo))
                      (rest todo)
                      acc)]))]
    (fn-for-wiz w "" empty empty)))


;
; PROBLEM:
;
; Design a function that consumes a wizard and produces the number of wizards
; in that tree (including the root). Your function should be tail recursive.
;

;; Wizard -> Natural
;; produce the number wizards in the tree (tail recursive)

(check-expect (sum-wiz Wg) 2) ; Tests
(check-expect (sum-wiz Wa) 1)
(check-expect (sum-wiz Wk) 11)

;; (define (sum-wiz w) 0) ; Stub

;; took template from Wizard

(define (sum-wiz w)
  (local
    [(define (fn-for-wiz w todo sum)
       (fn-for-low
        ;; (append todo (wiz-children w)) --> breadth-first search
        (append (wiz-children w) todo) ; ---> depth-first search
        (add1 sum)))


     (define (fn-for-low todo sum)
       (cond
         [(empty? todo) sum]
         [else
          (fn-for-wiz (first todo) (rest todo) sum)]))]
    (fn-for-wiz w empty 0)))


(test)
