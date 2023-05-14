#lang racket
(require test-engine/racket-tests)


;; hp-family-tree-starter.rkt

; In this problem set you will represent information about descendant family
; trees from Harry Potter and design functions that operate on those trees.
;
; To make your task much easier we suggest two things:
;   - you only need a DESCENDANT family tree
;   - read through this entire problem set carefully to see what information
;     the functions below are going to need. Design your data definitions to
;     only represent that information.
;   - you can find all the information you need by looking at the individual
;     character pages like the one we point you to for Arthur Weasley.


; Design a data definition that represents a family tree from the Harry Potter
; wiki, which contains all necessary information for the other problems.  You
; will use this data definition throughout the rest of the homework.

(define-struct wizard (name wand children))
;; Wizard is (make-wizard String String ListOfWizard)
;; Interp. (make-wizard name wand children) is a wizard with
;; - name as the name of the wizard
;; - wand as the material their wand is made
;; - chidren as their children

;; ListOfWizard is one of:
;; - empty
;; - (cons Wizard ListOfWizard)
;; Interp. list of Wizard

#;
(define (fn-for-wizard w)
  (...
   (wizard-name w)
   (wizard-wand w)
   (fn-for-low (wizard-children w))))

#;
(define (fn-for-low low)
  (cond
    [(empty? low) (...)]
    [else
     (...
      (fn-for-wizard (first low))
      (fn-for-low (rest low)))]))

; Define a constant named ARTHUR that represents the descendant family tree for
; Arthur Weasley. You can find all the infomation you need by starting
; at: http://harrypotter.wikia.com/wiki/Arthur_Weasley.
;
; You must include all of Arthur's children and these grandchildren: Lily,
; Victoire, Albus, James.
;
;
; Note that on the Potter wiki you will find a lot of information. But for some
; people some of the information may be missing. Enter that information with a
; special value of "" (the empty string) meaning it is not present. Don't forget
; this special value when writing your interp.


;; FIY I didnot write this by hand but used a js to get this output from a js object generated
;; by GOOGLE BARD using the same mutual reference technique
(define ARTHUR (make-wizard "Arthur Weasley" "Cherry"
                            (cons (make-wizard "Bill Weasley" "Holly"
                                               (cons (make-wizard "Victoire Weasley" "Vine" empty)
                                                     (cons (make-wizard "Dominique Weasley" "Willow" empty)
                                                           (cons (make-wizard "Louis Weasley" "Ash" empty) empty))))
                                  (cons (make-wizard "Charlie Weasley" "Ash" empty)
                                        (cons (make-wizard "Percy Weasley" "Walnut"
                                                           (cons (make-wizard "Molly Weasley II" "Vine" empty)
                                                                 (cons (make-wizard "Lucy Weasley" "Willow" empty) empty)))
                                              (cons (make-wizard "Fred Weasley" "Vine"
                                                                 (cons (make-wizard "Fred Weasley II" "Mahogany" empty)
                                                                       (cons (make-wizard "Roxanne Weasley" "Walnut" empty) empty)))
                                                    (cons (make-wizard "George Weasley" "Cherry"
                                                                       (cons (make-wizard "George Weasley II" "Mahogany" empty)
                                                                             (cons (make-wizard "Roxanne Weasley" "Walnut" empty) empty)))
                                                          (cons (make-wizard "Ron Weasley" "Ash"
                                                                             (cons (make-wizard "Hugo Weasley" "Cherry" empty)
                                                                                   (cons (make-wizard "Rose Weasley" "Vine" empty) empty)))
                                                                (cons (make-wizard "Ginny Weasley" "Hazel"
                                                                                   (cons (make-wizard "James Sirius Potter" "Mahogany" empty)
                                                                                         (cons (make-wizard "Albus Severus Potter" "Walnut" empty)
                                                                                               (cons (make-wizard "Lily Luna Potter" "Cherry" empty) empty)))) empty)))))))))


; Design a function that produces the names of every wizard in a given tree
; whose wands are made of a given material.
;
; You must use ARTHUR as one of your examples.

;; String Wizard -> ListOfString
;; String ListOfWizard -> ListOfString
;; produce a list of names of every wizards owned a wand of given wood material

(check-expect (wand-owners--wizard "NotAWant" ARTHUR) empty)
(check-expect (wand-owners--wizard "Vine" ARTHUR)
              (list "Victoire Weasley" "Molly Weasley II" "Fred Weasley" "Rose Weasley")) ; Tests

;; (define (wand-owners--wizard str w) empty) ; Stub
;; (define (wand-owners--low str low) empty) ; Stub

(define (wand-owners--wizard str w)
  (if (string=? (wizard-wand w) str)
   (list (wizard-name w))
   (wand-owners--low str (wizard-children w))))

(define (wand-owners--low str low)
  (cond
    [(empty? low) empty]
    [else
     (append
      (wand-owners--wizard str (first low))
      (wand-owners--low str (rest low)))]))

(test)
