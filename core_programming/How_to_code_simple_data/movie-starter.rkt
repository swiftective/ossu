#lang racket
(require test-engine/racket-tests)

;; movie-starter.rkt

;; =================
;; Data definitions:

; PROBLEM A:
;
; Design a data definition to represent a movie, including
; title, budget, and year released.
;
; To help you to create some examples, find some interesting movie facts below:
; "Titanic" - budget: 200000000 released: 1997
; "Avatar" - budget: 237000000 released: 2009
; "The Avengers" - budget: 220000000 released: 2012
;
; However, feel free to resarch more on your own!

(define-struct movie (t b yr))
;; Movie is (make-movie String Natural Natural)
;; Interp. (make-movie t b yr) is a movie with
;;          t is the title
;;          b is the budget
;;          yr is the year-released
(define M1 (make-movie "Avatar" 237000000 2009))
(define M2 (make-movie  "The Avengers" 220000000 2012))
(define M3 (make-movie  "Titanic" 200000000 1997))

#;
(define (fn-for-movie m)
  (...
    (movie-t m)         ; String
    (movie-b m)         ; Natural
    (movie-yr m)))      ; Natural

;; Template rules used
;; - Compound: 2 fields

;; =================
;; Functions:

; PROBLEM B:
;
; You have a list of movies you want to watch, but you like to watch your
; rentals in chronological order. Design a function that consumes two movies
; and produces the title of the most recently released movie.
;
; Note that the rule for templating a function that consumes two compound data
; parameters is for the template to include all the selectors for both
; parameters.

;; Movie Movie -> String
;; Given two movies return the recently released movie

(check-expect (recent-movie M1 M2) "The Avengers") ; Tests
(check-expect (recent-movie M1 M3) "Avatar")

;; (define (recent-movie m1 m2) "") ; Stub

;; (define (recent-movie m1 m2)  ; Template
;;    (...  (movie-t m1)         ; String
;;          (movie-b m1)         ; Natural
;;          (movie-yr m1)        ; Natural
;;          (movie-t m2)         ; String
;;          (movie-b m2)         ; Natural
;;          (movie-yr m2)))      ; Natural

(define (recent-movie m1 m2)
  (if (>= (movie-yr m1) (movie-yr m2))
    (movie-t m1)
    (movie-t m2)))

(test)
