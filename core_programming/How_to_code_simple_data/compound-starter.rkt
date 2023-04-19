#lang racket

;; compound-starter.rkt

;
; PROBLEM:
;
; Design a data definition to represent hockey players, including both
; their first and last names.

(define-struct player (fn ln))
;; Player is (make-player String String)
;; Interp. (make-player fn ln) is a hockey player with
;;          fn is the first name
;;          ln is the last name
;; (define P1 (make-player "Jackie" "Chan"))
;; (define P2 (make-player "James" "Bond"))

#;
(define (fn-for-player p)
  (... (player-fn p)        ; String
       (player-ln p)))      ; String

;; Template rules used
;; - Compound rule: 2 fields
