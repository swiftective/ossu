#lang racket

(define-struct pos (x y))

(define P1 (make-pos 3 6))
(define P2 (make-pos 2 8))

(pos-x P1) ; 3
(pos-y P2) ; 8

(pos? P1)        ; true
(pos? "hello")   ; false
