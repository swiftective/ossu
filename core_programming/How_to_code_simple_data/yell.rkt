#lang racket
(require test-engine/racket-tests)

;; String -> String ;; Signature
;; Add ! to the end of string ;; Purpose

;; (define (yell str) "") ;; Snub

(check-expect (yell "Hello") "Hello!") ;; Tests
(check-expect (yell "Hi") "Hi!")

;; (define (yell str)
;;   (...str))

(define (yell str)
  (string-append str "!"))

(test)
