#lang racket

;; This is a conditional
(cond [(> 2 4) #t]
      [( > 2 4 ) #f]
      [else "I don't know"])
