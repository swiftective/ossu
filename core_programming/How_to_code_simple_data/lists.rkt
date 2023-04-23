#lang racket
(require 2htdp/image)

(define L1 (cons "Flames" empty))
(define L2 (cons "Leafs" (cons "Flames" (cons "Hello" empty))))
(define L3 (cons (string-append "New" "Game") empty))
(define L4
 (cons (square 10 "solid" "blue")
       (cons (triangle 20 "solid" "green") empty)))

#| (first L1) |#
#| (first L2) |#
#| (first L3) |#
#| (first L4) |#


#| (rest L1) |#
#| (rest L2) |#
#| (rest L3) |#
#| (rest L4) |#

#| (first (rest L2))         ; second element of L1 |#
#| (first (rest (rest L2)))  ; third element of L1 |#

(empty? empty)
(empty? L1)
