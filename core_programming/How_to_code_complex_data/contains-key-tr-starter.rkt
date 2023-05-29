#lang racket
(require test-engine/racket-tests)

;; bt-contains-tr-starter.rkt

; Problem:
;
; Starting with the following data definition for a binary tree (not a binary search tree)
; design a tail-recursive function called contains? that consumes a key and a binary tree
; and produces true if the tree contains the key.
;


(define-struct node (k v l r))
;; BT is one of:
;;  - false
;;  - (make-node Integer String BT BT)
;; Interp. A binary tree, each node has a key, value and 2 children
(define BT1 false)
(define BT2 (make-node 1 "a"
                       (make-node 6 "f"
                                  (make-node 4 "d" false false)
                                  false)
                       (make-node 7 "g" false false)))

;; BT String -> Boolean
;; produce true if Bt contains key

(check-expect (contains? BT1 1) false) ; Tests
(check-expect (contains? BT2 1) true)
(check-expect (contains? BT2 7) true)
(check-expect (contains? BT2 5) false)
(check-expect (contains? BT2 6) true)
(check-expect (contains? BT2 4) true)

;; (define (contains? node key) true) ; Stub

(define (contains? n k)
  (local
    [(define (contains? n todo)
       (cond
         [(false? n) (wl todo)]
         [else
          (or
           (= (node-k n) k)
           (wl
            (append
             (list (node-l n) (node-r n))
             todo)))]))

     (define (wl todo)
       (cond
         [(empty? todo) false]
         [else
          (contains?
           (first todo) (rest todo))]))]
    (contains? n empty)))


(test)
