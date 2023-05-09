#lang racket
(require test-engine/racket-tests)

;; insert-starter.rkt

;
; PROBLEM:
;
; Design a function that consumes an Integer, String and BST, and adds a node
; that has the given key and value to the tree. The node should be inserted in
; the proper place in the tree. The function can assume there is not already
; an entry for that number in the tree. The function should produce the new BST.
;
; Do not worry about keeping the tree balanced. We will come back to this later.
;


;; Data definitions:

(define-struct node (key val l r) #:transparent)
;; A BST (Binary Search Tree) is one of:
;;  - false
;;  - (make-node Integer String BST BST)
;; interp. false means no BST, or empty BST
;;         key is the node key
;;         val is the node val
;;         l and r are left and right subtrees
;; INVARIANT: for a given node:
;;     key is > all keys in its l(eft)  child
;;     key is < all keys in its r(ight) child
;;     the same key never appears twice in the tree

(define BST0 false)
(define BST1 (make-node 1 "abc" false false))
(define BST4 (make-node 4 "dcj" false (make-node 7 "ruf" false false)))
(define BST3 (make-node 3 "ilk" BST1 BST4))
(define BST42
  (make-node 42 "ily"
             (make-node 27 "wit" (make-node 14 "olp" false false) false)
             false))
(define BST10 (make-node 10 "why" BST3 BST42))

#;
(define (fn-for-bst t)
  (cond [(false? t) (...)]
        [else
         (... (node-key t)    ;Integer
              (node-val t)    ;String
              (fn-for-bst (node-l t))
              (fn-for-bst (node-r t)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic-distinct: false
;;  - compound: (make-node Integer String BST BST)
;;  - self reference: (node-l t) has type BST
;;  - self reference: (node-r t) has type BST

;; Functions

;; BST BST -> BST
;; insert the consumed Integer, String in the proper place in BST

;; Integer String BST -> BST
;; insert the consumed Integer, String in the proper place in BST

(check-expect (insert-node 10 "abc" false) (make-node 10 "abc" false false)) ; Tests

(check-expect (insert-node 10 "cde" (make-node 10 "abc" false false))
              (make-node 10 "cde" false false))

(check-expect (insert-node 20 "bcd" BST1)
              (make-node (node-key BST1)
                         (node-val BST1)
                         false
                         (make-node 20 "bcd" false false)))

;; (define (insert-node k v t) t) ; Stub

;; took template from BST

(define (insert-node k v t)
  (cond [(false? t) (make-node k v false false)]
        [else
         (cond
           [(= k (node-key t)) (make-node k v (node-l t) (node-r t))]
           [(< k (node-key t)) (make-node (node-key t)
                                          (node-val t)
                                          (insert-node k v (node-l t))
                                          (node-r t))]
           [(> k (node-key t)) (make-node (node-key t)
                                          (node-val t)
                                          (node-l t)
                                          (insert-node k v (node-r t)))])]))


(test)
