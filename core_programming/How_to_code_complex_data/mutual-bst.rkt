#lang racket
(require test-engine/racket-tests)

(define-struct junction (left straight right))

;; Maze is one of:
;; - false
;; - "finish"
;; - (make-junction Maze Maze Maze)
;; a maze where at each junction you can either go straight, left or right.
;; false means dead end, "finish" means you've reached the end of the maze
(define M0 false)      ; a maze that is a dead end
(define M1 "finish")   ; a maze where you are already at the finish
(define M2 (make-junction
            (make-junction false
                           (make-junction false
                                          false
                                          (make-junction false
                                                         false
                                                         false))
                           (make-junction false
                                          (make-junction
                                           (make-junction false
                                                          false
                                                          false)
                                           (make-junction false
                                                          "finish"
                                                          false)
                                           false)
                                          false))
            (make-junction false
                           false
                           (make-junction false
                                          false
                                          false))
            false))    ; a maze


#;
(define (fn-for-junction j)
  (...
   (fn-for-maze (junction-left j))
   (fn-for-maze (junction-straight j))
   (fn-for-maze (junction-right j))))

#;
(define (fn-for-maze m)
  (cond
    [(false? m) (...)]
    [(string=? "finish" m) (...)]
    [else
     (...
      (fn-for-junction m))]))


(define (find-finish--junction j)
  (or
   (find-finish--maze (junction-left j))
   (find-finish--maze (junction-straight j))
   (find-finish--maze (junction-right j))))

(define (find-finish--maze m)
  (cond
    [(false? m) false]
    [(and (string? m) (string=? "finish" m)) "Found Finish"]
    [else
     (find-finish--junction m)]))

(find-finish--maze M0)
(find-finish--maze M1)
(find-finish--maze M2)

(test)
