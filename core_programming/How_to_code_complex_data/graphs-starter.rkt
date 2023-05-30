#lang htdp/asl
(require test-engine/racket-tests)

; PROBLEM:
;
; Imagine you are suddenly transported into a mysterious house, in which all
; you can see is the name of the room you are in, and any doors that lead OUT
; of the room.  One of the things that makes the house so mysterious is that
; the doors only go in one direction. You can't see the doors that lead into
; the room.
;
;
; In computer science, we refer to such an information structure as a directed
; graph. Like trees, in directed graphs the arrows have direction. But in a
; graph it is  possible to go in circles, as in the second example above. It
; is also possible for two arrows to lead into a single node, as in the fourth
; example.
;
;
; Design a data definition to represent such houses. Also provide example data
; for the four houses above.


(define-struct room (name exits))
;; Room is (make-room String (listof Room))
;; interp. the room's name, and list of rooms that the exits lead to

(define H1 (make-room "A" (list (make-room "B" empty))))

#;
(define H2
  (shared
      ((-0-
        (make-room "A"
                   (list
                    (make-room "B" (list -0-)))))) -0-))

(define H3
  (shared
      ((-A- (make-room "A" (list -B-)))
       (-B- (make-room "B" (list -C-)))
       (-C- (make-room "C" (list -A-))))
    -A-))


(define H4
  (shared
      ((-A- (make-room "A" (list -B- -D-)))
       (-B- (make-room "B" (list -C- -E-)))
       (-C- (make-room "C" (list -B-)))
       (-D- (make-room "D" (list -E-)))
       (-E- (make-room "E" (list -F- -A-)))
       (-F- (make-room "F" empty)))
    -A-))


;; template structural recursion,
;; encapsulate w/ local,
;; tail-recursive w/ worklist,
;; context-preserving accumulator what rooms have we already visited

#;
(define (fn-for-house r)
  ;; todo is (listof Room); a worklist accumulator
  ;; visited is (listof String); context-preserving accumulator, names of room already visited
  (local
    [(define (fn-for-room r todo visited)  ; (... (room-name r))
       (if (member (room-name r) visited)
           (fn-for-lor todo visited)
           (fn-for-lor
            (append (room-exits r) todo)
            (cons (room-name r) visited))))

     (define (fn-for-lor todo visited)
       (cond
         [(empty? todo) (... visited)]
         [else
          (fn-for-room (first todo) (rest todo) visited)]))]
    (fn-for-room r empty empty)))


;; PROBLEM:
;; Design a function that consumes a Room
;; and a room name, and produce true
;; if it is possible to reach a room with the given
;; name starting at the given room.

;; NOTE: note that if you defined H4F to be the room
;; name F in the H4 house then (reachable? H4F "A")
;; would produce false because it is not possible
;; to get to A from F in that house


;; Room String -> Boolean
;; produce true if room is reachable

(check-expect (reachable? H1 "A") true)
(check-expect (reachable? H1 "B") true)
(check-expect (reachable? H1 "C") false)
(check-expect (reachable? (first (room-exits H1)) "A") false)
(check-expect (reachable? H4 "F") true)

;; (define (reachable? r rn) true)

;<template from Room>

(define (reachable? r rn)
  ;; todo is (listof Room); a worklist accumulator
  ;; visited is (listof String); context-preserving accumulator, names of room already visited
  (local
    [(define (fn-for-room r todo visited)  ; (... (room-name r))
       (cond
         [(member (room-name r) visited)
          (fn-for-lor todo visited)]
         [(string=? (room-name r) rn) true]
         [else (fn-for-lor
                (append (room-exits r) todo)
                (cons (room-name r) visited)) ]))

     (define (fn-for-lor todo visited)
       (cond
         [(empty? todo) false]
         [else
          (fn-for-room (first todo) (rest todo) visited)]))]
    (fn-for-room r empty empty)))


(test)
