#lang htdp/asl
(require test-engine/racket-tests)

; count-rooms-starter.rkt

;; Data Definitions:

(define-struct room (name exits))
;; Room is (make-room String (listof Room))
;; interp. the room's name, and list of rooms that the exits lead to

; .

(define H1 (make-room "A" (list (make-room "B" empty))))

; .

(define H2
  (shared ((-0- (make-room "A" (list (make-room "B" (list -0-))))))
    -0-))


; .

(define H3
  (shared ((-A- (make-room "A" (list -B-)))
           (-B- (make-room "B" (list -C-)))
           (-C- (make-room "C" (list -A-))))
    -A-))



; .

(define H4
  (shared ((-A- (make-room "A" (list -B- -D-)))
           (-B- (make-room "B" (list -C- -E-)))
           (-C- (make-room "C" (list -B-)))
           (-D- (make-room "D" (list -E-)))
           (-E- (make-room "E" (list -F- -A-)))
           (-F- (make-room "F" (list))))
    -A-))

;; template: structural recursion, encapsulate w/ local, tail-recursive w/ worklist,
;;           context-preserving accumulator what rooms have we already visited

#;
(define (fn-for-house r0)
  ;; todo is (listof Room); a worklist accumulator
  ;; visited is (listof String); context preserving accumulator, names of rooms already visited
  (local [(define (fn-for-room r todo visited)
            (if (member (room-name r) visited)
                (fn-for-lor todo visited)
                (fn-for-lor (append (room-exits r) todo)
                            (cons (room-name r) visited)))) ; (... (room-name r))
          (define (fn-for-lor todo visited)
            (cond [(empty? todo) (...)]
                  [else
                   (fn-for-room (first todo)
                                (rest todo)
                                visited)]))]
    (fn-for-room r0 empty empty)))

;
; PROBLEM:
;
; Using the following data definition, design a function that consumes a room and produces
; the total number of rooms reachable from the given room. Include the starting room itself.
; Your function should be tail recursive, but you should not use the primitive length function.
;

;; Room -> Natural
;; produce the total number of room that are reachable

(check-expect (reachable-rooms H1) 2) ; Tests
(check-expect (reachable-rooms H4) 6)

;; (define (reachable-rooms r) 1) ; Stub

;; took template from Room

(define (reachable-rooms r)
  ; c is the count of rooms visited
  (local [(define (fn-for-room r todo visited c)
            (if (member (room-name r) visited)
                (fn-for-lor todo visited c)
                (fn-for-lor (append (room-exits r) todo)
                            (cons (room-name r) visited)
                            (add1 c))))
          (define (fn-for-lor todo visited c)
            (cond [(empty? todo) c]
                  [else
                   (fn-for-room (first todo)
                                (rest todo)
                                visited c)]))]
    (fn-for-room r empty empty 0)))




(test)
