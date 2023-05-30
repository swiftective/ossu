#lang htdp/asl
(require test-engine/racket-tests)

; max-exits-from-starter.rkt

;; Data Definitions:

(define-struct room (name exits))
;; Room is (make-room String (listof Room))
;; interp. the room's name, and list of rooms that the exits lead to

(define H1 (make-room "A" (list (make-room "B" empty))))

(define H2
  (shared ((-0- (make-room "A" (list (make-room "B" (list -0-))))))
    -0-))


(define H3
  (shared ((-A- (make-room "A" (list -B-)))
           (-B- (make-room "B" (list -C-)))
           (-C- (make-room "C" (list -A-))))
    -A-))



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
; Using the following data definition, design a function that produces the room with the most exits
; (in the case of a tie you can produce any of the rooms in the tie).


; Room -> Room
; produce the Room with most exits

(check-expect (most-exits H4) H4)
(check-expect (most-exits H3) H3)

;; (define (most-exits r) r)

(define (most-exits r)
  (local [
          ; mer is Room with the most exits
          (define (fn-for-room r todo visited mer)
            (if (member (room-name r) visited)
                (fn-for-lor todo visited mer)
                (fn-for-lor (append (room-exits r) todo)
                            (cons (room-name r) visited)
                            (if (< (length (room-exits mer))
                                   (length (room-exits r)))
                                r mer))))

          (define (fn-for-lor todo visited mer)
            (cond [(empty? todo) mer]
                  [else
                   (fn-for-room (first todo)
                                (rest todo)
                                visited
                                mer)]))]
    (fn-for-room r empty empty r)))


(test)
