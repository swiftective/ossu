#lang htdp/asl
(require test-engine/racket-tests)

; max-exits-to-starter.rkt

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
; Using the following data definition, design a function that produces the room to which the greatest
; number of other rooms have exits (in the case of a tie you can produce any of the rooms in the tie).
;


;; Room -> Room
;; produces the room to which the greatest
;; number of other rooms have exits (in the case of a tie you can produce any of the rooms in the tie)

(check-expect (most-exits-to H1)
              (first (room-exits H1)))

(check-expect (most-exits-to H4)
              (first (room-exits H4)))

(check-expect (most-exits-to H1) (first (room-exits H1)))
(check-expect (most-exits-to H2) (shared ((-A- (make-room "A" (list -B-)))
                                          (-B- (make-room "B" (list -A-))))
                                   -B-))

(check-expect (most-exits-to H4)
              (shared ((-A- (make-room "A" (list -B- -D-)))
                       (-B- (make-room "B" (list -C- -E-)))
                       (-C- (make-room "C" (list -B-)))
                       (-D- (make-room "D" (list -E-)))
                       (-E- (make-room "E" (list -F- -A-)))
                       (-F- (make-room "F" (list))))
                -B-))

(check-expect (most-exits-to (shared ((-A- (make-room "A" (list -B- -D-)))
                                      (-B- (make-room "B" (list -C- -E-)))
                                      (-C- (make-room "C" (list -B-)))
                                      (-D- (make-room "D" (list -E- -C-)))
                                      (-E- (make-room "E" (list -F- -A- -C-)))
                                      (-F- (make-room "F" (list))))
                               -A-))
              (shared ((-A- (make-room "A" (list -B- -D-)))
                       (-B- (make-room "B" (list -C- -E-)))
                       (-C- (make-room "C" (list -B-)))
                       (-D- (make-room "D" (list -E- -C-)))
                       (-E- (make-room "E" (list -F- -A- -C-)))
                       (-F- (make-room "F" (list))))
                -C-))

;; (define (most-exits-to r) r) ; Stub

;; took template from Room

(define (most-exits-to r)
  (local [
          ; ETR (Exits-to-room) is (make-etr Room Natural)
          ; count of room exits leading to a room
          (define-struct etr (room count))

          ;; Room (listof ETR) -> (listof ETR)
          ;; add1 to the (etr-room) that is equal to room consumed
          ;; if no room found add new ETR with room and count 1
          ;; (define (add-count r loetr) loetr)
          (define (add-count r loetr)
            (cond
              [(empty? loetr) (list (make-etr r 1))]
              [else
               (if (string=?
                    (room-name r)
                    (room-name (etr-room (first loetr))))
                   (cons (make-etr
                          (etr-room (first loetr))
                          (add1 (etr-count (first loetr)))) (rest loetr))
                   (cons
                    (first loetr)
                    (add-count r (rest loetr))))]))

          ;; (listof Room) (listof ETR) -> (listof ETR)
          ;; add1 for all the rooms available in loetr that contains room from lor
          ;; (define (add-counts lor loetr) loetr)
          (define (add-counts lor loetr)
            (cond
              [(empty? lor) loetr]
              [(empty? loetr)
               (map
                (λ (r) (make-etr r 1)) lor)]
              [else
               (add-counts
                (rest lor)
                (add-count (first lor) loetr))]))

          ;; (listof ETR) -> ETR
          ;; produce ETR with the highest count
          ;; (define (most-count loetr) (etr-room (first loetr)))
          (define (most-count loetr)
            (foldr
             (λ (c p)
               (if (> (etr-count c) (etr-count p)) c p))
             (first loetr)
             (rest loetr)))

          ;; loetr is (listof ETR), context-preserving accumulator
          (define (fn-for-room r todo visited loetr)
            (if (member (room-name r) visited)
                (fn-for-lor todo visited loetr)
                (fn-for-lor (append (room-exits r) todo)
                            (cons (room-name r) visited)
                            (add-counts
                             (room-exits r) loetr))))

          (define (fn-for-lor todo visited loetr)
            (cond [(empty? todo) (etr-room (most-count loetr))]
                  [else
                   (fn-for-room (first todo)
                                (rest todo)
                                visited
                                loetr)]))]
    (fn-for-room r empty empty empty)))


(test)
