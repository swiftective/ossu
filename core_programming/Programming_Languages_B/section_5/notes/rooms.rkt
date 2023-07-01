#lang racket

(define-struct room (name lor-fn))


(define A (make-room "A" (λ () (list B C))))
(define B (make-room "B" (λ () (list A))))
(define C (make-room "C" (λ () (list B A D))))
(define D (make-room "D" (λ () null)))

(define (list-rooms r)

  (define (list-rooms r vr wl)

    (define lor ((room-lor-fn r)))

    (if (visited? r vr)
      (list-rooms-lor vr wl)
      (list-rooms-lor
        (cons r vr)
        (append wl lor))))

  (define (list-rooms-lor vr wl)
    (cond
      [(null? wl) (reverse vr)]
      [#t
       (list-rooms (car wl) vr (cdr wl))]))

  (define (visited? r1 vr)
    (define rn (room-name r1))
    (ormap
      (λ (r2)
        (string=?
          rn (room-name r2))) vr))

  (list-rooms r null null))

(map
  (λ (r) (room-name r))
  (list-rooms A)) ; ("A", "B", "C", "D")
