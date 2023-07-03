#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ stride low) high stride))))

(define (string-append-map xs suffix)
  (map (λ (s) (string-append s suffix)) xs))


(define (list-nth-mod xs n)
  (cond
    [(< n 0) (error "list-nth-mod: negative number")]
    [(null? xs) (error "list-nth-mod: empty list")]
    [#t
     (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (if (<= n 0)
    null
    (let ([x (s)])
      (cons (car x) (stream-for-n-steps (cdr x) (- n 1))))))

(define funny-number-stream
  (letrec ([g (λ (n) (if (= (modulo n 5) 0) (- n) n))]
           [f (λ (n) (cons (g n) (λ () (f (+ n 1)))))])
    (λ () (f 1))))

(define dan-then-dog
  (letrec ([dan-f (λ () (cons "dan.jpg" dog-f))]
           [dog-f (λ () (cons "dan.jpg" dan-f))])
    dan-f))

(define (stream-add-zero s)
  (λ ()
    (let ([x (s)])
      (cons (cons 0 (car x)) (stream-add-zero (cdr x))))))

(define (cycle-lists xs ys)
  (letrec ([r (λ (x y) (if (null? x) y x))]
           [f (λ (lsta lstb)
                (λ () (let ([xs (r lsta xs)] [ys (r lstb ys)])
                         (cons (cons (car xs) (car ys)) (f (cdr xs) (cdr ys))))))])
    (f xs ys)))

(define (vector-assoc v vec)
  (letrec ([g (λ (n) (let ([r (vector-ref vec n)])
                          (if (equal? (car r) v) r (f (+ n 1)))))]
           [f (λ (n)
                (cond
                  [(= n (vector-length vec)) #f]
                  [(pair? (vector-ref vec n)) (g n)]
                  [#t (f (+ n 1))]))])
    (f 0)))

(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [next-pos 0]
           [cache-ans (λ (v)
                        (begin
                          (vector-set! cache next-pos v)
                          (set! next-pos (modulo (+ next-pos 1) n))))])
    (λ (v)
      (let ([cv (vector-assoc v cache)])
        (if cv cv
          (let ([ans (assoc v xs)])
            (if ans (begin (cache-ans ans) ans) ans)))))))
