#lang racket
(require test-engine/racket-tests)

;; student-starter.rkt

;; =================
;; Data definitions:

; PROBLEM A:
;
; Design a data definition to help a teacher organize their next field trip.
; On the trip, lunch must be provided for all students. For each student, track
; their name, their grade (from 1 to 12), and whether or not they have allergies.

(define-struct student (n g a?))
;; Student is (make-student String Natural[1, 12] Boolean)
;; Interp. (make-student name grade) is a student with
;;          n is name of student
;;          g is the grade of student (from 1 to 12)
;;          a is the whether they have allergies or not
(define S1 (make-student "John" 3 #t))
(define S2 (make-student "Alex" 12 #f))
(define S3 (make-student "Jones" 8 #t))
(define S4 (make-student "Mary" 4 #f))

#;
(define (fn-for-student s)
  (...
    (student-n s)   ; String
    (student-g s)   ; Natural[1, 12]
    (student-a s))) ; Boolean

;; Template rules used
;; - Compound: 3 fields

;; =================
;; Functions:

; PROBLEM B:
;
; To plan for the field trip, if students are in grade 6 or below, the teacher
; is responsible for keeping track of their allergies. If a student has allergies,
; and is in a qualifying grade, their name should be added to a special list.
; Design a function to produce true if a student name should be added to this list.

;; Student -> Boolean
;; produce true if student has allergies AND is grade 6 or below

(check-expect (special? S1) #t) ; Tests
(check-expect (special? S2) #f)
(check-expect (special? S3) #f)
(check-expect (special? S4) #f)

;; (define (special? s) true) ; Stub

;; (define (special? s) ; Template
;;    (... (student-n s)
;;         (student-g s)
;;         (student-a s)))

(define (special? s)
  (and (<= (student-g s) 6) (student-a? s)))

(test)
