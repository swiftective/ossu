#lang racket
(require test-engine/racket-tests)
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; Constants:

(define WIDTH  300)
(define HEIGHT 500)
(define FONT-SIZE 30)
(define FONT-COLOR "white")

(define INVADER-X-SPEED 1)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))
(define INVADER-WIDTH/2 (/ (image-width INVADER) 2))
(define MISSILE (ellipse 5 15 "solid" "red"))

(define MISSILE-Y
  (- HEIGHT (+ (/ (image-height MISSILE) 2) (image-height TANK))))


;; Data Definitions:

(define-struct game (invaders missiles tank) #:transparent)
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loi (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir) #:transparent)
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx) #:transparent)
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right
(define I4 (make-invader (+ 1 (- WIDTH INVADER-WIDTH/2)) (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y) #:transparent)
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))



;; ListOfInvaders is one of:
;; - empty
;; - (cons Invader ListOfInvaders)
;; Interp. list of invaders

(define LOI empty)
(define LOI1 (list (make-invader 10 20 -20)))

#;
(define (fn-for-loi loi)
  (cond
    [(empty? loi) (...)]
    [else
     (...  (fn-for-invader (first loi))
           (fn-for-loi     (rest loi)))]))



;; ListOfMissiles is one of:
;; - empty
;; - (cons Missile ListOfMissiles)
;; Interp. list of missiles
(define LOM empty)
(define LOM1 (list (make-missile 10 20)))

#;
(define (fn-for-lom lom)
  (cond
    [(empty? lom) (...)]
    [else
     (...
      (fn-for-missile (first lom))
      (fn-for-lom     (rest lom)))]))

;; Functions

;; Game -> Game
;; start the world with (main (make-game empty empty T0))

(define (main s)
  (big-bang s                                           ; Game
    (on-tick   next-frame)                      ; Game -> Game
    (to-draw   render-frame)                    ; Game -> Image
    (stop-when game-over? render-game-over)     ; Game -> Boolean ; Game -> Image
    (on-key    handle-key)))                    ; Game KeyEvent -> Game


;; Game -> Game
;; produce the next frame of the game

;; (define (next-frame s) s) ;stub

; template as function composition

(define (next-frame s)
  (remove-hit-invaders (next-space s)))

;; Game -> Game
;; remove the list of invaders and list of missiles
;; that have the same x, y coordinates +/- HIT-RANGE

(check-expect (remove-hit-invaders G0) G0)
(check-expect (remove-hit-invaders G1) G1)

(check-expect (remove-hit-invaders
               (make-game (list (make-invader 10 20 -10)) (list (make-missile 10 20)) T0))
              (make-game empty empty T0))

(check-expect (remove-hit-invaders
               (make-game (list (make-invader 5 20 -10)) (list (make-missile 10 20)) T0))
              (make-game empty empty T0))

(check-expect (remove-hit-invaders
               (make-game (list (make-invader 10 35 -10)) (list (make-missile 10 20)) T0))
              (make-game (list (make-invader 10 35 -10)) (list (make-missile 10 20)) T0))

;; (define (remove-hit-invaders s) s) ;stub

; took template from Game

(define (remove-hit-invaders s)
  (make-game  (filter-invaders  (game-invaders s) (game-missiles s))
              (filter-missiles  (game-missiles s) (game-invaders s))
              (game-tank s)))


;; ListOfInvaders ListOfMissiles -> ListOfInvaders
;; filter hit invaders

(check-expect (filter-invaders empty empty) empty)

(check-expect (filter-invaders
               (list (make-invader 10 20 -19))
               (list (make-missile 10 30)))
              empty)

(check-expect (filter-invaders (list (make-invader 10 20 -19))
                               (list (make-missile 10 35)))
              (list (make-invader 10 20 -19)))

(check-expect (filter-invaders
               (list (make-invader 10 20 -19)
                     (make-invader 20 30 10))
               (list (make-missile 10 30)
                     (make-missile 24 20)))
              empty)

;; (define (filter-invaders loi lom) loi) ;stub

; took template from ListOfInvaders

(define (filter-invaders loi lom)
  (cond
    [(empty? loi) empty]
    [else
     (if (invader-hit? (first loi) lom)
         (filter-invaders (rest loi) lom)
         (cons (first loi) (filter-invaders (rest loi) lom)))]))

;; Invader ListOfMissiles -> Boolean
;; check if invader is in HIT-RANGE of any missile

(check-expect (invader-hit? (make-invader 10 20 -19) empty) false)

(check-expect (invader-hit? (make-invader 10 20 -19)
                            (list (make-missile 10 30))) true)

(check-expect (invader-hit? (make-invader 10 20 -19)
                            (list (make-missile 10 35)
                                  (make-missile 10 35)
                                  (make-missile 10 35))) false)

;; (define (invader-hit? i lom) true) ;stub

(define (invader-hit? i lom)
  (cond
    [(empty? lom) false]
    [else
     (if (and
          (and (<= (missile-y (first lom)) (+ HIT-RANGE (invader-y i)))
               (>= (missile-y (first lom)) (- (invader-y i) HIT-RANGE)))
          (and (<= (missile-x (first lom)) (+ HIT-RANGE (invader-x i)))
               (>= (missile-x (first lom)) (- (invader-x i) HIT-RANGE))))
         #t
         (invader-hit? i (rest lom))) ]))

;; ListOfMissiles ListOfInvaders -> ListOfMissiles
;; check if the missile is in HIT-RANGE of any invader

(check-expect (filter-missiles empty empty) empty)

(check-expect (filter-missiles (list (make-missile 10 30))
                               (list (make-invader 10 20 -19))) empty)

(check-expect (filter-missiles (list (make-missile 10 35))
                               (list (make-invader 10 20 -19)))
              (list (make-missile 10 35)))

(check-expect (filter-missiles (list (make-missile 10 30)
                                     (make-missile 24 30))
                               (list (make-invader 10 20 -19)
                                     (make-invader 20 25 10))) empty)

;; (define (filter-missiles lom loi) lom) ;stub

; took template from ListOfMissiles

(define (filter-missiles lom loi)
  (cond
    [(empty? lom) empty]
    [else
     (if (missile-hit? (first lom) loi)
         (filter-missiles (rest lom) loi)
         (cons (first lom) (filter-missiles (rest lom) loi)))]))

;; Invader ListOfMissiles -> Boolean
;; check if invader is in HIT-RANGE of any missile

(check-expect (missile-hit? (make-missile 10 20) empty) false)

(check-expect (missile-hit? (make-missile 10 30)
                            (list (make-invader 10 20 -19))) true)

(check-expect (missile-hit? (make-missile 10 35)
                            (list (make-invader 10 20 -19))) false)

;; (define (missile-hit? m loi) true) ;stub

(define (missile-hit? m loi)
  (cond
    [(empty? loi) false]
    [else
     (if (and
          (and (<= (invader-y (first loi)) (+ HIT-RANGE (missile-y m)))
               (>= (invader-y (first loi)) (- (missile-y m) HIT-RANGE)))
          (and (<= (invader-x (first loi)) (+ HIT-RANGE (missile-x m)))
               (>= (invader-x (first loi)) (- (missile-x m) HIT-RANGE))))
         #t
         (missile-hit? m (rest loi))) ]))

;; Game -> Game
;; update the space elements

;; (define (next-space s) s) ;stub

; took template from Game

(define (next-space s)
  (make-game  (next-invaders  (game-invaders s))
              (next-missiles  (game-missiles s))
              (next-tank      (game-tank s))))

;; ListOfInvaders -> ListOfInvaders
;; add new invader to list
;; if first invader in the list has a invader-y more than INVADE-RATE
;; or list if empty
;; and also move the invader-y of every invader by 1 pixel
;; and also move the invader-x of every invader by 1 pixel
;; if the invader-x is more than WIDTH, invert the invader-dx

(check-expect (cons? (next-invaders empty)) true)
(check-expect (cons? (next-invaders (list (make-invader 10 200 10)))) true)
(check-expect (cons? (next-invaders (list (make-invader 0 200 10)))) true)

;; (define (next-invaders loi) loi) ;stub

; template as function composition

(define (next-invaders loi)
  (move-invaders (add-invaders loi)))

;; ListOfInvaders -> ListOfInvaders
;; increment the invader-y of every invader by INVADER-Y-SPEED on every invader in list
;; increment the invader-x of every invader by invader-x of every invader in list
;; if the invader-x is more than (- WIDTH INVADER-HEIGHT/2), invert the invader-dx
;; if the invader-x is less than INVADER-HEIGHT/2, invert the invader-dx


(check-expect
 (move-invaders (list I1 I2 I3 I4))
 (list
  (move-invader I1)
  (move-invader I2)
  (move-invader I3)
  (move-invader I4)))

(check-expect (move-invaders empty) empty)

;; (define (move-invaders loi) loi) ;stub

; took template from ListOfInvaders

(define (move-invaders loi)
  (cond
    [(empty? loi) empty]
    [else
     (cons (move-invader   (first loi))
           (move-invaders  (rest loi)))]))

;; Invader -> Invader
;; increment the invader-x of every invader by invader-x
;; increment the invader-y of every invader by INVADER-Y-SPEED
;; if the invader-x is more than (- WIDTH INVADER-HEIGHT/2), invert the invader-dx
;; if the invader-x is less than INVADER-HEIGHT/2, invert the invader-dx

(check-expect
 (move-invader I1)
 (make-invader
  (+ (invader-x I1) (invader-dx I1))
  (+ (invader-y I1) INVADER-Y-SPEED)
  (invader-dx I1)))

(check-expect
 (move-invader I4)
 (make-invader
  (+ (invader-x I4) (- (invader-dx I4)))
  (+ (invader-y I4) INVADER-Y-SPEED)
  (- (invader-dx I4))))

;; (define (move-invader i) i) ;stub

(define (move-invader i)
  (cond
    [(change-direction? i)
     (make-invader (+ (invader-x i) (- (invader-dx i)))
                   (+ (invader-y i) INVADER-Y-SPEED)
                   (- (invader-dx i)))]
    [else
     (make-invader (+ (invader-x i) (invader-dx i))
                   (+ (invader-y i) INVADER-Y-SPEED)
                   (invader-dx i))]))

;; Invader -> Boolean
;; check if should change direction of the invader

(check-expect (change-direction? (make-invader 0 10 -1)) true)
(check-expect (change-direction? (make-invader 0 10 1)) false)
(check-expect (change-direction? (make-invader WIDTH 10 1)) true)
(check-expect (change-direction? (make-invader WIDTH 10 -1)) false)

;; (define (move-invader? i) true) ;stub

(define (change-direction? i)
  (or
    (and (> (+ (invader-x i) (invader-dx i)) (- WIDTH INVADER-WIDTH/2))
         (> (invader-dx i) 0))
    (and
      (< (+ (invader-x i) (invader-dx i)) INVADER-WIDTH/2)
      (< (invader-dx i) 0))))


;; ListOfInvaders -> ListOfInvaders
;; if first invader in the list has a invader-y more than INVADE-RATE
;; or list if empty RANDOMLY

(check-expect (cons? (add-invaders empty)) true)

;; (define (add-invaders loi) loi) ;stub

(define (add-invaders loi)
  (cond
    [(empty? loi)
     (cons
       (make-invader (random INVADER-WIDTH/2 (- WIDTH INVADER-WIDTH/2)) -1 INVADER-X-SPEED) empty)]
    [else (if (add-invader? (first loi))
              (cons (make-invader (random WIDTH) -1 INVADER-X-SPEED) loi)
              loi)]))


;; Invader -> Boolean
;; produce true if the invader-y of invader more than INVADE-RATE, else false

(check-expect (add-invader? (make-invader 39 40 10)) false)
(check-expect (add-invader? (make-invader 39 INVADE-RATE 10)) true)

;; (define (add-invader? loi) true) ;stub

(define (add-invader? i)
  (>= (invader-y i) INVADE-RATE))

;; ListOfMissiles -> ListOfMissiles
;; increment the missile-y position of every missile in list by MISSILE-SPEED
;; remove off-screen ones

(check-expect (next-missiles LOM)
              (remove-offscreen (move-missiles LOM)))

(check-expect (next-missiles LOM1)
              (remove-offscreen (move-missiles LOM1)))

;; (define (next-missiles lom) lom) ;stub

; template as function composition

(define (next-missiles lom)
  (remove-offscreen (move-missiles lom)))

;; ListOfMissiles -> ListOfMissiles
;; increment the missile-y position of every missile in list by MISSILE-SPEED

(check-expect (move-missiles empty) empty)

(check-expect (move-missiles (list M1))
              (list (make-missile 150 (- 300 MISSILE-SPEED))))

;; (define (move-missiles lom) lom) ;stub

(define (move-missiles lom)
  (cond
    [(empty? lom) empty]
    [else
     (cons
      (move-missile     (first lom))
      (move-missiles    (rest lom)))]))

;; Missile -> Missile
;; decrement the missile-y position by MISSILE-SPEED

(check-expect (move-missile M1)
              (make-missile 150 (- 300 MISSILE-SPEED)))

;; (define (move-missile m) m) ;stub

(define (move-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

;; ListOfMissiles -> ListOfMissiles
;; remove the missiles that are off-screen

(check-expect (remove-offscreen empty) empty)
(check-expect (remove-offscreen (list (make-missile 10 (- (image-height MISSILE))))) empty)
(check-expect (remove-offscreen (list (make-missile 10  0)))
              (list (make-missile 10 0)))

;; (define (remove-offscreen lom) lom) ;stub

; took template from ListOfMissiles

(define (remove-offscreen lom)
  (cond
    [(empty? lom) empty]
    [else
     (if (off-screen? (first lom))
         (remove-offscreen (rest lom))
         (cons (first lom) (remove-offscreen (rest lom))))]))

;; Missile -> Boolean
;; produce true if missile-y if less than (image-height MISSILE)

(check-expect (off-screen? (make-missile 10 (- (image-height MISSILE)))) true)
(check-expect (off-screen? (make-missile 10  0)) false)

;; (define (off-screen? m) true) ;stub

(define (off-screen? m)
  (<= (missile-y m)
      (- (image-height MISSILE))))


;; Tank -> Tank
;; move the tank by TANK-SPEED in appropiate direction
;; increment the tank-x by (* (tank-dir tank) TANK-SPEED)
;;
;; if tank-x is more than or equal (- WIDTH (/ (image-width TANK) 2)) and (tank-dir) is 1
;; if tank-x is less than or equal (/ (image-width TANK) 2) and (tank-dir) is -1
;; return same tank

#| (define T0 (make-tank (/ WIDTH 2) 1))   ;center going right |#
#| (define T1 (make-tank 50 1))            ;going right |#
#| (define T2 (make-tank 50 -1))           ;going left |#

(check-expect (next-tank T0) (make-tank (+ TANK-SPEED (/ WIDTH 2)) 1))
(check-expect (next-tank T1) (make-tank (+ TANK-SPEED 50) 1))
(check-expect (next-tank T2) (make-tank (+ (- TANK-SPEED) 50) -1))

;; (define (next-tank t) t) ;stub

(define (next-tank t)
  (cond
    [(and (>= (tank-x t) (- WIDTH (/ (image-width TANK) 2)))
          (= (tank-dir t) 1)) t]
    [(and (<= (tank-x t) (/ (image-width TANK) 2))
          (= (tank-dir t) -1)) t]
    [else (make-tank (+ (tank-x t)
                        (* (tank-dir t) TANK-SPEED)) (tank-dir t))]))


;; Game -> Image
;; render the next-frame 0f Game

;; (define (render-frame s) s) ;stub

(define (render-frame s)
  (render-invaders
   (game-invaders s)
   (render-missiles
    (game-missiles s)
    (render-tank (game-tank s)))))


;; Tank Image -> Image
;; place TANK on (tank-x), (- HEIGHT TANK-HEIGHT/2)

(check-expect
 (render-tank T0)
 (place-image TANK (tank-x T0) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

;; (define (render-tank t) BACKGROUND) ;stub

(define (render-tank t)
  (place-image TANK (tank-x t) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))


;; ListOfInvaders Image -> Image
;; place every invader in list on appropiate x, y coordinates on consumed bg

(check-expect (render-invaders empty BACKGROUND) BACKGROUND)

(check-expect (render-invaders (list I1 I2) BACKGROUND)
              (place-image INVADER (invader-x I1) (invader-y I1)
                           (place-image INVADER (invader-x I2) (invader-y I2) BACKGROUND)))


;; (define (render-invaders loi bg) bg) ;stub

(define (render-invaders loi bg)
  (cond
    [(empty? loi) bg]
    [else
     (place-image
      INVADER
      (invader-x (first loi))
      (invader-y (first loi))
      (render-invaders (rest loi) bg))]))

;; ListOfMissiles Image -> Image
;; place every missile in list on appropiate x, y coordinates on consumed bg


(check-expect (render-missiles empty BACKGROUND) BACKGROUND)

(check-expect (render-missiles (list M1 M2) BACKGROUND)
              (place-image MISSILE (missile-x M1) (missile-y M1)
                           (place-image MISSILE (missile-x M2) (missile-y M2) BACKGROUND)))

;; (define (render-missiles lom bg) bg) ;stub

(define (render-missiles lom bg)
  (cond
    [(empty? lom) bg]
    [else
     (place-image
      MISSILE
      (missile-x (first lom))
      (missile-y (first lom))
      (render-missiles (rest lom) bg))]))

;; Game KeyEvent -> Game
;; Add a new missile to list when space bar is pressed
;; Change the direction of the tank when left key is pressed
;; change the direction of the tank when right key is pressed

;; (define (handle-key s ke) s) ;stub

; template from KeyEvent

(define (handle-key s ke)
  (cond [(or
           (key=? ke "i")
           (key=? ke " "))     (shoot-missile s)]
        [(or
           (key=? ke "h")
           (key=? ke "left"))  (tank-left s)]
        [(or
           (key=? ke "l")
           (key=? ke "right")) (tank-right s)]
        [else s]))


;; Game -> Game
;; Add missile to ListOfMissiles in (game-missiles)
;; on appropiate x coordinate (tank-x (game-tank s)) and y coordinate MISSILE-Y

(check-expect (shoot-missile G0)
              (make-game (game-invaders G0)
                         (add-missile (tank-x (game-tank G0)) MISSILE-Y (game-missiles G0))
                         (game-tank G0)))

;; (define (shoot-missile s) s) ;stub

(define (shoot-missile s)
  (make-game (game-invaders s)
             (add-missile (tank-x (game-tank s)) MISSILE-Y (game-missiles s))
             (game-tank s)))

;; ListOfMissiles Number Number -> ListOfMissiles
;; add new missile in appropiate x, y coordinate

(check-expect (add-missile 10 20 empty)
              (list (make-missile 10 20)))

(check-expect
 (add-missile 10 20 (list M1 M2))
 (list (make-missile 10 20) M1 M2))

;; (define (add-missile x y lom) lom) ;stub

(define (add-missile x y lom)
  (cons (make-missile x y) lom))

;; Game -> Game
;; change the direction of tank to left

(check-expect (tank-left G0)
              (make-game
               (game-invaders G0)
               (game-missiles G0)
               (make-tank (tank-x (game-tank G0)) -1)))

;; (define (tank-left s) s) ;stub


(define (tank-left s)
  (make-game (game-invaders s)
             (game-missiles s)
             (make-tank (tank-x (game-tank s)) -1)))


;; Game -> Game
;; change the direction of tank to right

(check-expect (tank-right G0)
              (make-game
               (game-invaders G0)
               (game-missiles G0)
               (make-tank (tank-x (game-tank G0)) 1)))

;; (define (tank-right s) s) ;stub

(define (tank-right s)
  (make-game (game-invaders s)
             (game-missiles s)
             (make-tank (tank-x (game-tank s)) 1)))

;; Game -> Boolean
;; produce true if any (invader-y) in the ListOfInvaders is more than HEIGHT

(check-expect
 (game-over? G0)
 (invaders-won? (game-invaders G0)))

;; (define (game-over? s) true) ;stub

(define (game-over? s)
  (invaders-won? (game-invaders s)))


;; ListOfInvaders -> Boolean
;; produce true if any (invader-y) in the ListOfInvaders is more than HEIGHT

(check-expect (invaders-won? empty) false)
(check-expect (invaders-won? (list (make-invader 10 HEIGHT -2))) true)
(check-expect (invaders-won? (list (make-invader 10 (- 1 HEIGHT) -2))) false)

;; (define (invaders-won? loi) true) ;stub

(define (invaders-won? loi)
  (cond
    [(empty? loi) false]
    [else
     (if (invader-won?   (first loi))
         true
         (invaders-won?     (rest loi)))]))


;; Invader -> Boolean
;; produce true if (invader-y) is more than HEIGHT

(check-expect (invader-won? (make-invader 10 HEIGHT -2)) true)
(check-expect (invader-won? (make-invader 10 (- 1 HEIGHT) -2)) false)

;; (define (invader-won? i) true) ;stub

(define (invader-won? i)
  (>= (invader-y i) HEIGHT))

;; Game -> Image
;; place rectangle on the center
;; with GAME OVER text in center on last game frame

(check-expect
 (render-game-over G0)
 (overlay
  (overlay
   (text "GAME OVER" FONT-SIZE FONT-COLOR)
   (rectangle (* WIDTH (/ 80 100))
              (* HEIGHT (/ 20 100))
              "solid"
              "red"))
  (render-frame G0)))

;; (define (render-game-over s) BACKGROUND) ;stub

(define (render-game-over s)
  (overlay
   (overlay
    (text "GAME OVER" FONT-SIZE FONT-COLOR)
    (rectangle (* WIDTH (/ 80 100))
               (* HEIGHT (/ 20 100))
               "solid"
               "red"))
   (render-frame s)))

(main (make-game empty empty T0))

(test)
