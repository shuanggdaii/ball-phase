;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |ball-phase 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/image)
(require 2htdp/universe)

;; Constantes:
(define WIDTH 400)
(define HEIGHT 600)
(define RADIUS 15)
(define COLOR-PADDLE (make-color 229 213 73))
(define COLOR-BALL "white")
(define BALL (circle RADIUS "solid" COLOR-BALL))
(define BG (rectangle WIDTH HEIGHT "solid" (make-color 3 37 108)))
(define TDISTANCE RADIUS)
(define BDISTANCE (- HEIGHT RADIUS))
(define LDISTANCE RADIUS)
(define RDISTANCE (- WIDTH RADIUS))
(define X-VELOCITY (random 41))
(define Y-VELOCITY (random 21))
(define PADDLE (rectangle 80 10 "solid" COLOR-PADDLE))

;;===
;; Data definitions:
(define-struct ball (x y vx vy))
;; Ball is (make-ball Number Number X-VELOCITY Y-VELOCITY)
;; interp. (make-ball (x y vx vy)) is BALL's x and y coordinate,
;;                                 x-coord moving velocity, and
;;                                 x-coord moving velocity

(define B1 (make-ball 10 20 20 5))
(define B2 (make-ball (/ WIDTH 2) (/ HEIGHT 2) 10 2))

;; template rules used:
;; compound 4 fields
#;
(define (fn-for-ball b)
  (... (ball-x b)
       (ball-y b)
       (ball-vx b)
       (ball-vy b)))


;;ListOfBall is one of:
;; - empty
;; - (cons Ball ListOfBall)
;; interp. a list of ball

(define LOB0 empty)
(define LOB1 (cons B1 empty))
(define LOB2 (cons B2 (cons B1 empty)))

;; template rules used:
;; one of (2 cases)
;; atomic-distinct: empty
;; compound: (cons Ball ListOfBall)
;; reference rule: (first lob) is Ball
;; self-reference: (rest lob)is ListOfBall

#;
(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else
         (... (fn-for-ball (first los))
              (fn-for-lob (rest los)))]))


(define-struct game (lob p))
;; Game is (make-game ListOfBall Number[0,WIDTH])
;; interp. (make-game (lob p)) is ListOfBall and PADDLE'S x coord.

(define G0 (make-game empty 0))
(define G1 (make-game LOB1 1))
(define G2 (make-game LOB2 (/ WIDTH 2)))

;; template rules used:
;; compound 2 fields
#;
(define (fn-for-game g)
  (... (game-lob g)
       (game-p g)))

;;====
;; Functions:

;; start with (main make-game(empty (/ WIDTH 2))
;; no testing for main

(define (main g)
  (big-bang g
            ;(state true)
            (on-tick next-game)    ;Game->Game
            (to-draw render-game)  ;Game->Image
            (on-mouse add-ball)    ;Game MouseEvent->Game
            (on-key handle-key)))  ;Game KeyEvent->Game

;;Game->Game
;;output next Game
(check-expect (next-game G0)
              (make-game empty 0))
(check-expect (next-game G1)
              (make-game (cons (make-ball 30 25 20 5) empty) 1))
(check-expect (next-game G2)
              (make-game (cons (make-ball 210 302 10 2)
                               (cons (make-ball 30 25 20 5) empty))
                         200))

(define (next-game g)
  (make-game (touch? (next-balls (game-lob g)) (game-p g)) (next-paddle g)))

;;Game->Game
;;output next paddle
(check-expect (next-paddle G0) 0)
(check-expect (next-paddle G1) 1)
(check-expect (next-paddle G2) (/ WIDTH 2))

(define (next-paddle g)
  (game-p g))

;;ListOfBall->ListOfBall
;;output the next ListOfBall, if it touches the paddle
;;                            it will disappear from the Game.

(check-expect (touch? LOB0 5) empty)
(check-expect (touch? LOB1 10) (cons (make-ball 10 20 20 5) empty))
(check-expect (touch? LOB2 15) (cons (make-ball 200 300 10 2)
                                     (cons (make-ball 10 20 20 5)
                                           empty)))
              
(define (touch? lob p)
  (cond[(empty? lob) empty]
       [else
        (if (if-touch-paddle (first lob) p)
            (touch? (rest lob) p)
            (cons (first lob) (touch? (rest lob) p)))]))

;;Ball->Boolean
;;check if the ball touched the paddle
(check-expect (if-touch-paddle B1 50) false)
(check-expect (if-touch-paddle B2 100) false)
(check-expect (if-touch-paddle (make-ball 200 480 60 4) 200) true)

(define (if-touch-paddle b p)
  (cond [(and (<= (- p 55) (+ (ball-x b) RADIUS) (+ p 55))
              (<= (- (* HEIGHT 4/5) 15) (+ (ball-y b) RADIUS) (+ (* HEIGHT 4/5) 15)))
         true]
        [else false]))

;;ListOfBall->ListOfBall
;;output the next ListOfBall
(check-expect (next-balls LOB0) empty)
(check-expect (next-balls LOB1) (cons (make-ball 30 25 20 5) empty))
(check-expect (next-balls LOB2) (cons (make-ball 210 302 10 2)
                                      (cons (make-ball 30 25 20 5) empty)))
(define (next-balls lob)
  (cond [(empty? lob) empty]
        [else
         (cons (next-ball (first lob))
               (next-balls (rest lob)))]))

;;Ball->Ball
;;output the next Ball
(check-expect (next-ball (make-ball 20 5 X-VELOCITY 5))
              (make-ball (- 20 X-VELOCITY) 
                         5
                         X-VELOCITY
                         -5))
(check-expect (next-ball (make-ball 20 599 X-VELOCITY 15))
              (make-ball (- 20 X-VELOCITY)
                         599
                         X-VELOCITY
                         -15))
(check-expect (next-ball (make-ball 1 599 5 Y-VELOCITY))
              (make-ball -4
                         599
                         5
                         (- Y-VELOCITY)))
(check-expect (next-ball (make-ball 385 599 5 Y-VELOCITY))
              (make-ball 380
                         599
                         5
                         (- Y-VELOCITY)))
(check-expect (next-ball (make-ball 20 15 X-VELOCITY Y-VELOCITY))
              (make-ball (+ 20 X-VELOCITY)
                         (+ 15 Y-VELOCITY)
                         X-VELOCITY
                         Y-VELOCITY))

(define (next-ball b)
  (cond [(collide-top b) (flip-top b)]
        [(collide-bottom b) (flip-bottom b)]
        [(collide-left b) (flip-left b)]
        [(collide-right b) (flip-right b)]
        [else
         (simple-move b)]))

;;Ball->Boolean
;;output if the ball touches the edge distance

;top edge
(check-expect (collide-top (make-ball 20 5 X-VELOCITY 5))
              true)
(check-expect (collide-top (make-ball 20 15 X-VELOCITY 15))
              false)
(check-expect (collide-top (make-ball 50 60 X-VELOCITY 6))
              false)

(define (collide-top b)
  (if (< (+ (ball-y b) (ball-vy b)) TDISTANCE)
      true
      false))

;bottom edge
(check-expect (collide-bottom (make-ball 20 599 X-VELOCITY 15))
              true)
(check-expect (collide-bottom (make-ball 20 400 X-VELOCITY -15))
              false)
(check-expect (collide-bottom (make-ball 50 300 X-VELOCITY -15))
              false)

(define (collide-bottom b)
  (if (> (+ (ball-y b) (ball-vy b)) BDISTANCE)
      true
      false))

;left edge
(check-expect (collide-left (make-ball 1 599 5 Y-VELOCITY))
              true)
(check-expect (collide-left (make-ball 20 400 5 Y-VELOCITY))
              false)
(check-expect (collide-left (make-ball 5 600 5 Y-VELOCITY))
              true)

(define (collide-left b)
  (if (< (+ (ball-x b) (ball-vx b)) LDISTANCE)
      true
      false))

;right edge
(check-expect (collide-right (make-ball 385 599 5 Y-VELOCITY))
              true)
(check-expect (collide-right (make-ball 20 400 5 Y-VELOCITY))
              false)
(check-expect (collide-right (make-ball 8 234 5 Y-VELOCITY))
              false)

(define (collide-right b)
  (if (> (+ (ball-x b) (ball-vx b)) RDISTANCE)
      true
      false))

;;Ball->Ball
;;output the next ball
;top edge
(check-expect (flip-top (make-ball 20 5 X-VELOCITY 5))
              (make-ball (- 20 X-VELOCITY) 
                 5
                 X-VELOCITY
                 -5))
(check-expect (flip-top (make-ball 50 5 X-VELOCITY 10))
              (make-ball (- 50 X-VELOCITY) 
                 5
                 X-VELOCITY
                 -10))
(check-expect (flip-top (make-ball 100 5 X-VELOCITY 15))
              (make-ball (- 100 X-VELOCITY) 
                 5
                 X-VELOCITY
                 -15))

(define (flip-top b)
      (make-ball (- (ball-x b) (ball-vx b))
                 (ball-y b)
                 (ball-vx b)
                 (* -1 (ball-vy b))))

;bottom edge
(check-expect (flip-bottom (make-ball 20 599 X-VELOCITY 15))
              (make-ball (- 20 X-VELOCITY)
                         599
                         X-VELOCITY
                         -15))
(check-expect (flip-bottom (make-ball 30 599 X-VELOCITY 15))
              (make-ball (- 30 X-VELOCITY)
                         599
                         X-VELOCITY
                         -15))
(check-expect (flip-bottom (make-ball 50 599 X-VELOCITY 15))
              (make-ball (- 50 X-VELOCITY)
                         599
                         X-VELOCITY
                         -15))

(define (flip-bottom b)
  (make-ball (- (ball-x b) (ball-vx b))
             (ball-y b)
             (ball-vx b)
             (* -1 (ball-vy b))))

;left edge
(check-expect (flip-left (make-ball 1 20 5 Y-VELOCITY))
              (make-ball -4
                         20
                         -5
                         Y-VELOCITY))
(check-expect (flip-left (make-ball 1 500 10 Y-VELOCITY))
              (make-ball -9
                         500
                         -10
                         Y-VELOCITY))
(check-expect (flip-left (make-ball 1 300 50 Y-VELOCITY))
              (make-ball -49
                         300
                         -50
                         Y-VELOCITY))

(define (flip-left b)
  (make-ball (- (ball-x b) (ball-vx b))
             (ball-y b)
             (* -1 (ball-vx b))
             (ball-vy b)))

;right edge
(check-expect (flip-right (make-ball 200 599 5 Y-VELOCITY))
              (make-ball 195
                         599
                         -5
                         Y-VELOCITY))
(check-expect (flip-right (make-ball 500 599 16 Y-VELOCITY))
              (make-ball 484
                         599
                         -16
                         Y-VELOCITY))
(check-expect (flip-right (make-ball 385 599 50 Y-VELOCITY))
              (make-ball 335
                         599
                         -50
                         Y-VELOCITY))

(define (flip-right b)
  (make-ball (- (ball-x b) (ball-vx b))
             (ball-y b)
             (* -1 (ball-vx b))
             (ball-vy b)))

;moving without touching edge
(check-expect (simple-move (make-ball 20 15 X-VELOCITY Y-VELOCITY))
              (make-ball (+ 20 X-VELOCITY)
                         (+ 15 Y-VELOCITY)
                         X-VELOCITY
                         Y-VELOCITY))
(check-expect (simple-move (make-ball 50 400 X-VELOCITY Y-VELOCITY))
              (make-ball (+ 50 X-VELOCITY)
                         (+ 400 Y-VELOCITY)
                         X-VELOCITY
                         Y-VELOCITY))
(check-expect (simple-move (make-ball 500 60 X-VELOCITY Y-VELOCITY))
              (make-ball (+ 500 X-VELOCITY)
                         (+ 60 Y-VELOCITY)
                         X-VELOCITY
                         Y-VELOCITY))
(define (simple-move b)
  (make-ball (+ (ball-x b) (ball-vx b))
             (+ (ball-y b) (ball-vy b))
             (ball-vx b)
             (ball-vy b)))

;;Game->Image
;;paint the game onto the BG
;(check-expect (render-game G0)
;              (place-image PADDLE 0 (* HEIGHT 4/5) BG)
(check-expect (render-game G0)
              (place-image PADDLE 0 (* HEIGHT 4/5) BG))
(check-expect (render-game G1)
              (place-image PADDLE 1 (* HEIGHT 4/5)
                           (place-image BALL 10 20 BG)))
(check-expect (render-game G2)
              (place-image PADDLE 200 (* HEIGHT 4/5)
                           (place-image BALL 200 300
                                        (place-image BALL 10 20 BG))))
(define (render-game g)
  (render-paddle g (render-balls (game-lob g))))

;;Game->Image
;;paint the paddle onto the BG
(check-expect (render-paddle G0 BG)
              (place-image PADDLE 0 (* HEIGHT 4/5) BG))
(check-expect (render-paddle G1 BG)
              (place-image PADDLE 1 (* HEIGHT 4/5) BG))
(check-expect (render-paddle G2 BG)
              (place-image PADDLE 200 (* HEIGHT 4/5) BG))

(define (render-paddle g img)
  (place-image PADDLE (game-p g) (* HEIGHT 4/5) img))

;;ListOfBall->Image
;;paint the BALL onto the BG
(check-expect (render-balls LOB0) BG)
(check-expect (render-balls LOB1) (place-image BALL 10 20 BG))
(check-expect (render-balls LOB2) (place-image BALL 200 300
                                               (place-image BALL 10 20 BG)))
(define (render-balls lob)
  (cond [(empty? lob) BG]
        [else
         (render-ball (first lob)
                      (render-balls (rest lob)))]))

;;Ball->Image
;;paint the BALL onto the BG
(check-expect (render-ball B1 BG)
              (place-image BALL 10 20 BG))
(check-expect (render-ball B2 BG)
              (place-image BALL 200 300 BG))
(check-expect (render-ball (make-ball 40 20 5 5) BG)
              (place-image BALL 40 20 BG))

(define (render-ball b img)
  (place-image BALL (ball-x b) (ball-y b) img))

;;Game MouseEvent -> Game
;;check the action of the mouse made by the computer user,
;;if the action is "button-down", add a ball in mouse position,
;;otherwise remain the game
(check-expect (add-ball G0 1 1 "button-down")
              (make-game (cons (make-ball 1
                                          1
                                          (- 10 X-VELOCITY)
                                          (- 10 Y-VELOCITY))
                               empty)
                         0))
(check-expect (add-ball G1 1 1 "enter") G1)
(check-expect (add-ball G2 2 22 "button-up") G2)

(define (add-ball g x-coord y-coord me)
  (cond [(mouse=? me "button-down")
         (make-game (cons (make-ball x-coord
                                     y-coord
                                     (- 10 X-VELOCITY)
                                     (- 10 Y-VELOCITY))
                          (game-lob g))
                    (game-p g))]
        [else g]))

;;Game KeyEvent -> Game
;;check the action of the key made by the computer user,
;;if the action is "left", the paddle move to thr left (15p);
;;if the action is "right", the paddle move to thr right (15p)
;;otherwise the paddle remain the same position
(check-expect (handle-key G0 " ") G0)
(check-expect (handle-key G1 "left")
              (make-game (cons (make-ball 10 20 20 5) empty) -14))
(check-expect (handle-key G2 "right")
              (make-game (cons (make-ball 200 300 10 2)
                               (cons (make-ball 10 20 20 5) empty))
                         215))
(define (handle-key g ke)
  (cond [(key=? ke "left") (make-game (game-lob g)
                                      (- (game-p g) 15))]
        [(key=? ke "right") (make-game (game-lob g)
                                       (+ (game-p g) 15))]
        [else g]))

(main (make-game empty (/ WIDTH 2)))