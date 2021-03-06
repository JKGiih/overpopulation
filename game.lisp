;;;;
;;
;;   Overpopulation
;;
;;   Copyright 2016 Lasse Pouru
;;
;;   This program is free software: you can redistribute it and/or modify
;;   it under the terms of the GNU General Public License as published by
;;   the Free Software Foundation, either version 3 of the License, or
;;   (at your option) any later version.
;; 
;;   This program is distributed in the hope that it will be useful,
;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;   GNU General Public License for more details.
;;
;;   You should have received a copy of the GNU General Public License
;;   along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;

(defun randomrow (c)
  (let ((row ()))
    (dotimes (i c)
       (setf row (cons (random 2) row)))
    row))

(defun randomrows (r c)
  (let ((rows ()))
    (dotimes (i r)
       (setf rows (cons (randomrow c) rows)))
    rows))

(defun calculate-scale (c)
  (if (< (/ (gethash 'width c) (gethash 'height c)) (/ 4 3))
      (setf (gethash 'width c) (round (* (gethash 'height c) (/ 4 3)))))
  (setf (gethash 'scale c) (floor (/ (gethash 'height c) (gethash 'unscaled-height c))))
  (setf (gethash 'horizontal-offset c) (floor (/ (- (gethash 'width c) (* (gethash 'scale c) (gethash 'unscaled-width c))) 2)))
  (setf (gethash 'vertical-offset c) (floor (/ (- (gethash 'height c) (* (gethash 'scale c) (gethash 'unscaled-height c))) 2)))
  c)

(defun update-start (world)
  ;; Draw start screen
  (let ((width (gethash 'width world)) (height (gethash 'height world)) (horizontal-offset (gethash 'horizontal-offset world)) (vertical-offset (gethash 'vertical-offset world)))
    (sdl:draw-surface-at-* (gethash 'bg-sprite world) horizontal-offset vertical-offset)
    (sdl:draw-string-blended-* "Overpopulation" (round (+ horizontal-offset (/ width 6.0))) (round (+ vertical-offset (/ height 5.0))) :font *ebgaramond-ttf-large* :color (gethash 'player-color world))
    (sdl:draw-string-blended-* "Or: Divided and Disillusioned We Wander" (round (+ horizontal-offset (/ width 4.0))) (round (+ vertical-offset (* (/ height 5.0) 2))) :font *ebgaramond-ttf-small* :color (gethash 'player-color world))
    (sdl:draw-string-blended-* "in a World Gasping Under the Weight of Our Folly" (round (+ horizontal-offset (/ width 5.0))) (round (+ vertical-offset (/ height 2.0))) :font *ebgaramond-ttf-small* :color (gethash 'player-color world))
    (sdl:draw-string-blended-* "an observation" (round (+ horizontal-offset (* (/ width 9.0) 5))) (round (+ vertical-offset (* (/ height 7.0) 5))) :font *ebgaramond-ttf-small* :color (gethash 'player-color world))
    (sdl:draw-string-blended-* "by HorseSoft" (round (+ horizontal-offset (* (/ width 7.0) 4))) (round (+ vertical-offset (* (/ height 5.0) 4))) :font *ebgaramond-ttf-medium* :color (gethash 'player-color world)))
  world)

(defun draw-win (world)
  (draw-game world)
  (sdl:draw-string-blended-* "WIN GET!" (round (+ horizontal-offset (/ width 6.0))) (round (+ vertical-offset (/ height 5.0))) :font *ebgaramond-ttf-large* :color (gethash 'player-color world))
  t)

(defun draw-game (world)  
  (let ((player-sprite (gethash 'player-sprite world)) (npc-sprite (gethash 'npc-sprite world)) (horizontal-offset (gethash 'horizontal-offset world)) (vertical-offset (gethash 'vertical-offset world)) (scale (gethash 'scale world)))
    ;; Draw background
    (sdl:draw-surface-at-* (gethash 'bg-sprite world) horizontal-offset vertical-offset)  
    ;; Draw characters  
    (loop for i from 0 to (- (gethash 'unscaled-height world) 1) do
	 (loop for j from 0 to (- (gethash 'unscaled-width world) 1) when (eq (nth (+ j 1) (nth (+ i 1) (gethash 'npcs world))) 1) do
	      (sdl:draw-surface-at-* npc-sprite (round (+ horizontal-offset (* j scale))) (round (+ vertical-offset (* i scale))))))
    (sdl:draw-surface-at-* player-sprite (round (+ horizontal-offset (* (round (gethash 'player-x world)) scale))) (round (+ vertical-offset (* (round (gethash 'player-y world)) scale))))
    (sdl:draw-surface-at-* player-sprite (round (+ horizontal-offset (* (round (gethash 'player2-x world)) scale))) (round (+ vertical-offset (* (round (gethash 'player2-y world)) scale))))
    (sdl:draw-surface-at-* player-sprite (round (+ horizontal-offset (* (round (gethash 'player3-x world)) scale))) (round (+ vertical-offset (* (round (gethash 'player3-y world)) scale))))
    (sdl:draw-surface-at-* player-sprite (round (+ horizontal-offset (* (round (gethash 'player4-x world)) scale))) (round (+ vertical-offset (* (round (gethash 'player4-y world)) scale)))))
  ;; Debug string
  ;; (sdl:draw-string-shaded-* (write-to-string (sdl:sdl-get-ticks)) (/ (gethash 'width world) 2) (/ (gethash 'height world) 4) sdl:*red* sdl:*black*)
  t)

(defun update-npcs (world)
  ;; Update npcs once every second
  (let ((npcs (gethash 'npcs world)) (newrows ()) (newrow()))    
	   (setf newrows (cons (nth (- (list-length npcs) 1) npcs) newrows)) ;; last row doesn't change
	     (loop for i from (+ (gethash 'unscaled-height world) 0) downto 1 do
		  (setf newrow (cons (nth (- (list-length (nth i npcs)) 1) (nth i npcs)) newrow)) ;; last npc in row doesn't change
		  (loop for j from (+ (gethash 'unscaled-width world) 0) downto 1 do
		       (cond ((and (eq (nth (+ j 0) (nth (+ i 0) npcs)) 0) ;; rules for dead npcs
				   (eq (+ (nth (- j 1) (nth (- i 1) npcs))
					  (nth j (nth (- i 1) npcs))
					(nth (+ j 1) (nth (- i 1) npcs))
					(nth (- j 1) (nth i npcs))
					(nth (+ j 1) (nth i npcs))
					(nth (- j 1) (nth (+ i 1) npcs))
					(nth j (nth (+ i 1) npcs))
					(nth (+ j 1) (nth (+ i 1) npcs))) 3))
			    (setf newrow (cons 1 newrow))) ;; dead will three live neighbours becomes live
			   ((eq (nth (+ j 0) (nth (+ i 0) npcs)) 1) ;; rules for live npcs
			    (cond ((eq (+ (nth (- j 1) (nth (- i 1) npcs))
					  (nth j (nth (- i 1) npcs))
					  (nth (+ j 1) (nth (- i 1) npcs))
					  (nth (- j 1) (nth i npcs))
					  (nth (+ j 1) (nth i npcs))
					  (nth (- j 1) (nth (+ i 1) npcs))
					  (nth j (nth (+ i 1) npcs))
					  (nth (+ j 1) (nth (+ i 1) npcs))) 3)
				   (setf newrow (cons 1 newrow)))
				  ((eq (+ (nth (- j 1) (nth (- i 1) npcs))
					  (nth j (nth (- i 1) npcs))
					  (nth (+ j 1) (nth (- i 1) npcs))
					  (nth (- j 1) (nth i npcs))
					  (nth (+ j 1) (nth i npcs))
					  (nth (- j 1) (nth (+ i 1) npcs))
					  (nth j (nth (+ i 1) npcs))
					  (nth (+ j 1) (nth (+ i 1) npcs))) 2)
				   (setf newrow (cons 1 newrow)))
				  (t (setf newrow (cons 0 newrow)))))
			   (t (setf newrow (cons 0 newrow))))) ;; cond goes here
		(setf newrow (cons (nth 0 (nth i npcs)) newrow)) ;; first npc in row doesn't change
		(setf newrows (cons newrow newrows)))
	   (setf newrows (cons (nth 0 npcs) newrows)) ;; first row doesn't change
	   (setf (gethash 'npcs world) newrows)
	 )
  (gethash 'npcs world))

(defun move-player (world)
  ;; Move characters
  (let ((npcs (gethash 'npcs world)) (player-x (round (gethash 'player-x world))) (player-y (round (gethash 'player-y world))) (player2-x (round (gethash 'player2-x world))) (player2-y (round (gethash 'player2-y world))) (player3-x (round (gethash 'player3-x world))) (player3-y (round (gethash 'player3-y world))) (player4-x (round (gethash 'player4-x world))) (player4-y (round (gethash 'player4-y world))))
    (cond ((sdl:get-key-state :sdl-key-w)
	   (cond ((eq (nth (+ player-x 1) (nth (+ player-y 0) npcs)) 0)
		  (setf (gethash 'player-y world) (- (gethash 'player-y world) 0.5))))
	   (cond ((eq (nth (+ player2-x 1) (nth (+ player2-y 2) npcs)) 0)
		  (setf (gethash 'player2-y world) (+ (gethash 'player2-y world) 0.5))))
	   (cond ((eq (nth (+ player3-x 1) (nth (+ player3-y 0) npcs)) 0)
		  (setf (gethash 'player3-y world) (- (gethash 'player3-y world) 0.5))))
	   (cond ((eq (nth (+ player4-x 1) (nth (+ player4-y 2) npcs)) 0)
		  (setf (gethash 'player4-y world) (+ (gethash 'player4-y world) 0.5))))))
    (cond ((sdl:get-key-state :sdl-key-s)
	   (cond ((eq (nth (+ player-x 1) (nth (+ player-y 2) npcs)) 0)
		  (setf (gethash 'player-y world) (+ (gethash 'player-y world) 0.5))))
	   (cond ((eq (nth (+ player2-x 1) (nth (+ player2-y 0) npcs)) 0)
		  (setf (gethash 'player2-y world) (- (gethash 'player2-y world) 0.5))))
	   (cond ((eq (nth (+ player3-x 1) (nth (+ player3-y 2) npcs)) 0)
		  (setf (gethash 'player3-y world) (+ (gethash 'player3-y world) 0.5))))
	   (cond ((eq (nth (+ player4-x 1) (nth (+ player4-y 0) npcs)) 0)
		  (setf (gethash 'player4-y world) (- (gethash 'player4-y world) 0.5))))))
    (cond ((sdl:get-key-state :sdl-key-a)
	   (cond ((eq (nth (+ player-x 0) (nth (+ player-y 1) npcs)) 0)
		  (setf (gethash 'player-x world) (- (gethash 'player-x world) 0.5))))
	   (cond ((eq (nth (+ player2-x 0) (nth (+ player2-y 1) npcs)) 0)
		  (setf (gethash 'player2-x world) (- (gethash 'player2-x world) 0.5))))
	   (cond ((eq (nth (+ player3-x 2) (nth (+ player3-y 1) npcs)) 0)
		  (setf (gethash 'player3-x world) (+ (gethash 'player3-x world) 0.5))))
	   (cond ((eq (nth (+ player4-x 2) (nth (+ player4-y 1) npcs)) 0)
		  (setf (gethash 'player4-x world) (+ (gethash 'player4-x world) 0.5))))))
    (cond ((sdl:get-key-state :sdl-key-d)
	   (cond ((eq (nth (+ player-x 2) (nth (+ player-y 1) npcs)) 0)
		  (setf (gethash 'player-x world) (+ (gethash 'player-x world) 0.5))))
	   (cond ((eq (nth (+ player2-x 2) (nth (+ player2-y 1) npcs)) 0)
		  (setf (gethash 'player2-x world) (+ (gethash 'player2-x world) 0.5))))
	   (cond ((eq (nth (+ player3-x 0) (nth (+ player3-y 1) npcs)) 0)
		  (setf (gethash 'player3-x world) (- (gethash 'player3-x world) 0.5))))
	   (cond ((eq (nth (+ player4-x 0) (nth (+ player4-y 1) npcs)) 0)
		  (setf (gethash 'player4-x world) (- (gethash 'player4-x world) 0.5)))))))
  ;; Keep characters within screen
  (let ((player-x (gethash 'player-x world)) (player-y (gethash 'player-y world)) (player2-x (gethash 'player2-x world)) (player2-y (gethash 'player2-y world)) (player3-x (gethash 'player3-x world)) (player3-y (gethash 'player3-y world)) (player4-x (gethash 'player4-x world)) (player4-y (gethash 'player4-y world)) (width (gethash 'unscaled-width world)) (height (gethash 'unscaled-height world)))
    (if (> player-y (- height 1))
	(setf (gethash 'player-y world) (- height 1)))
    (if (< (gethash 'player-y world) 0)
        (setf (gethash 'player-y world) 0))
    (if (> player-x (- width 1))
        (setf (gethash 'player-x world) (- width 1)))
    (if (< player-x 0)
        (setf (gethash 'player-x world) 0))
    (if (> player2-y (- height 1))
        (setf (gethash 'player2-y world) (- height 1)))
    (if (< player2-y 0)
        (setf (gethash 'player2-y world) 0))
    (if (> player2-x (- width 1))
        (setf (gethash 'player2-x world) (- width 1)))
    (if (< player2-x 0)
        (setf (gethash 'player2-x world) 0))
    (if (> player3-y (- height 1))
        (setf (gethash 'player3-y world) (- height 1)))
    (if (< player3-y 0)
        (setf (gethash 'player3-y world) 0))
    (if (> player3-x (- width 1))
        (setf (gethash 'player3-x world) (- width 1)))
    (if (< player3-x 0)
        (setf (gethash 'player3-x world) 0))
    (if (> player4-y (- height 1))
        (setf (gethash 'player4-y world) (- height 1)))
    (if (< player4-y 0)
        (setf (gethash 'player4-y world) 0))
    (if (> player4-x (- width 1))
        (setf (gethash 'player4-x world) (- width 1)))
    (if (< player4-x 0)
        (setf (gethash 'player4-x world) 0)))
  ;; Set character locations to 2
  (let ((npcs (gethash 'npcs world)))
    (setf (nth (+ (round (gethash 'player-x world)) 1) (nth (+ (round (gethash 'player-y world)) 1) npcs)) 2)
    (setf (nth (+ (round (gethash 'player2-x world)) 1) (nth (+ (round (gethash 'player2-y world)) 1) npcs)) 2)
    (setf (nth (+ (round (gethash 'player3-x world)) 1) (nth (+ (round (gethash 'player3-y world)) 1) npcs)) 2)
    (setf (nth (+ (round (gethash 'player4-x world)) 1) (nth (+ (round (gethash 'player4-y world)) 1) npcs)) 2))
  world)
  
(defun check-win (world)
  (let ((player-x (gethash 'player-x world)) (player-y (gethash 'player-y world)) (player2-x (gethash 'player2-x world)) (player2-y (gethash 'player2-y world)) (player3-x (gethash 'player3-x world)) (player3-y (gethash 'player3-y world)) (player4-x (gethash 'player4-x world)) (player4-y (gethash 'player4-y world)) (width (gethash 'unscaled-width world)) (height (gethash 'unscaled-height world)))
    (cond ((<= (+ (abs (- player-x player2-x)) (abs (- player-x player3-x)) (abs (- player-x player4-x)) (abs (- player2-x player-x)) (abs (- player2-x player3-x)) (abs (- player2-x player4-x)) (abs (- player3-x player-x)) (abs (- player3-x player2-x)) (abs (- player3-x player4-x)) (abs (- player4-x player-x)) (abs (- player4-x player2-x)) (abs (- player4-x player3-x)) (abs (- player-y player2-y)) (abs (- player-y player3-y)) (abs (- player-y player4-y)) (abs (- player2-y player-y)) (abs (- player2-y player3-y)) (abs (- player2-y player4-y)) (abs (- player3-y player-y)) (abs (- player3-y player2-y)) (abs (- player3-y player4-y)) (abs (- player4-y player-y)) (abs (- player4-y player2-y)) (abs (- player4-y player3-y))) 20)
	   (setf (gethash 'state world) "win"))))
  (gethash 'state world))

(defun update-game (world)
  (cond ((> (- (sdl:sdl-get-ticks) (gethash 'last-update world)) 1000)
	 ;; Update npcs
         (setf (gethash 'npcs world) (update-npcs world))
         ;; Check if game is won
         (setf (gethash 'state world) (check-win world))
         ;; Update last-update
         (setf (gethash 'last-update world) (sdl:sdl-get-ticks))))
  ;; Move player
  (setf world (move-player world))
  world)

(defun initialize-game (world)
  (setf (gethash 'player-x world) (random (gethash 'unscaled-width world)))
  (setf (gethash 'player-y world) (random (gethash 'unscaled-height world)))
  (setf (gethash 'player2-x world) (random (gethash 'unscaled-width world)))
  (setf (gethash 'player2-y world) (random (gethash 'unscaled-height world)))
  (setf (gethash 'player3-x world) (random (gethash 'unscaled-width world)))
  (setf (gethash 'player3-y world) (random (gethash 'unscaled-height world)))
  (setf (gethash 'player4-x world) (random (gethash 'unscaled-width world)))
  (setf (gethash 'player4-y world) (random (gethash 'unscaled-height world)))
  (setf (gethash 'npcs world) (randomrows (+ (gethash 'unscaled-height world) 2) (+ (gethash 'unscaled-width world) 2)))    
  world)

(defun play-game ()
  (let ((world (make-hash-table)))
    ;; Set default config
    (setf (gethash 'state world) "start")
    (setf (gethash 'width world) 800)
    (setf (gethash 'height world) 600)
    (setf (gethash 'unscaled-width world) 80)
    (setf (gethash 'unscaled-height world) 60)
    (setf (gethash 'horizontal-offset world) 0)
    (setf (gethash 'vertical-offset world) 0)
    (setf (gethash 'scale world) 10)
    (setf (gethash 'fullscreen world) NIL)
    (setf (gethash 'npc-color world) sdl:*black*)
    (setf (gethash 'bg-color world) (sdl:color :r 4 :g 27 :b 0))
    (setf (gethash 'player-color world) (sdl:color :r 224 :g 255 :b 192))
    (setf (gethash 'last-update world) 0)
    (setf (gethash 'player-sprite world) NIL)
    (setf (gethash 'npc-sprite world) NIL)
    (setf (gethash 'bg-sprite world) NIL)
    (when (probe-file "sounds/sf.ogg")
      (setf (gethash 'sound-on-p world) t)
      (setf (gethash 'sound-volume world) 96)
      (setf (gethash 'sound-effect-1 world) NIL))
    (when (probe-file "sounds/bgm.ogg")
      (setf (gethash 'music-on-p world) t)
      (setf (gethash 'music-volume world) 96)
      (setf (gethash 'music world) NIL))
    ;; Read config file, change default config
    (when (probe-file "config.lisp")
      (load "config.lisp")
      (setf world (change-config world)))
    ;; Set sound off if files missing
    (when (not (probe-file "sounds/sf.ogg"))
      (setf (gethash 'sound-on-p world) NIL))
    (when (not (probe-file "sounds/bgm.ogg"))
      (setf (gethash 'music-on-p world) NIL))
    ;; Set fields that depend on config
    (setf world (calculate-scale world))
    (setq world (initialize-game world))    
    (sdl:with-init ()
      ;; Create window
      (sdl:window (gethash 'width world) (gethash 'height world) :title-caption "Overpopulation" :fullscreen (gethash 'fullscreen world))
      (setf (sdl:frame-rate) 30)
      ;; Create sprites
      (setf (gethash 'player-sprite world) (sdl:create-surface (gethash 'scale world) (gethash 'scale world)))
      (sdl:draw-box (sdl:rectangle-from-edges-* 0 0 (gethash 'scale world) (gethash 'scale world))
		    :color (gethash 'player-color world) :surface (gethash 'player-sprite world))
      (setf (gethash 'npc-sprite world) (sdl:create-surface (gethash 'scale world) (gethash 'scale world)))
      (sdl:draw-box (sdl:rectangle-from-edges-* 0 0 (gethash 'scale world) (gethash 'scale world))
		    :color (gethash 'npc-color world) :surface (gethash 'npc-sprite world))
      (setf (gethash 'bg-sprite world) (sdl:create-surface (* (gethash 'scale world) (gethash 'unscaled-width world)) (* (gethash 'scale world) (gethash 'unscaled-height world))))
      (sdl:draw-box (sdl:rectangle-from-edges-* 0 0 (gethash 'width world) (gethash 'height world))
		    :color (gethash 'bg-color world) :surface (gethash 'bg-sprite world))      
      ;; Initialize fonts
      (defparameter *ebgaramond-ttf-small* (make-instance 'SDL:ttf-font-definition :size (round (* (/ (gethash 'height world) (gethash 'unscaled-height world)) 2)) :filename "EBGaramond12-Regular.ttf"))
      (defparameter *ebgaramond-ttf-medium* (make-instance 'SDL:ttf-font-definition :size (round (* (/ (gethash 'height world) (gethash 'unscaled-height world)) 4)) :filename "EBGaramond12-Regular.ttf"))
      (defparameter *ebgaramond-ttf-large* (make-instance 'SDL:ttf-font-definition :size (round (* (/ (gethash 'height world) (gethash 'unscaled-height world)) 8)) :filename "EBGaramond12-Regular.ttf"))
      (unless (setf *ebgaramond-ttf-small* (sdl:initialise-font *ebgaramond-ttf-small*))
	(error "Cannot initialize font: EBGaramond12-Regular.ttf"))
      (unless (setf *ebgaramond-ttf-medium* (sdl:initialise-default-font *ebgaramond-ttf-medium*))
	(error "Cannot initialize font: EBGaramond12-Regular.ttf"))
      (unless (setf *ebgaramond-ttf-large* (sdl:initialise-font *ebgaramond-ttf-large*))
	(error "Cannot initialize font: EBGaramond12-Regular.ttf"))
      ;; Load sounds and music and play music
      (when (or (gethash 'music-on-p world) (gethash 'sound-on-p world))
	(sdl-mixer:OPEN-AUDIO)
	(when (gethash 'music-on-p world)
	  (setf (sdl-mixer:music-volume) (gethash 'music-volume world))
	  (setf (gethash 'music world) (sdl-mixer:load-music "sounds/bgm.ogg"))
	  (sdl-mixer:play-music (gethash 'music world) :loop t))
	(when (gethash 'sound-on-p world)
	  (sdl-mixer:reserve-channels 1) ; Reserve channel 0 for looping sound effect
	  (setf (gethash 'sound-effect-1 world) (sdl-mixer:load-sample "sounds/sf.ogg"))
	  (setf (sdl-mixer:sample-volume (gethash 'sound-effect-1 world)) (gethash 'sound-volume world))))	  
      (sdl:with-events (:poll)
	(:quit-event ()
		     (when (or (gethash 'music-on-p world) (gethash 'sound-on-p world))
		       (when (gethash 'music-on-p world)
			 (sdl-mixer:Halt-Music)
			 (sdl-mixer:free (gethash 'music world)))
		       (when (gethash 'sound-on-p world)
			 (sdl-mixer:free (gethash 'sound-effect-1 world)))
		       (sdl-mixer:CLOSE-AUDIO t))
		     t)
	(:key-down-event (:key key)		
			 (when (sdl:key= key :sdl-key-escape)
			   (cond
			     ((equal (gethash 'state world) "start")
			      (sdl:push-quit-event))
			     ((or (equal (gethash 'state world) "game") (equal (gethash 'state world) "win"))
			      (setf (gethash 'state world) "start"))))
			 (when (sdl:key= key :sdl-key-return)
			   (cond
			     ((or (equal (gethash 'state world) "start") (equal (gethash 'state world) "win"))
			      (setq world (initialize-game world))
    			      (setf (gethash 'state world) "game"))))
			 (when (or (sdl:key= key :sdl-key-w) (sdl:key= key :sdl-key-s) (sdl:key= key :sdl-key-a) (sdl:key= key :sdl-key-d))
			   (if (and (gethash 'sound-on-p world) (not (sdl-mixer:sample-playing-p 0)) (equal (gethash 'state world) "game"))
			       (sdl-mixer:play-sample (gethash 'sound-effect-1 world) :channel 0 :loop t))))
	(:key-up-event (:key key)
		       (when (or (sdl:key= key :sdl-key-w) (sdl:key= key :sdl-key-s) (sdl:key= key :sdl-key-a) (sdl:key= key :sdl-key-d))
			 (if (and (gethash 'sound-on-p world) (sdl-mixer:sample-playing-p 0) (not (or (sdl:get-key-state :sdl-key-w) (sdl:get-key-state :sdl-key-s) (sdl:get-key-state :sdl-key-a) (sdl:get-key-state :sdl-key-d))))
			     (sdl-mixer:halt-sample :channel 0 :fade 5))))
	(:idle ()	       
	       ;; Clear screen
	       (sdl:clear-display sdl:*black*)
	       ;; Update and draw the state we're in
	       (cond
		 ((equal (gethash 'state world) "start") 
		  (setq world (update-start world)))
		 ((equal (gethash 'state world) "game") 
		  (setq world (update-game world))
		  (draw-game world))
		 ((equal (gethash 'state world) "win")
		  (draw-win world)))
	       ;; Redraw screen
	       (sdl:update-display))))))
