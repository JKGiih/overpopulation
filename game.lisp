(defun randomrow ()
  (let ((row ()))
    (dotimes (i 64)
       (setf row (cons (random 2) row)))
    row))

(defun randomrows ()
  (let ((rows ()))
    (dotimes (i 48)
       (setf rows (cons (randomrow) rows)))
    rows))

(defun calculate-scale (c)
  (if (< (/ (gethash 'width c) (gethash 'height c)) (/ 4 3))
      (setf (gethash 'width c) (round (* (gethash 'height c) (/ 4 3)))))
  (if (> (/ (gethash 'width c) (gethash 'height c)) (/ 4 3))
      (setf (gethash 'widescreen-offset c) (round (/ (- (gethash 'width c) (* (gethash 'height c) (/ 4 3))) 2))))
  (setf (gethash 'scale c) (/ (gethash 'height c) (gethash 'unscaled-height c)))
  c)

(defun update-start (world)

  ;; Begin game when left key is pressed
  (when (sdl:mouse-left-p)
    (setf (gethash 'state world) "game"))

  ;; Draw start screen
  (sdl:draw-box (sdl:rectangle-from-midpoint-* (/ (gethash 'width world) 2) (/ (gethash 'height world) 2) (- (gethash 'width world) (/ (gethash 'width world) 10)) (- (gethash 'height world) (/ (gethash 'height world) 2)))
		:color (gethash 'color world))
  (sdl:draw-string-shaded-* "Click mouse to start" (/ (gethash 'width world) 2) (/ (gethash 'height world) 2) sdl:*red* sdl:*black*)
  world)

(defun update-game (world)

  ;; Return to start screen when right key is pressed
  (when (sdl:mouse-right-p)
    (setf (gethash 'state world) "start"))

  ;; Change the color of the box when left button is down
  (when (sdl:mouse-left-p)
    (setf (gethash 'color world) (sdl:color :r (random 255) :g (random 255) :b (random 255))))
  
  ;; Move player
  (if (sdl:get-key-state :sdl-key-w)
      (setf (gethash 'player-y world) (- (gethash 'player-y world) 0.2)))
  (if (sdl:get-key-state :sdl-key-s)
      (setf (gethash 'player-y world) (+ (gethash 'player-y world) 0.2)))
  (if (sdl:get-key-state :sdl-key-a)
      (setf (gethash 'player-x world) (- (gethash 'player-x world) 0.2)))
  (if (sdl:get-key-state :sdl-key-d)
      (setf (gethash 'player-x world) (+ (gethash 'player-x world) 0.2)))

  ;; Keep player within screen
  (if (> (gethash 'player-y world) (- (gethash 'unscaled-height world) 4))
      (setf (gethash 'player-y world) (- (gethash 'unscaled-height world) 4)))
  (if (< (gethash 'player-y world) 0)
      (setf (gethash 'player-y world) 0))
  (if (> (gethash 'player-x world) (- (gethash 'unscaled-width world) 2))
      (setf (gethash 'player-x world) (- (gethash 'unscaled-width world) 2)))
  (if (< (gethash 'player-x world) 0)
      (setf (gethash 'player-x world) 0))

  ;; Draw green background
  ;; (sdl:draw-box (sdl:rectangle-from-edges-* (gethash 'widescreen-offset world) 0 (+ (gethash 'widescreen-offset world) (* (gethash 'scale world) (gethash 'unscaled-width world))) (* (gethash 'scale world) (gethash 'unscaled-height world))) :color sdl:*green*)

  ;; Draw mouse controlled box
  (sdl:draw-box (sdl:rectangle-from-midpoint-* (sdl:mouse-x) (sdl:mouse-y) 20 20)
		:color (gethash 'color world))

  ;; Draw cells
  (loop for i from 0 to (gethash 'unscaled-height world) do
  (loop for j from 0 to (gethash 'unscaled-width world) when (eq (nth j (nth i (gethash 'cells world))) 1) do
       (sdl:draw-box (sdl:rectangle-from-edges-* (+ (gethash 'widescreen-offset world) (* (gethash 'scale world) j)) (* (gethash 'scale world) i) (+ (gethash 'widescreen-offset world) (* (gethash 'scale world) (+ j 1))) (* (gethash 'scale world) (+ i 1))) :color (gethash 'color world))))

  ;; Scale and draw player surface on screen
  (sdl:draw-surface-at-* (gethash 'player-sprite world) (round (+ (gethash 'widescreen-offset world) (* (round (gethash 'player-x world)) (gethash 'scale world)))) (round (* (round (gethash 'player-y world)) (gethash 'scale world))))

  ;; Debug string
  (sdl:draw-string-shaded-* (write-to-string (sdl-mixer:music-playing-p)) (/ (gethash 'width world) 2) (/ (gethash 'height world) 2) sdl:*red* sdl:*black*)

  world)

(defun play-game ()
  (let ((world (make-hash-table)))
    ;; Set default config
    (setf (gethash 'state world) "start")
    (setf (gethash 'width world) 800)
    (setf (gethash 'height world) 600)
    (setf (gethash 'unscaled-width world) 64)
    (setf (gethash 'unscaled-height world) 48)
    (setf (gethash 'widescreen-offset world) 0)
    (setf (gethash 'scale world) 10)
    (setf (gethash 'fullscreen world) NIL)
    (setf (gethash 'color world) sdl:*white*)
    (when (and (probe-file "sounds/sf1.ogg") (probe-file "sounds/sf2.ogg"))
      (setf (gethash 'sound-on-p world) t)
      (setf (gethash 'sound-volume world) 96)
      (setf (gethash 'sound-effect-1 world) NIL)
      (setf (gethash 'sound-effect-2 world) NIL))
    (when (probe-file "sounds/bgm.ogg")
      (setf (gethash 'music-on-p world) t)
      (setf (gethash 'music-volume world) 96)
      (setf (gethash 'music world) NIL))

    (setf (gethash 'cells world) (randomrows)) ;; Cells for game of life
    
    ;; Read config file, change default config
    (when (probe-file "config.lisp")
      (load "config.lisp")
      (setf world (change-config world)))

    ;; Set sound off if files missing
    (when (not (and (probe-file "sounds/sf1.ogg") (probe-file "sounds/sf2.ogg")))
      (setf (gethash 'sound-on-p world) NIL))
    (when (not (probe-file "sounds/bgm.ogg"))
      (setf (gethash 'music-on-p world) NIL))

    ;; Set fields that depend on config
    (setf world (calculate-scale world))
    (setf (gethash 'player-x world) (/ (gethash 'unscaled-width world) 2))
    (setf (gethash 'player-y world) (/ (gethash 'unscaled-height world) 2))
    (setf (gethash 'player-sprite world) NIL)

    (sdl:with-init ()

      ;; Create window
      (sdl:window (gethash 'width world) (gethash 'height world) :title-caption "Window title goes here" :fullscreen (gethash 'fullscreen world))
      (setf (sdl:frame-rate) 60)

      ;; Create sprite
      (setf (gethash 'player-sprite world) (sdl:create-surface (* 2 (gethash 'scale world)) (* 4 (gethash 'scale world))))
      (sdl:draw-box (sdl:rectangle-from-edges-* 0 0 (* 2 (gethash 'scale world)) (* 4 (gethash 'scale world)))
		    :color sdl:*blue* :surface (gethash 'player-sprite world))
      
      ;; Initialize fonts
      (unless (sdl:initialise-default-font sdl:*ttf-font-vera*)
	(error "Cannot initialize the default font."))

      ;; Load sounds and music and play music
      (when (or (gethash 'music-on-p world) (gethash 'sound-on-p world))
	(sdl-mixer:OPEN-AUDIO)
	(when (gethash 'music-on-p world)
	  (setf (sdl-mixer:music-volume) (gethash 'music-volume world))
	  (setf (gethash 'music world) (sdl-mixer:load-music "sounds/bgm.ogg"))
	  (sdl-mixer:play-music (gethash 'music world) :loop t))
	(when (gethash 'sound-on-p world)
	  (sdl-mixer:reserve-channels 1) ; Reserve channel 0 for looping sound effect
	  (setf (gethash 'sound-effect-1 world) (sdl-mixer:load-sample "sounds/sf1.ogg"))
	  (setf (gethash 'sound-effect-2 world) (sdl-mixer:load-sample "sounds/sf2.ogg"))
	  (setf (sdl-mixer:sample-volume (gethash 'sound-effect-1 world)) (gethash 'sound-volume world))
	  (setf (sdl-mixer:sample-volume (gethash 'sound-effect-2 world)) (gethash 'sound-volume world))))
	  
      (sdl:with-events ()

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
			     ((equal (gethash 'state world) "game")
			      (setf (gethash 'state world) "start"))))
			 (when (sdl:key= key :sdl-key-return)
			   (cond
			     ((equal (gethash 'state world) "start")
			      (setf (gethash 'state world) "game"))))
			 (when (or (sdl:key= key :sdl-key-w) (sdl:key= key :sdl-key-s) (sdl:key= key :sdl-key-a) (sdl:key= key :sdl-key-d))
			   (if (and (gethash 'sound-on-p world) (not (sdl-mixer:sample-playing-p 0)))
			       (sdl-mixer:play-sample (gethash 'sound-effect-1 world) :channel 0 :loop t))))

	(:key-up-event (:key key)
		       (when (or (sdl:key= key :sdl-key-w) (sdl:key= key :sdl-key-s) (sdl:key= key :sdl-key-a) (sdl:key= key :sdl-key-d))
			 (if (and (gethash 'sound-on-p world) (sdl-mixer:sample-playing-p 0) (not (or (sdl:get-key-state :sdl-key-w) (sdl:get-key-state :sdl-key-s) (sdl:get-key-state :sdl-key-a) (sdl:get-key-state :sdl-key-d))))
			     (sdl-mixer:halt-sample :channel 0 :fade 5))))		       
	 
	(:idle ()
	       
	       ;; Clear screen every frame
	       (sdl:clear-display sdl:*black*)

	       ;; Update and draw the state we're in
	       (cond
		 ((equal (gethash 'state world) "start") 
		  (setq world (update-start world)))
		 ((equal (gethash 'state world) "game")
		  (setq world (update-game world))))

	       ;; Redraw screen every frame
	       (sdl:update-display))))))
