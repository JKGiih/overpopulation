(defun randomrow ()
  (let ((row ()))
    (dotimes (i 66) ;; two columns outside screen
       (setf row (cons (random 2) row)))
    row))

(defun randomrows ()
  (let ((rows ()))
    (dotimes (i 50) ;; two rows outside screen
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
    (setf (gethash 'player-x world) (random (gethash 'unscaled-width world)))
    (setf (gethash 'player-y world) (random (gethash 'unscaled-height world)))
    (setf (gethash 'cells world) (randomrows))
    (setf (gethash 'state world) "game"))

  ;; Draw start screen
  (sdl:draw-box (sdl:rectangle-from-midpoint-* (/ (gethash 'width world) 2) (/ (gethash 'height world) 2) (- (gethash 'width world) (/ (gethash 'width world) 10)) (- (gethash 'height world) (/ (gethash 'height world) 2)))
		:color (gethash 'color world))
  (sdl:draw-string-shaded-* "Click mouse to start" (/ (gethash 'width world) 2) (/ (gethash 'height world) 2) sdl:*red* sdl:*black*)
  world)

(defun update-game (world)

  ;; Update frame
  (setf (gethash 'frame world) (mod (+ 1 (gethash 'frame world)) 60))

  ;; Return to start screen when right key is pressed
  (when (sdl:mouse-right-p)
    (setf (gethash 'state world) "start"))

  ;; Update cells once every second
  (cond ((eq (gethash 'frame world) 0)
	 (let ((newrows ())
	       (newrow()))
	   (setf newrows (cons (nth 49 (gethash 'cells world)) newrows)) ;; last row doesn't change
	   (loop for i from (+ (gethash 'unscaled-height world) 0) downto 1 do
		(setf newrow (cons (nth 65 (nth i (gethash 'cells world))) newrow)) ;; last cell in row doesn't change
		(loop for j from (+ (gethash 'unscaled-width world) 0) downto 1 do
		     (cond ((and (eq (nth (+ j 0) (nth (+ i 0) (gethash 'cells world))) 0) ;; rules for dead cells
				 (eq (+ (nth (- j 1) (nth (- i 1) (gethash 'cells world)))
					(nth j (nth (- i 1) (gethash 'cells world)))
					(nth (+ j 1) (nth (- i 1) (gethash 'cells world)))
					(nth (- j 1) (nth i (gethash 'cells world)))
					(nth (+ j 1) (nth i (gethash 'cells world)))
					(nth (- j 1) (nth (+ i 1) (gethash 'cells world)))
					(nth j (nth (+ i 1) (gethash 'cells world)))
					(nth (+ j 1) (nth (+ i 1) (gethash 'cells world)))) 3))
			    (setf newrow (cons 1 newrow))) ;; dead will three live neighbours becomes live
			   ((eq (nth (+ j 0) (nth (+ i 0) (gethash 'cells world))) 1) ;; rules for live cells
			    (cond ((eq (+ (nth (- j 1) (nth (- i 1) (gethash 'cells world)))
					  (nth j (nth (- i 1) (gethash 'cells world)))
					  (nth (+ j 1) (nth (- i 1) (gethash 'cells world)))
					  (nth (- j 1) (nth i (gethash 'cells world)))
					  (nth (+ j 1) (nth i (gethash 'cells world)))
					  (nth (- j 1) (nth (+ i 1) (gethash 'cells world)))
					  (nth j (nth (+ i 1) (gethash 'cells world)))
					  (nth (+ j 1) (nth (+ i 1) (gethash 'cells world)))) 3)
				   (setf newrow (cons 1 newrow)))
				  ((eq (+ (nth (- j 1) (nth (- i 1) (gethash 'cells world)))
					  (nth j (nth (- i 1) (gethash 'cells world)))
					  (nth (+ j 1) (nth (- i 1) (gethash 'cells world)))
					  (nth (- j 1) (nth i (gethash 'cells world)))
					  (nth (+ j 1) (nth i (gethash 'cells world)))
					  (nth (- j 1) (nth (+ i 1) (gethash 'cells world)))
					  (nth j (nth (+ i 1) (gethash 'cells world)))
					  (nth (+ j 1) (nth (+ i 1) (gethash 'cells world)))) 2)
				   (setf newrow (cons 1 newrow)))
				  (t (setf newrow (cons 0 newrow)))))
			   (t (setf newrow (cons 0 newrow))))) ;; cond goes here
		(setf newrow (cons (nth 0 (nth i (gethash 'cells world))) newrow)) ;; first cell in row doesn't change
		(setf newrows (cons newrow newrows)))
	   (setf newrows (cons (nth 0 (gethash 'cells world)) newrows))
	   (setf (gethash 'cells world) newrows)) ;; first row doesn't change
	 )
	(t NIL))
        
  ;; Change the color of the box when left button is down
  (when (sdl:mouse-left-p)
    (setf (gethash 'color world) (sdl:color :r (random 255) :g (random 255) :b (random 255))))
  
  ;; Move player
  (if (and (sdl:get-key-state :sdl-key-w) (eq (nth (+ (round (gethash 'player-x world)) 1) (nth (+ (round (gethash 'player-y world)) 0) (gethash 'cells world))) 0))
      (setf (gethash 'player-y world) (- (gethash 'player-y world) 0.2)))
  (if (and (sdl:get-key-state :sdl-key-s) (eq (nth (+ (round (gethash 'player-x world)) 1) (nth (+ (round (gethash 'player-y world)) 2) (gethash 'cells world))) 0))
      (setf (gethash 'player-y world) (+ (gethash 'player-y world) 0.2)))
  (if (and (sdl:get-key-state :sdl-key-a) (eq (nth (+ (round (gethash 'player-x world)) 0) (nth (+ (round (gethash 'player-y world)) 1) (gethash 'cells world))) 0))
      (setf (gethash 'player-x world) (- (gethash 'player-x world) 0.2)))
  (if (and (sdl:get-key-state :sdl-key-d) (eq (nth (+ (round (gethash 'player-x world)) 2) (nth (+ (round (gethash 'player-y world)) 1) (gethash 'cells world))) 0))
      (setf (gethash 'player-x world) (+ (gethash 'player-x world) 0.2)))

  ;; Keep player within screen
  (if (> (gethash 'player-y world) (- (gethash 'unscaled-height world) 1))
      (setf (gethash 'player-y world) (- (gethash 'unscaled-height world) 1)))
  (if (< (gethash 'player-y world) 0)
      (setf (gethash 'player-y world) 0))
  (if (> (gethash 'player-x world) (- (gethash 'unscaled-width world) 1))
      (setf (gethash 'player-x world) (- (gethash 'unscaled-width world) 1)))
  (if (< (gethash 'player-x world) 0)
      (setf (gethash 'player-x world) 0))

  ;; Draw green background
  (sdl:draw-box (sdl:rectangle-from-edges-* (gethash 'widescreen-offset world) 0 (+ (gethash 'widescreen-offset world) (* (gethash 'scale world) (gethash 'unscaled-width world))) (* (gethash 'scale world) (gethash 'unscaled-height world))) :color sdl:*green*)

  ;; Draw mouse controlled box
  ;; (sdl:draw-box (sdl:rectangle-from-midpoint-* (sdl:mouse-x) (sdl:mouse-y) 20 20)
  ;;		:color (gethash 'color world))

  ;; Draw cells
  (loop for i from 0 to (- (gethash 'unscaled-height world) 1) do
       (loop for j from 0 to (- (gethash 'unscaled-width world) 1) when (eq (nth (+ j 1) (nth (+ i 1) (gethash 'cells world))) 1) do
	    (sdl:draw-box (sdl:rectangle-from-edges-* (+ (gethash 'widescreen-offset world) (* (gethash 'scale world) j)) (* (gethash 'scale world) i) (+ (gethash 'widescreen-offset world) (* (gethash 'scale world) (+ j 1))) (* (gethash 'scale world) (+ i 1))) :color (gethash 'color world))))

  ;; Scale and draw player surface on screen
  (sdl:draw-surface-at-* (gethash 'player-sprite world) (round (+ (gethash 'widescreen-offset world) (* (round (gethash 'player-x world)) (gethash 'scale world)))) (round (* (round (gethash 'player-y world)) (gethash 'scale world))))

  ;; Debug string
  (sdl:draw-string-shaded-* (write-to-string (nth (+ (round (gethash 'player-x world)) 0) (nth (+ (round (gethash 'player-y world)) 0) (gethash 'cells world)))) (/ (gethash 'width world) 2) (/ (gethash 'height world) 5) sdl:*red* sdl:*black*)
  
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
    (setf (gethash 'frame world) 0)
    (when (and (probe-file "sounds/sf1.ogg") (probe-file "sounds/sf2.ogg"))
      (setf (gethash 'sound-on-p world) t)
      (setf (gethash 'sound-volume world) 96)
      (setf (gethash 'sound-effect-1 world) NIL)
      (setf (gethash 'sound-effect-2 world) NIL))
    (when (probe-file "sounds/bgm.ogg")
      (setf (gethash 'music-on-p world) t)
      (setf (gethash 'music-volume world) 96)
      (setf (gethash 'music world) NIL))

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
    (setf (gethash 'player-x world) (random (gethash 'unscaled-width world)))
    (setf (gethash 'player-y world) (random (gethash 'unscaled-height world)))
    (setf (gethash 'player-sprite world) NIL)

    (setf (gethash 'cells world) (randomrows)) ;; Cells for game of life
    
    (sdl:with-init ()

      ;; Create window
      (sdl:window (gethash 'width world) (gethash 'height world) :title-caption "Window title goes here" :fullscreen (gethash 'fullscreen world))
      (setf (sdl:frame-rate) 60)

      ;; Create sprite
      (setf (gethash 'player-sprite world) (sdl:create-surface (gethash 'scale world) (gethash 'scale world)))
      (sdl:draw-box (sdl:rectangle-from-edges-* 0 0 (gethash 'scale world) (gethash 'scale world))
		    :color sdl:*red* :surface (gethash 'player-sprite world))
      
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
			      (setf (gethash 'player-x world) (random (gethash 'unscaled-width world)))
			      (setf (gethash 'player-y world) (random (gethash 'unscaled-height world)))
			      (setf (gethash 'cells world) (randomrows))
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
