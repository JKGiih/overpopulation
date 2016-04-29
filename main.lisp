(defun main ()

  ;; Load SDL
  (cffi:define-foreign-library sdl
			       (:darwin (:or (:framework "SDL")
					     (:default "libSDL")))
			       (:windows "SDL.dll")
			       (:unix (:or "libSDL-1.2.so.0.7.2"
					   "libSDL-1.2.so.0"
					   "libSDL-1.2.so"
					   "libSDL.so"
					   "libSDL")))
  (cffi:use-foreign-library sdl)

  ;; Run the game
  (sdl:with-init ()
		 (play-game))

  ;; Quit
  #-allegro (quit) #+allegro (exit))
