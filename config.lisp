(defun change-config (c)
  (setf (gethash 'width c) 1024)                                    ;; width of the screen. default 1024.
  (setf (gethash 'height c) 768)                                    ;; height of the screen. default 768.
  (setf (gethash 'fullscreen c) NIL)                                ;; fullscreen. possible values t for true and NIL for false. default NIL.
  (setf (gethash 'npc-color c) (sdl:color :r 177 :g 113 :b 57))     ;; npc color in rgb. default (sdl:color :r 177 :g 113 :b 57).
  (setf (gethash 'bg-color c) (sdl:color :r 4 :g 27 :b 0))          ;; background color in rgb. default (sdl:color :r 4 :g 27 :b 0).
  (setf (gethash 'player-color c) (sdl:color :r 167 :g 124 :b 57))  ;; player color in rgb. default (sdl:color :r 167 :g 124 :b 57).
  (setf (gethash 'sound-on-p c) t)                                  ;; sound effects. possible values t for ON and NIL for OFF. default t.
  (setf (gethash 'sound-volume c) 85)                               ;; sound effects volume. possible values [0, 128]. default 85.
  (setf (gethash 'music-on-p c) t)                                  ;; music. possible values t for ON and NIL for OFF. default t.
  (setf (gethash 'music-volume c) 85)                               ;; music volume. possible values [0, 128]. default 85.
  c)
