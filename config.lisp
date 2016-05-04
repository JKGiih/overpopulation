(defun change-config (c)
  (setf (gethash 'width c) 1024)                                     ;; width of the screen. default 800.
  (setf (gethash 'height c) 768)                                    ;; height of the screen. default 600.
  (setf (gethash 'fullscreen c) NIL)                                ;; fullscreen. possible values t for true and NIL for false. default NIL.
  (setf (gethash 'npc-color c) sdl:*black*)                         ;; cell color in rgb.
  (setf (gethash 'bg-color c) sdl:*green*)                          ;; background color in rgb.
  (setf (gethash 'player-color c) (sdl:color :r 224 :g 255 :b 192)) ;; player color in rgb.
  (setf (gethash 'sound-on-p c) t)                                  ;; sound effects. possible values t for ON and NIL for OFF. default t.
  (setf (gethash 'sound-volume c) 96)                               ;; sound effects volume. possible values [0, 128]. default 96.
  (setf (gethash 'music-on-p c) NIL)                                ;; music. possible values t for ON and NIL for OFF. default t.
  (setf (gethash 'music-volume c) 96)                               ;; music volume. possible values [0, 128]. default 96.
  c)
