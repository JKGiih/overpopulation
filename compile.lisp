(ql:quickload "lispbuilder-sdl")
(ql:quickload "lispbuilder-sdl-image")
(ql:quickload "lispbuilder-sdl-ttf")
(ql:quickload "lispbuilder-sdl-mixer")
(load (compile-file "game"))
(load (compile-file "main"))
(save-lisp-and-die "main" :toplevel #'main :executable t)
