(ql:quickload "lispbuilder-sdl")
(ql:quickload "lispbuilder-sdl-image")
(ql:quickload "lispbuilder-sdl-ttf")
(ql:quickload "lispbuilder-sdl-mixer")
(load "game")
(load "main")
(require :sb-sprof)
(sb-sprof:with-profiling (:report :graph) (play-game))

