# Overpopulation, Or: Divided and Disillusioned We Wander in a World Gasping Under the Weight of Our Folly

### Summary

A game utilizing Lispbuilder-SDL.

### Requirements

A Common Lisp compiler, Quicklisp and the following SDL libraries:

- sdl
- sdl_gfx
- sdl_image
- sdl_mixer
- sdl_sound
- sdl_ttf

### Compilation on GNU/Linux with SBCL and Quicklisp

```
(ql:quickload "lispbuilder-sdl")
(ql:quickload "lispbuilder-sdl-image")
(ql:quickload "lispbuilder-sdl-ttf")
(ql:quickload "lispbuilder-sdl-mixer")
(load (compile-file "game"))
(load (compile-file "main"))
(save-lisp-and-die "main" :toplevel #'main :executable t)
```

These steps may be automated by load the included file compile.lisp. 

See the Lispbuilder wiki for more information.
