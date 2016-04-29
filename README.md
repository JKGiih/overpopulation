# lispbuilder-sdl-skeleton

### Summary

A game utilizing Lispbuilder-SDL.

### Requirements

A Common Lisp compiler and Quicklisp.

### Compilation with SBCL and Quicklisp

```
(ql:quickload "lispbuilder-sdl")
(ql:quickload "lispbuilder-sdl-image")
(ql:quickload "lispbuilder-sdl-ttf")
(ql:quickload "lispbuilder-sdl-mixer")
(load (compile-file "game"))
(load (compile-file "main"))
(save-lisp-and-die "main" :toplevel #'main :executable t)
```

See the Lispbuilder wiki for more information.
