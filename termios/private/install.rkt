#lang racket/base

(require make/setup-extension)

(pre-install ".."
	     (build-path ".." "private")
	     "defines.c"
	     "."
	       '()
	       '()
	       '()
	       '()
	       '()
	       '()
	       (lambda (thunk) (thunk))
	       #t)
