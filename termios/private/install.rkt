#lang racket/base

(require make/setup-extension)

(provide pre-installer)

;; based on https://github.com/tonyg/racket-packet-socket/blob/master/packet-socket/private/install.rkt

(define (pre-installer collections-top-path racket-termios-path)
  (pre-install racket-termios-path
	     (build-path racket-termios-path "private")
	     "defines.c"
	     "."
	     '()
	     '()
	     '()
	     '()
	     '()
	     '()
	     (lambda (thunk) (thunk))
	     #t))
