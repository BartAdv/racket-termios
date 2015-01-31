#lang racket/base

(require ffi/unsafe
	 ffi/unsafe/define)

(require "defines.rkt")
(provide (all-from-out "defines.rkt"))

(define _tcflag_t _uint)
(define _cc_t _byte)
(define _speed_t _uint)

(define NCCS 32)

(define-cstruct _TERMIOS
  ([c_iflag _tcflag_t]
   [c_oflag _tcflag_t]
   [c_cflag _tcflag_t]
   [c_lflag _tcflag_t]
   [c_cc (_array/vector _cc_t NCCS)]
   [c_ispeed _speed_t]
   [c_ospeed _speed_t]))

(define-ffi-definer define-termios (ffi-lib "libc.so.6"))

(define scheme_get_port_fd
  (get-ffi-obj 'scheme_get_port_fd #f (_fun _racket -> _intptr)))

;; based on https://github.com/dyoo/ffi-tutorial/blob/master/ffi/tutorial/examples/strdup/strdup.rkt
(define _file-port/no-null
  (make-ctype _int
    (λ (x)
      (unless (port? x)
        (raise-argument-error '_file-port/no-null "file-stream-port" x))
      (scheme_get_port_fd x))
    ;; no output
    (lambda (x) x)))

(define (check v who)
  (unless (zero? v)
    (error who "failed: ~a" v)))

(define-termios tcgetattr
  (_fun (f : _file-port/no-null)
	(t : (_ptr o _TERMIOS))
	-> (r : _int)
	-> (case r
	     [(EBADF) (raise-argument-error '_file-port/no-null "file-port" f)]
	     [(ENOTTY) (raise "File associated with given file port is not a terminal")]
	     [else t])))


(define-termios tcsetattr (_fun _file-port/no-null _int _TERMIOS-pointer -> (r : _int) -> (check r 'tcsetattr))) ; const pointer?
