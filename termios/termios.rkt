#lang racket/base

(require ffi/unsafe
	 ffi/unsafe/define)

(require "private/defines.rkt")
(provide (all-from-out "private/defines.rkt"))

(provide tcgetattr)
(provide tcsetattr)

(define _tcflag_t _uint)
(define _cc_t _byte)
(define _speed_t _uint)

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

(define-termios cfgetospeed (_fun _TERMIOS-pointer -> _speed_t))

(define-termios cfgetispeed (_fun _TERMIOS-pointer -> _speed_t))

(define-termios cfsetospeed (_fun _TERMIOS-pointer _speed_t
				  -> (r : _int)
				  -> (check r 'cfsetospeed)))

(define-termios cfsetispeed (_fun _TERMIOS-pointer _speed_t
				  -> (r : _int)
				  -> (check r 'cfsetispeed)))

;; TODO: make it conditional basing on __USE_MISC
(define-termios cfsetspeed (_fun _TERMIOS-pointer _speed_t
				 -> (r : _int)
				 -> (check r 'cfsetspeed))))
				   

(define-termios cfmakeraw (_fun _TERMIOS-pointer -> _void))

(define-termios tcsendbreak (_fun _file-port/no-null _int
				  -> (r : _int)
				  -> (check r 'tcsendbreak)))

(define-termios tcflush (_fun _file-port/no-null _int
			      -> (r : _int)
			      -> (check r 'tcsendbreak)))

(define-termios tcflow (_fun _file-port/no-null _int
			     -> (r : _int)
			     -> (check r 'tcsendbreak)))

;; TODO: conditionally basing on __USE_UNIX98 or __USE_XOPEN2K8
;; TODO: _pid_t ?
(define-termios tcgetsid (_fun _file-port/no-null -> _uint))
