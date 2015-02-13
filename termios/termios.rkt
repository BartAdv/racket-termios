#lang racket

(require ffi/unsafe
	 ffi/unsafe/define
	 racket/match)

(require "private/defines.rkt")
(provide (all-from-out "private/defines.rkt"))
(require (for-syntax "private/defines.rkt"))

(provide tcgetattr tcsetattr cfgetospeed cfgetispeed cfsetspeed
	 cfmakeraw tcsendbreak tcflush tcflow)

(define _tcflag_t _uint)
(define _cc_t _byte)
(define _speed_t _uint)

;; oh, this is cheap
(define-syntax (compile-when stx)
  (define s (syntax->datum stx))
  (if (eval (cadr s)) (datum->syntax stx (caddr s)) #'(void)))

(begin-for-syntax
 (define iospeed-fields? (and _HAVE_STRUCT_TERMIOS_C_ISPEED  _HAVE_STRUCT_TERMIOS_C_OSPEED)))

(compile-when iospeed-fields?
	      (define-cstruct _TERMIOS
		([c_iflag _tcflag_t]
		 [c_oflag _tcflag_t]
		 [c_cflag _tcflag_t]
		 [c_lflag _tcflag_t]
		 [c_cc (_array/vector _cc_t NCCS)]
		 [c_ispeed _speed_t]
		 [c_ospeed _speed_t])))

(compile-when (not iospeed-fields?)
	      (define-cstruct _TERMIOS
		([c_iflag _tcflag_t]
		 [c_oflag _tcflag_t]
		 [c_cflag _tcflag_t]
		 [c_lflag _tcflag_t]
		 [c_cc (_array/vector _cc_t NCCS)])))

;; most likely versions need to be adjusted
(define-ffi-definer define-termios (ffi-lib "libc" '["6" #f]))

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
  (_fun #:save-errno 'posix
	(f : _file-port/no-null)
	(t : (_ptr o _TERMIOS))
	-> (r : _int)
	-> (if (zero? r)
	       t
	       (match (saved-errno)
		 [(== EBADF) (raise-argument-error '_file-port/no-null "file-port" f)]
		 [(== ENOTTY) (error "File associated with given file port is not a terminal")]
		 [_ (error 'tcgetattr "Unknown errno: ~a" (saved-errno))]))))


(define-termios tcsetattr
  (_fun #:save-errno 'posix
	(f : _file-port/no-null) _int _TERMIOS-pointer ; const pointer?
	-> (r : _int)
	-> (unless (zero? r)
	     (match (saved-errno)
	       [(== EBADF) (raise-argument-error '_file_port/no-null "file-port" f)]
	       [(== EINTR) (error "A signal interrupted tcsetattr")]
	       [(== EINVAL) (error "The optional_actions argument is not a supported value, or an attempt was made to change an attribute represented in the termios structure to an unsupported value")]
	       [(== ENOTTY) (error "File associated with given file port is not a terminal")]
	       [(== EIO) (error "The process group of the writing process is orphaned, and the writing process is not ignoring or blocking SIGTTOU")]
	       [_ (error 'tcsetattr "nknown errno: ~a" (saved-errno))]))))

(define-termios cfgetospeed (_fun _TERMIOS-pointer -> _speed_t))

(define-termios cfgetispeed (_fun _TERMIOS-pointer -> _speed_t))

(define-syntax-rule (einval r)
  (unless (zero? r)
    (match (saved-errno)
      [(== EINVAL) (error "The speed value is not a valid baud rate")])))
(define-termios cfsetospeed
  (_fun #:save-errno 'posix
	_TERMIOS-pointer _speed_t
	-> (r : _int)
	-> (einval r)))

(define-termios cfsetispeed
  (_fun #:save-errno 'posix
	_TERMIOS-pointer _speed_t
	-> (r : _int)
	-> (einval r)))

(compile-when __USE_MISC
 (define-termios cfsetspeed
   (_fun #:save-errno 'posix
	 _TERMIOS-pointer _speed_t
	 -> (r : _int)
	 -> (einval r))))
				   

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

(compile-when (or __USE_UNIX98 __USE_XOPEN2K8) (provide tcgetsid))
;; TODO: _pid_t ?
(compile-when (or __USE_UNIX98 __USE_XOPEN2K8)
 (define-termios tcgetsid (_fun _file-port/no-null -> _uint)))
