#include <termios.h>
#include <errno.h>
#include "escheme.h"

#define MODULE_NAME "defines"

void define_bool(Scheme_Env* mod, const char* n, int v)
{
  Scheme_Object* obj;
  obj = v ? scheme_true : scheme_false;
  scheme_add_global(n, obj, mod);
}

Scheme_Object* scheme_reload(Scheme_Env* env)
{
  Scheme_Env *mod;
  Scheme_Object* obj;
  
  mod = scheme_primitive_module(scheme_intern_symbol(MODULE_NAME), env);

#define DEF(a) \
  obj = scheme_make_integer(a); \
  scheme_add_global(#a, obj, mod);
#define UNDEF(a) \
  scheme_add_global(#a, scheme_false, mod);

#ifdef EBADF 
  DEF(EBADF);
#else
  UNDEF(EBADF);
#endif
#ifdef ENOTTY 
  DEF(ENOTTY);
#else
  UNDEF(ENOTTY);
#endif
#ifdef EINTR 
  DEF(EINTR);
#else
  UNDEF(EINTR);
#endif
#ifdef EINVAL 
  DEF(EINVAL);
#else
  UNDEF(EINVAL);
#endif
#ifdef EIO 
  DEF(EIO);
#else
  UNDEF(EIO);
#endif
	      
#if defined __USE_MISC
  define_bool(mod, "__USE_MISC", 1);
#else
  define_bool(mod, "__USE_MISC", 0);
#endif

#if defined __USE_UNIX98
  define_bool(mod, "__USE_UNIX98", 1);
#else
  define_bool(mod, "__USE_UNIX98", 0);
#endif

#if defined __USE_XOPEN2K8
  define_bool(mod, "__USE_XOPEN2K8", 1);
#else
  define_bool(mod, "__USE_XOPEN2K8", 0);
#endif

#if defined _HAVE_STRUCT_TERMIOS_C_ISPEED
  define_bool(mod, "_HAVE_STRUCT_TERMIOS_C_ISPEED", 1);
#else
  define_bool(mod, "_HAVE_STRUCT_TERMIOS_C_ISPEED", 0);
#endif
  
#if defined _HAVE_STRUCT_TERMIOS_C_OSPEED
  define_bool(mod, "_HAVE_STRUCT_TERMIOS_C_OSPEED", 1);
#else
  define_bool(mod, "_HAVE_STRUCT_TERMIOS_C_OSPEED", 0);
#endif

#ifdef NCCS 
  DEF(NCCS);
#else
  UNDEF(NCCS);
#endif

/* c_cc characters */
#ifdef VINTR 
  DEF(VINTR);
#else
  UNDEF(VINTR);
#endif
#ifdef VQUIT 
  DEF(VQUIT);
#else
  UNDEF(VQUIT);
#endif
#ifdef VERASE 
  DEF(VERASE);
#else
  UNDEF(VERASE);
#endif
#ifdef VKILL 
  DEF(VKILL);
#else
  UNDEF(VKILL);
#endif
#ifdef VEOF 
  DEF(VEOF);
#else
  UNDEF(VEOF);
#endif
#ifdef VTIME 
  DEF(VTIME);
#else
  UNDEF(VTIME);
#endif
#ifdef VMIN 
  DEF(VMIN);
#else
  UNDEF(VMIN);
#endif
#ifdef VSWTC 
  DEF(VSWTC);
#else
  UNDEF(VSWTC);
#endif
#ifdef VSTART 
  DEF(VSTART);
#else
  UNDEF(VSTART);
#endif
#ifdef VSTOP 
  DEF(VSTOP);
#else
  UNDEF(VSTOP);
#endif
#ifdef VSUSP 
  DEF(VSUSP);
#else
  UNDEF(VSUSP);
#endif
#ifdef VEOL 
  DEF(VEOL);
#else
  UNDEF(VEOL);
#endif
#ifdef VREPRINT 
  DEF(VREPRINT);
#else
  UNDEF(VREPRINT);
#endif
#ifdef VDISCARD 
  DEF(VDISCARD);
#else
  UNDEF(VDISCARD);
#endif
#ifdef VWERASE 
  DEF(VWERASE);
#else
  UNDEF(VWERASE);
#endif
#ifdef VLNEXT 
  DEF(VLNEXT);
#else
  UNDEF(VLNEXT);
#endif
#ifdef VEOL2 
  DEF(VEOL2);
#else
  UNDEF(VEOL2);
#endif

/* c_iflag bits */
#ifdef IGNBRK 
  DEF(IGNBRK);
#else
  UNDEF(IGNBRK);
#endif
#ifdef BRKINT 
  DEF(BRKINT);
#else
  UNDEF(BRKINT);
#endif
#ifdef IGNPAR 
  DEF(IGNPAR);
#else
  UNDEF(IGNPAR);
#endif
#ifdef PARMRK 
  DEF(PARMRK);
#else
  UNDEF(PARMRK);
#endif
#ifdef INPCK 
  DEF(INPCK);
#else
  UNDEF(INPCK);
#endif
#ifdef ISTRIP 
  DEF(ISTRIP);
#else
  UNDEF(ISTRIP);
#endif
#ifdef INLCR 
  DEF(INLCR);
#else
  UNDEF(INLCR);
#endif
#ifdef IGNCR 
  DEF(IGNCR);
#else
  UNDEF(IGNCR);
#endif
#ifdef ICRNL 
  DEF(ICRNL);
#else
  UNDEF(ICRNL);
#endif
#ifdef IUCLC 
  DEF(IUCLC);
#else
  UNDEF(IUCLC);
#endif
#ifdef IXON 
  DEF(IXON);
#else
  UNDEF(IXON);
#endif
#ifdef IXANY 
  DEF(IXANY);
#else
  UNDEF(IXANY);
#endif
#ifdef IXOFF 
  DEF(IXOFF);
#else
  UNDEF(IXOFF);
#endif
#ifdef IMAXBEL 
  DEF(IMAXBEL);
#else
  UNDEF(IMAXBEL);
#endif
#ifdef IUTF8 
  DEF(IUTF8);
#else
  UNDEF(IUTF8);
#endif
  
  /* c_oflag bits */
#ifdef OPOST 
  DEF(OPOST);
#else
  UNDEF(OPOST);
#endif
#ifdef OLCUC 
  DEF(OLCUC);
#else
  UNDEF(OLCUC);
#endif
#ifdef ONLCR 
  DEF(ONLCR);
#else
  UNDEF(ONLCR);
#endif
#ifdef OCRNL 
  DEF(OCRNL);
#else
  UNDEF(OCRNL);
#endif
#ifdef ONOCR 
  DEF(ONOCR);
#else
  UNDEF(ONOCR);
#endif
#ifdef ONLRET 
  DEF(ONLRET);
#else
  UNDEF(ONLRET);
#endif
#ifdef OFILL 
  DEF(OFILL);
#else
  UNDEF(OFILL);
#endif
#ifdef OFDEL 
  DEF(OFDEL);
#else
  UNDEF(OFDEL);
#endif

#ifdef NLDLY 
  DEF(NLDLY);
#else
  UNDEF(NLDLY);
#endif
#ifdef NL0 
  DEF(NL0);
#else
  UNDEF(NL0);
#endif
#ifdef NL1 
  DEF(NL1);
#else
  UNDEF(NL1);
#endif
#ifdef CRDLY 
  DEF(CRDLY);
#else
  UNDEF(CRDLY);
#endif
#ifdef CR0 
  DEF(CR0);
#else
  UNDEF(CR0);
#endif
#ifdef CR1 
  DEF(CR1);
#else
  UNDEF(CR1);
#endif
#ifdef CR2 
  DEF(CR2);
#else
  UNDEF(CR2);
#endif
#ifdef CR3 
  DEF(CR3);
#else
  UNDEF(CR3);
#endif
#ifdef TABDLY 
  DEF(TABDLY);
#else
  UNDEF(TABDLY);
#endif
#ifdef TAB0 
  DEF(TAB0);
#else
  UNDEF(TAB0);
#endif
#ifdef TAB1 
  DEF(TAB1);
#else
  UNDEF(TAB1);
#endif
#ifdef TAB2 
  DEF(TAB2);
#else
  UNDEF(TAB2);
#endif
#ifdef TAB3 
  DEF(TAB3);
#else
  UNDEF(TAB3);
#endif
#ifdef BSDLY 
  DEF(BSDLY);
#else
  UNDEF(BSDLY);
#endif
#ifdef BS0 
  DEF(BS0);
#else
  UNDEF(BS0);
#endif
#ifdef BS1 
  DEF(BS1);
#else
  UNDEF(BS1);
#endif
#ifdef FFDLY 
  DEF(FFDLY);
#else
  UNDEF(FFDLY);
#endif
#ifdef FF0 
  DEF(FF0);
#else
  UNDEF(FF0);
#endif
#ifdef FF1 
  DEF(FF1);
#else
  UNDEF(FF1);
#endif

#ifdef VTDLY 
  DEF(VTDLY);
#else
  UNDEF(VTDLY);
#endif
#ifdef VT0 
  DEF(VT0);
#else
  UNDEF(VT0);
#endif
#ifdef VT1 
  DEF(VT1);
#else
  UNDEF(VT1);
#endif

#ifdef XTABS 
  DEF(XTABS);
#else
  UNDEF(XTABS);
#endif
  
/* c_cflag bit meaning */
#ifdef CBAUD 
  DEF(CBAUD);
#else
  UNDEF(CBAUD);
#endif
#ifdef B0 
  DEF(B0);
#else
  UNDEF(B0);
#endif
#ifdef B50 
  DEF(B50);
#else
  UNDEF(B50);
#endif
#ifdef B75 
  DEF(B75);
#else
  UNDEF(B75);
#endif
#ifdef B110 
  DEF(B110);
#else
  UNDEF(B110);
#endif
#ifdef B134 
  DEF(B134);
#else
  UNDEF(B134);
#endif
#ifdef B150 
  DEF(B150);
#else
  UNDEF(B150);
#endif
#ifdef B200 
  DEF(B200);
#else
  UNDEF(B200);
#endif
#ifdef B300 
  DEF(B300);
#else
  UNDEF(B300);
#endif
#ifdef B600 
  DEF(B600);
#else
  UNDEF(B600);
#endif
#ifdef B1200 
  DEF(B1200);
#else
  UNDEF(B1200);
#endif
#ifdef B1800 
  DEF(B1800);
#else
  UNDEF(B1800);
#endif
#ifdef B2400 
  DEF(B2400);
#else
  UNDEF(B2400);
#endif
#ifdef B4800 
  DEF(B4800);
#else
  UNDEF(B4800);
#endif
#ifdef B9600 
  DEF(B9600);
#else
  UNDEF(B9600);
#endif
#ifdef B19200 
  DEF(B19200);
#else
  UNDEF(B19200);
#endif
#ifdef B38400 
  DEF(B38400);
#else
  UNDEF(B38400);
#endif
#ifdef EXTA 
  DEF(EXTA);
#else
  UNDEF(EXTA);
#endif
#ifdef EXTB 
  DEF(EXTB);
#else
  UNDEF(EXTB);
#endif
#ifdef CSIZE 
  DEF(CSIZE);
#else
  UNDEF(CSIZE);
#endif
#ifdef CS5 
  DEF(CS5);
#else
  UNDEF(CS5);
#endif
#ifdef CS6 
  DEF(CS6);
#else
  UNDEF(CS6);
#endif
#ifdef CS7 
  DEF(CS7);
#else
  UNDEF(CS7);
#endif
#ifdef CS8 
  DEF(CS8);
#else
  UNDEF(CS8);
#endif
#ifdef CSTOPB 
  DEF(CSTOPB);
#else
  UNDEF(CSTOPB);
#endif
#ifdef CREAD 
  DEF(CREAD);
#else
  UNDEF(CREAD);
#endif
#ifdef PARENB 
  DEF(PARENB);
#else
  UNDEF(PARENB);
#endif
#ifdef PARODD 
  DEF(PARODD);
#else
  UNDEF(PARODD);
#endif
#ifdef HUPCL 
  DEF(HUPCL);
#else
  UNDEF(HUPCL);
#endif
#ifdef CLOCAL 
  DEF(CLOCAL);
#else
  UNDEF(CLOCAL);
#endif
#ifdef CBAUDEX 
  DEF(CBAUDEX);
#else
  UNDEF(CBAUDEX);
#endif
#ifdef B57600 
  DEF(B57600);
#else
  UNDEF(B57600);
#endif
#ifdef B115200 
  DEF(B115200);
#else
  UNDEF(B115200);
#endif
#ifdef B230400 
  DEF(B230400);
#else
  UNDEF(B230400);
#endif
#ifdef B460800 
  DEF(B460800);
#else
  UNDEF(B460800);
#endif
#ifdef B500000 
  DEF(B500000);
#else
  UNDEF(B500000);
#endif
#ifdef B576000 
  DEF(B576000);
#else
  UNDEF(B576000);
#endif
#ifdef B921600 
  DEF(B921600);
#else
  UNDEF(B921600);
#endif
#ifdef B1000000 
  DEF(B1000000);
#else
  UNDEF(B1000000);
#endif
#ifdef B1152000 
  DEF(B1152000);
#else
  UNDEF(B1152000);
#endif
#ifdef B1500000 
  DEF(B1500000);
#else
  UNDEF(B1500000);
#endif
#ifdef B2000000 
  DEF(B2000000);
#else
  UNDEF(B2000000);
#endif
#ifdef B2500000 
  DEF(B2500000);
#else
  UNDEF(B2500000);
#endif
#ifdef B3000000 
  DEF(B3000000);
#else
  UNDEF(B3000000);
#endif
#ifdef B3500000 
  DEF(B3500000);
#else
  UNDEF(B3500000);
#endif
#ifdef B4000000 
  DEF(B4000000);
#else
  UNDEF(B4000000);
#endif
#ifdef __MAX_BAUD
  DEF(__MAX_BAUD);
#else
  UNDEF(__MAX_BAUD);
#endif
#ifdef CIBAUD 
  DEF(CIBAUD);
#else
  UNDEF(CIBAUD);
#endif
#ifdef CMSPAR 
  DEF(CMSPAR);
#else
  UNDEF(CMSPAR);
#endif
#ifdef CRTSCTS 
  DEF(CRTSCTS);
#else
  UNDEF(CRTSCTS);
#endif

/* c_lflag bits */
#ifdef ISIG 
  DEF(ISIG);
#else
  UNDEF(ISIG);
#endif
#ifdef ICANON 
  DEF(ICANON);
#else
  UNDEF(ICANON);
#endif
#ifdef XCASE 
  DEF(XCASE);
#else
  UNDEF(XCASE);
#endif
#ifdef ECHO 
  DEF(ECHO);
#else
  UNDEF(ECHO);
#endif
#ifdef ECHOE 
  DEF(ECHOE);
#else
  UNDEF(ECHOE);
#endif
#ifdef ECHOK 
  DEF(ECHOK);
#else
  UNDEF(ECHOK);
#endif
#ifdef ECHONL 
  DEF(ECHONL);
#else
  UNDEF(ECHONL);
#endif
#ifdef NOFLSH 
  DEF(NOFLSH);
#else
  UNDEF(NOFLSH);
#endif
#ifdef TOSTOP 
  DEF(TOSTOP);
#else
  UNDEF(TOSTOP);
#endif
#ifdef ECHOCTL 
  DEF(ECHOCTL);
#else
  UNDEF(ECHOCTL);
#endif
#ifdef ECHOPRT 
  DEF(ECHOPRT);
#else
  UNDEF(ECHOPRT);
#endif
#ifdef ECHOKE 
  DEF(ECHOKE);
#else
  UNDEF(ECHOKE);
#endif
#ifdef FLUSHO 
  DEF(FLUSHO);
#else
  UNDEF(FLUSHO);
#endif
#ifdef PENDIN 
  DEF(PENDIN);
#else
  UNDEF(PENDIN);
#endif
#ifdef IEXTEN 
  DEF(IEXTEN);
#else
  UNDEF(IEXTEN);
#endif
#ifdef EXTPROC 
  DEF(EXTPROC);
#else
  UNDEF(EXTPROC);
#endif

/* tcflow() and TCXONC use these */
#ifdef TCOOFF 
  DEF(TCOOFF);
#else
  UNDEF(TCOOFF);
#endif
#ifdef TCOON 
  DEF(TCOON);
#else
  UNDEF(TCOON);
#endif
#ifdef TCIOFF 
  DEF(TCIOFF);
#else
  UNDEF(TCIOFF);
#endif
#ifdef TCION 
  DEF(TCION);
#else
  UNDEF(TCION);
#endif

/* tcflush() and TCFLSH use these */
#ifdef TCIFLUSH 
  DEF(TCIFLUSH);
#else
  UNDEF(TCIFLUSH);
#endif
#ifdef TCOFLUSH 
  DEF(TCOFLUSH);
#else
  UNDEF(TCOFLUSH);
#endif
#ifdef TCIOFLUSH 
  DEF(TCIOFLUSH);
#else
  UNDEF(TCIOFLUSH);
#endif

/* tcsetattr uses these */
#ifdef TCSANOW 
  DEF(TCSANOW);
#else
  UNDEF(TCSANOW);
#endif
#ifdef TCSADRAIN 
  DEF(TCSADRAIN);
#else
  UNDEF(TCSADRAIN);
#endif
#ifdef TCSAFLUSH 
  DEF(TCSAFLUSH);
#else
  UNDEF(TCSAFLUSH);
#endif

  scheme_finish_primitive_module(mod);
  return scheme_void;
}

Scheme_Object* scheme_initialize(Scheme_Env* env)
{
  return scheme_reload(env);
}

Scheme_Object* scheme_module_name()
{
  return scheme_intern_symbol(MODULE_NAME);
}
