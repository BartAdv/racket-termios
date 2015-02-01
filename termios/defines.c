#include <termios.h>
#include "escheme.h"

#define MODULE_NAME "termios-defines"

Scheme_Object* scheme_reload(Scheme_Env* env)
{
  Scheme_Env *mod;
  Scheme_Object* obj;
  
  mod = scheme_primitive_module(scheme_intern_symbol(MODULE_NAME), env);

#define DEF(a) \
  obj = scheme_make_integer(a); \
  scheme_add_global(#a, obj, mod);

  DEF(NCCS);

  /* c_cc characters */
  DEF(VINTR);
  DEF(VQUIT);
  DEF(VERASE);
  DEF(VKILL);
  DEF(VEOF);
  DEF(VTIME);
  DEF(VMIN);
  DEF(VSWTC);
  DEF(VSTART);
  DEF(VSTOP);
  DEF(VSUSP);
  DEF(VEOL);
  DEF(VREPRINT);
  DEF(VDISCARD);
  DEF(VWERASE);
  DEF(VLNEXT);
  DEF(VEOL2);

  /* c_iflag bits */
  DEF(IGNBRK);
  DEF(BRKINT);
  DEF(IGNPAR);
  DEF(PARMRK);
  DEF(INPCK);
  DEF(ISTRIP);
  DEF(INLCR);
  DEF(IGNCR);
  DEF(ICRNL);
  DEF(IUCLC);
  DEF(IXON);
  DEF(IXANY);
  DEF(IXOFF);
  DEF(IMAXBEL);
  DEF(IUTF8);
  
  /* c_oflag bits */
  DEF(OPOST);
  DEF(OLCUC);
  DEF(ONLCR);
  DEF(OCRNL);
  DEF(ONOCR);
  DEF(ONLRET);
  DEF(OFILL);
  DEF(OFDEL);
#if defined __USE_MISC || defined __USE_XOPEN
  DEF(NLDLY);
  DEF(NL0);
  DEF(NL1);
  DEF(CRDLY);
  DEF(CR0);
  DEF(CR1);
  DEF(CR2);
  DEF(CR3);
  DEF(TABDLY);
  DEF(TAB0);
  DEF(TAB1);
  DEF(TAB2);
  DEF(TAB3);
  DEF(BSDLY);
  DEF(BS0);
  DEF(BS1);
  DEF(FFDLY);
  DEF(FF0);
  DEF(FF1);
#endif

  DEF(VTDLY);
  DEF(VT0);
  DEF(VT1);

#ifdef __USE_MISC
  DEF(XTABS);
#endif
  
  /* c_cflag bit meaning */
#ifdef __USE_MISC
  DEF(CBAUD);
#endif
  DEF(B0);
  DEF(B50);
  DEF(B75);
  DEF(B110);
  DEF(B134);
  DEF(B150);
  DEF(B200);
  DEF(B300);
  DEF(B600);
  DEF(B1200);
  DEF(B1800);
  DEF(B2400);
  DEF(B4800);
  DEF(B9600);
  DEF(B19200);
  DEF(B38400);
#ifdef __USE_MISC
  DEF(EXTA);
  DEF(EXTB);
#endif
  DEF(CSIZE);
  DEF(CS5);
  DEF(CS6);
  DEF(CS7);
  DEF(CS8);
  DEF(CSTOPB);
  DEF(CREAD);
  DEF(PARENB);
  DEF(PARODD);
  DEF(HUPCL);
  DEF(CLOCAL);
#ifdef __USE_MISC
  DEF(CBAUDEX);
#endif
  DEF(B57600);
  DEF(B115200);
  DEF(B230400);
  DEF(B460800);
  DEF(B500000);
  DEF(B576000);
  DEF(B921600);
  DEF(B1000000);
  DEF(B1152000);
  DEF(B1500000);
  DEF(B2000000);
  DEF(B2500000);
  DEF(B3000000);
  DEF(B3500000);
  DEF(B4000000);
#define __MAX_BAUD B4000000
#ifdef __USE_MISC
  DEF(CIBAUD);
  DEF(CMSPAR);
  DEF(CRTSCTS);
#endif

/* c_lflag bits */
  DEF(ISIG);
  DEF(ICANON);
#if defined __USE_MISC || defined __USE_XOPEN
  DEF(XCASE);
#endif
  DEF(ECHO);
  DEF(ECHOE);
  DEF(ECHOK);
  DEF(ECHONL);
  DEF(NOFLSH);
  DEF(TOSTOP);
#ifdef __USE_MISC
  DEF(ECHOCTL);
  DEF(ECHOPRT);
  DEF(ECHOKE);
  DEF(FLUSHO);
  DEF(PENDIN);
#endif
  DEF(IEXTEN);
#ifdef __USE_MISC
  DEF(EXTPROC);
#endif

  /* tcflow() and TCXONC use these */
  DEF(TCOOFF);
  DEF(TCOON);
  DEF(TCIOFF);
  DEF(TCION);

  /* tcflush() and TCFLSH use these */
  DEF(TCIFLUSH);
  DEF(TCOFLUSH);
  DEF(TCIOFLUSH);

  /* tcsetattr uses these */
  DEF(TCSANOW);
  DEF(TCSADRAIN);
  DEF(TCSAFLUSH);

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
