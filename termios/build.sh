raco ctool --xform defines.c
raco ctool --3m --cc defines.3m.c
mkdir -p compiled/native/x86-64-linux/3m
raco ctool --3m --ld compiled/native/x86_64-linux/3m/termios-defines_rkt.so defines_3m.o

