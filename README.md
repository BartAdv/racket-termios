# Racket FFI to termios functions

The bindings expose API as defined in `termios.h` along with constants defined there. Constants are defined via helper extension module, meaning the values are the actual values used by the environment the package is built in.

The constants are defined conditionally, that is, when the value is not defined in C headers, it ends up as `#f` in Racket. This allows the code that uses this package to determine whether certain flags are available on current platform.

## TODO
* Struct field offsets basing on real ones
* Determine what to do with ispeed/ospeed fields
