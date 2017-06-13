#ifndef __HSC_MACROS_H__
#define __HSC_MACROS_H__

/*
This files defines some hsc2hs macros. For documentation on how to construct custom macros see:
https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/utils.html#custom-constructs
*/

#include <bindings.dsl.h>

#define bc_sizeof_varid(name) {printf("c'sizeof_");bc_word(name);}; \

/*
The #sizeof macro outputs a Haskell constant with the size of the C/C++ type in bytes.

For example the following:

  #sizeof Point2i

Results in the following Haskell code:

  c'sizeof_Point2i :: Int
  c'sizeof_Point2i = 8
*/
#define hsc_sizeof(name) \
  { bc_sizeof_varid(# name);printf(" :: Int\n"); \
    bc_sizeof_varid(# name);printf(" = %lu\n", sizeof(name)); \
  }; \

#endif /* __HSC_MACROS_H__ */
