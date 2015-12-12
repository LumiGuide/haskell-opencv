#ifndef __THEA_MACROS_H__
#define __THEA_MACROS_H__

#include <bindings.dsl.h>

#define bc_sizeof_varid(name) {printf("c'sizeof_");bc_word(name);}; \

#define hsc_sizeof(name) \
  { bc_sizeof_varid(# name);printf(" :: Int\n"); \
    bc_sizeof_varid(# name);printf(" = %lu\n", sizeof(name)); \
  }; \

#endif /* __THEA_MACROS_H__ */
