/*
 * libmochi: boxed value type for FFI-crossing values.
 *
 * MEP-45 Phase 10.1. See website/docs/mep/mep-0045.md.
 *
 * mochi_value_t is a tagged union that can carry any Mochi scalar across
 * a C FFI boundary without losing type information. The union covers the
 * five Mochi scalar kinds (nil, bool, int, float, string) plus an opaque
 * handle for C-allocated objects.
 *
 * Constructors return a fresh mochi_value_t by value (16 bytes on LP64).
 * Accessors are undefined behaviour if the wrong tag is used; always call
 * the matching mochi_val_is_* predicate first.
 */

#ifndef MOCHI_VALUE_H
#define MOCHI_VALUE_H

#include <stdint.h>

/* Discriminator tag. */
typedef enum {
    MOCHI_VAL_NIL    = 0,
    MOCHI_VAL_BOOL   = 1,
    MOCHI_VAL_INT    = 2,
    MOCHI_VAL_FLOAT  = 3,
    MOCHI_VAL_STR    = 4,
    MOCHI_VAL_HANDLE = 5,
} mochi_val_tag;

/* The boxed value struct. Pass and return by value. */
typedef struct {
    mochi_val_tag tag;
    union {
        int         b; /* MOCHI_VAL_BOOL   */
        int64_t     i; /* MOCHI_VAL_INT    */
        double      f; /* MOCHI_VAL_FLOAT  */
        const char *s; /* MOCHI_VAL_STR    (borrowed; caller owns lifetime) */
        void       *h; /* MOCHI_VAL_HANDLE (opaque C pointer) */
    } as;
} mochi_value_t;

/* Constructors */
mochi_value_t mochi_val_nil(void);
mochi_value_t mochi_val_bool(int b);
mochi_value_t mochi_val_int(int64_t i);
mochi_value_t mochi_val_float(double f);
mochi_value_t mochi_val_str(const char *s);
mochi_value_t mochi_val_handle(void *h);

/* Tag predicates (return 1 if tag matches, 0 otherwise) */
int mochi_val_is_nil(mochi_value_t v);
int mochi_val_is_bool(mochi_value_t v);
int mochi_val_is_int(mochi_value_t v);
int mochi_val_is_float(mochi_value_t v);
int mochi_val_is_str(mochi_value_t v);
int mochi_val_is_handle(mochi_value_t v);

/* Accessors: UB if tag does not match the expected kind. */
int         mochi_val_as_bool(mochi_value_t v);
int64_t     mochi_val_as_int(mochi_value_t v);
double      mochi_val_as_float(mochi_value_t v);
const char *mochi_val_as_str(mochi_value_t v);
void       *mochi_val_as_handle(mochi_value_t v);

/* Tag accessor: returns the raw mochi_val_tag discriminator. */
mochi_val_tag mochi_val_tag_of(mochi_value_t v);

#endif /* MOCHI_VALUE_H */
