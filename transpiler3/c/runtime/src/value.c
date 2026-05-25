/*
 * libmochi: boxed value type implementation.
 *
 * MEP-45 Phase 10.1. See website/docs/mep/mep-0045.md.
 *
 * All functions are trivial one-liners; the header provides the type
 * definition. Separated into a .c file so the linker can strip them
 * when unused.
 */

#include "mochi/value.h"

mochi_value_t mochi_val_nil(void) {
    mochi_value_t v;
    v.tag  = MOCHI_VAL_NIL;
    v.as.i = 0;
    return v;
}

mochi_value_t mochi_val_bool(int b) {
    mochi_value_t v;
    v.tag  = MOCHI_VAL_BOOL;
    v.as.b = b ? 1 : 0;
    return v;
}

mochi_value_t mochi_val_int(int64_t i) {
    mochi_value_t v;
    v.tag  = MOCHI_VAL_INT;
    v.as.i = i;
    return v;
}

mochi_value_t mochi_val_float(double f) {
    mochi_value_t v;
    v.tag  = MOCHI_VAL_FLOAT;
    v.as.f = f;
    return v;
}

mochi_value_t mochi_val_str(const char *s) {
    mochi_value_t v;
    v.tag  = MOCHI_VAL_STR;
    v.as.s = s;
    return v;
}

mochi_value_t mochi_val_handle(void *h) {
    mochi_value_t v;
    v.tag  = MOCHI_VAL_HANDLE;
    v.as.h = h;
    return v;
}

int mochi_val_is_nil(mochi_value_t v)    { return v.tag == MOCHI_VAL_NIL; }
int mochi_val_is_bool(mochi_value_t v)   { return v.tag == MOCHI_VAL_BOOL; }
int mochi_val_is_int(mochi_value_t v)    { return v.tag == MOCHI_VAL_INT; }
int mochi_val_is_float(mochi_value_t v)  { return v.tag == MOCHI_VAL_FLOAT; }
int mochi_val_is_str(mochi_value_t v)    { return v.tag == MOCHI_VAL_STR; }
int mochi_val_is_handle(mochi_value_t v) { return v.tag == MOCHI_VAL_HANDLE; }

int         mochi_val_as_bool(mochi_value_t v)   { return v.as.b; }
int64_t     mochi_val_as_int(mochi_value_t v)    { return v.as.i; }
double      mochi_val_as_float(mochi_value_t v)  { return v.as.f; }
const char *mochi_val_as_str(mochi_value_t v)    { return v.as.s; }
void       *mochi_val_as_handle(mochi_value_t v) { return v.as.h; }

mochi_val_tag mochi_val_tag_of(mochi_value_t v)  { return v.tag; }
