#include "mochi/value.h"
#include <stdint.h>

mochi_value_t c_box_int(int64_t n)    { return mochi_val_int(n); }
mochi_value_t c_box_str(const char *s) { return mochi_val_str(s); }
/* Returns the numeric tag (MOCHI_VAL_INT=2, MOCHI_VAL_STR=4). */
int64_t c_tag_of(mochi_value_t v) { return (int64_t)mochi_val_tag_of(v); }
