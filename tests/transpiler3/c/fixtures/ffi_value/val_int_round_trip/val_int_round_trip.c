#include "mochi/value.h"
#include <stdint.h>

mochi_value_t c_box_int(int64_t n)   { return mochi_val_int(n); }
int64_t       c_unbox_int(mochi_value_t v) { return mochi_val_as_int(v); }
