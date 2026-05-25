#include "mochi/value.h"

mochi_value_t c_box_bool(int b)        { return mochi_val_bool(b); }
int           c_unbox_bool(mochi_value_t v) { return mochi_val_as_bool(v); }
