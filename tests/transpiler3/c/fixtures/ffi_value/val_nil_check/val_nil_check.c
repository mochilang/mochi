#include "mochi/value.h"

mochi_value_t c_make_nil(void)       { return mochi_val_nil(); }
int           c_is_nil(mochi_value_t v) { return mochi_val_is_nil(v); }
