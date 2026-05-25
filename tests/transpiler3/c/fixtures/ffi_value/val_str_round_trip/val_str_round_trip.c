#include "mochi/value.h"

mochi_value_t c_box_str(const char *s)      { return mochi_val_str(s); }
const char   *c_unbox_str(mochi_value_t v)  { return mochi_val_as_str(v); }
