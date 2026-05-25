#include "mochi/value.h"

mochi_value_t c_box_float(double x)        { return mochi_val_float(x); }
double        c_unbox_float(mochi_value_t v) { return mochi_val_as_float(v); }
