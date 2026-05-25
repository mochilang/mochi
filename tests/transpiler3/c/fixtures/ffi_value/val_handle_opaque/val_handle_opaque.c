#include "mochi/value.h"
#include <stdint.h>
#include <stdlib.h>

mochi_value_t c_alloc_counter(void) {
    int64_t *p = (int64_t *)malloc(sizeof(int64_t));
    *p = 0;
    return mochi_val_handle(p);
}

int64_t c_counter_inc(mochi_value_t v) {
    int64_t *p = (int64_t *)mochi_val_as_handle(v);
    return ++(*p);
}

int64_t c_counter_get(mochi_value_t v) {
    int64_t *p = (int64_t *)mochi_val_as_handle(v);
    return *p;
}
