/*
 * MEP-42 Phase 4.2.21 nested integer list runtime.
 *
 * Backs Mochi's list<list<int>> when the program is compiled with
 * `mochi build --target=c`. The IR ops OpNewListList, OpListListLen,
 * OpListListPush, OpListListGet, and OpListListToStr lower to calls
 * into this header.
 *
 * Identity: pure C99, no libc beyond stdlib.h/string.h/stdint.h.
 * The outer carrier owns a growable mochi_list_i64** array; the
 * inner lists are not copied on push (the outer struct borrows the
 * caller's mochi_list_i64 pointer). Allocations leak at process exit
 * like the rest of the Phase 4 C runtime.
 */
#ifndef MOCHI_RUNTIME_C_LIST_LIST_H
#define MOCHI_RUNTIME_C_LIST_LIST_H

#include <stdint.h>

#include "mochi_list_i64.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct mochi_list_list {
    mochi_list_i64 **data;
    int64_t          len;
    int64_t          cap;
} mochi_list_list;

mochi_list_list *mochi_list_list_new(void);
int64_t mochi_list_list_len(const mochi_list_list *m);
void mochi_list_list_push(mochi_list_list *m, mochi_list_i64 *row);
mochi_list_i64 *mochi_list_list_get(const mochi_list_list *m, int64_t i);

/*
 * mochi_list_list_to_str renders m in the Mochi reference display
 * form `[[1, 2, 3], [4, 5, 6]]`: outer brackets, comma-space row
 * separators, each row rendered via mochi_list_i64_to_str. Empty
 * outer list returns a static "[]" literal (no malloc); non-empty
 * results are heap-allocated and leak at process exit (Phase 4 MVP,
 * parity with the i64 and f64 array formatters).
 */
const char *mochi_list_list_to_str(const mochi_list_list *m);

#ifdef __cplusplus
}
#endif

#endif /* MOCHI_RUNTIME_C_LIST_LIST_H */
