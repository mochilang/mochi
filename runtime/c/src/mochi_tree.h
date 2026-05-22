/*
 * MEP-42 Phase 4.3.15.1 list<any> runtime.
 *
 * Backs Mochi's `list<any>` when the program is compiled with
 * `mochi build --target=c`. Every payload is itself a `mochi_tree*`,
 * so binary_trees-style kernels (nodes encoded as `[]` or
 * `[left, right]`) lower to a single recursive struct without a
 * Cell-tag indirection or per-element variant tag.
 *
 * Identity: pure C99, no libc beyond stdlib.h/string.h/stdint.h.
 * Allocations are heap-resident and leak at process exit; the AOT
 * target's MVP does not run finalisers between builds. Growth
 * doubles on overflow, mirroring mochi_list_i64.
 */
#ifndef MOCHI_RUNTIME_C_TREE_H
#define MOCHI_RUNTIME_C_TREE_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct mochi_tree {
    struct mochi_tree **children;
    int64_t             len;
    int64_t             cap;
} mochi_tree;

mochi_tree *mochi_tree_new(void);
int64_t     mochi_tree_len(const mochi_tree *t);
void        mochi_tree_push(mochi_tree *t, mochi_tree *child);
mochi_tree *mochi_tree_get(const mochi_tree *t, int64_t i);

#ifdef __cplusplus
}
#endif

#endif /* MOCHI_RUNTIME_C_TREE_H */
