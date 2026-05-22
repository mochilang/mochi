/*
 * MEP-42 Phase 4.3.15.1 list<any> runtime. See mochi_tree.h.
 */
#include "mochi_tree.h"

#include <stdint.h>
#include <stdlib.h>

mochi_tree *mochi_tree_new(void) {
    mochi_tree *t = (mochi_tree *)malloc(sizeof(mochi_tree));
    if (t == NULL) {
        abort();
    }
    t->children = NULL;
    t->len = 0;
    t->cap = 0;
    return t;
}

int64_t mochi_tree_len(const mochi_tree *t) {
    return t->len;
}

void mochi_tree_push(mochi_tree *t, mochi_tree *child) {
    if (t->len == t->cap) {
        int64_t newcap = t->cap == 0 ? 4 : t->cap * 2;
        mochi_tree **nd = (mochi_tree **)realloc(t->children, (size_t)newcap * sizeof(mochi_tree *));
        if (nd == NULL) {
            abort();
        }
        t->children = nd;
        t->cap = newcap;
    }
    t->children[t->len++] = child;
}

mochi_tree *mochi_tree_get(const mochi_tree *t, int64_t i) {
    return t->children[i];
}
