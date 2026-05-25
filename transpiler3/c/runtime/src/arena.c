/*
 * mochi_arena_t implementation.
 * MEP-45 Phase 8.3. See include/mochi/arena.h for API.
 */
#include "mochi/arena.h"
#include "mochi/errors.h"

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/* Allocate a new chunk with room for cap bytes after the header. */
static mochi_arena_chunk *arena_new_chunk(size_t cap) {
    mochi_arena_chunk *c = (mochi_arena_chunk *)malloc(sizeof(mochi_arena_chunk) + cap);
    if (c == NULL) mochi_panic_index();
    c->next = NULL;
    c->used = 0;
    c->cap  = cap;
    return c;
}

void mochi_arena_init(mochi_arena_t *a, size_t chunk_size) {
    if (chunk_size < MOCHI_ARENA_ALIGN) chunk_size = MOCHI_ARENA_ALIGN;
    a->chunk_size = chunk_size;
    a->head       = arena_new_chunk(chunk_size);
}

void *mochi_arena_alloc(mochi_arena_t *a, size_t size) {
    /* Round up to alignment. */
    size_t aligned = (size + (MOCHI_ARENA_ALIGN - 1)) & ~(size_t)(MOCHI_ARENA_ALIGN - 1);
    if (aligned == 0) aligned = MOCHI_ARENA_ALIGN;

    mochi_arena_chunk *c = a->head;
    if (c == NULL || c->used + aligned > c->cap) {
        /* Need a new chunk: large enough for this request. */
        size_t new_cap = a->chunk_size;
        if (aligned > new_cap) new_cap = aligned;
        mochi_arena_chunk *nc = arena_new_chunk(new_cap);
        nc->next = a->head;
        a->head  = nc;
        c        = nc;
    }

    void *ptr = (char *)c + sizeof(mochi_arena_chunk) + c->used;
    c->used  += aligned;
    return ptr;
}

void mochi_arena_free(mochi_arena_t *a) {
    mochi_arena_chunk *c = a->head;
    while (c != NULL) {
        mochi_arena_chunk *next = c->next;
        free(c);
        c = next;
    }
    a->head = NULL;
}
