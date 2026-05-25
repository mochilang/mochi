/*
 * mochi_arena_t: bump allocator for query-scope intermediates.
 *
 * MEP-45 Phase 8.3. Intermediates produced during a `from/where/select`
 * query live on the arena and are freed in bulk at the query boundary.
 * The surviving result list is copied to the heap before the arena is
 * released.
 *
 * Design: linked list of fixed-size chunks. Allocations bump a cursor
 * forward within the current chunk; a new chunk is allocated when the
 * current one overflows. Alignment is always MOCHI_ARENA_ALIGN (8 bytes).
 * The arena is never thread-shared (one per query invocation, stack-local
 * in the emitted C); no locking is needed.
 */
#pragma once
#include <stddef.h>

#define MOCHI_ARENA_DEFAULT_CHUNK (65536)
#define MOCHI_ARENA_ALIGN         (8)

typedef struct mochi_arena_chunk {
    struct mochi_arena_chunk *next;
    size_t used;
    size_t cap;
    /* data[] follows immediately in memory */
} mochi_arena_chunk;

typedef struct {
    mochi_arena_chunk *head; /* current (most recently allocated) chunk */
    size_t chunk_size;       /* size of each new chunk (bytes, excl header) */
} mochi_arena_t;

/* Initialise arena with given chunk size (use MOCHI_ARENA_DEFAULT_CHUNK). */
void mochi_arena_init(mochi_arena_t *a, size_t chunk_size);

/* Allocate size bytes from the arena. Always aligned to MOCHI_ARENA_ALIGN.
 * Panics (calls mochi_panic_index) if malloc fails. Never returns NULL. */
void *mochi_arena_alloc(mochi_arena_t *a, size_t size);

/* Release all chunks. After this call, any pointer returned by mochi_arena_alloc
 * is invalid. The arena struct itself may be reused after another mochi_arena_init. */
void mochi_arena_free(mochi_arena_t *a);
