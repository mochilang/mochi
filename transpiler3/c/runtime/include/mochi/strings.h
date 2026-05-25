/*
 * libmochi: string operation declarations.
 *
 * MEP-45 Phase 6.0. mochi_str_cat concatenates two NUL-terminated C
 * strings and returns a freshly malloc'd result. The caller owns the
 * memory; in Phase 6.0 it is never freed (arena / GC deferred).
 */
#pragma once

#include <stdint.h>

/* mochi_str_cat(a, b) -- returns malloc'd a+b NUL-terminated string. */
const char *mochi_str_cat(const char *a, const char *b);
