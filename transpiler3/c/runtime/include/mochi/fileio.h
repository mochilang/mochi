/*
 * libmochi: file I/O runtime declarations.
 *
 * MEP-45 Phase 6.5.
 *
 * Provides readFile, writeFile, appendFile, and lines for Mochi's
 * file I/O builtins. All returned strings and list buffers are
 * freshly malloc'd; deallocation is deferred to Phase 17 GC.
 *
 * Error handling: file-open failures call mochi_panic_index() which
 * writes a diagnostic to stderr and exits with MOCHI_ERR_INDEX (4).
 *
 * ABI stability: every symbol here is part of libmochi's versioned
 * surface. Existing prototypes never change shape.
 */
#ifndef MOCHI_FILEIO_H
#define MOCHI_FILEIO_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include "mochi/list.h"
#include "mochi/errors.h"

/*
 * mochi_read_file(path) -- read the entire contents of a file and
 * return them as a NUL-terminated C string. The caller owns the
 * returned buffer (malloc'd). Panics if the file cannot be opened.
 */
const char *mochi_read_file(const char *path);

/*
 * mochi_write_file(path, content) -- write content to a file,
 * creating it if it does not exist or truncating it if it does.
 * Panics if the file cannot be opened for writing.
 */
void mochi_write_file(const char *path, const char *content);

/*
 * mochi_append_file(path, content) -- append content to a file,
 * creating it if it does not exist.
 * Panics if the file cannot be opened for appending.
 */
void mochi_append_file(const char *path, const char *content);

/*
 * mochi_lines(path) -- read a file and return each line as an element
 * of a list<string>. The trailing newline on each line is stripped.
 * If the file ends with a newline, no empty trailing element is added.
 * Panics if the file cannot be opened.
 */
mochi_list_str mochi_lines(const char *path);

#ifdef __cplusplus
}
#endif

#endif /* MOCHI_FILEIO_H */
