/*
 * libmochi: CSV I/O runtime declarations.
 *
 * MEP-45 Phase 8.4.
 *
 * Provides mochi_csv_parse_line and mochi_csv_format_row: the two
 * low-level helpers that operate on mochi_list_str (list<string>).
 * The TU-local __mochi_load_csv / __mochi_save_csv wrappers that
 * bridge to mochi_list_list_str are emitted by the code generator
 * when any loadCSV/saveCSV call is present in the program.
 *
 * CSV dialect: RFC 4180 subset.
 *   - Fields separated by commas.
 *   - Fields containing commas, double-quotes, or newlines are
 *     enclosed in double-quotes.
 *   - A double-quote inside a quoted field is escaped as "".
 *   - Trailing \r\n or \n stripped by mochi_lines() before parse.
 *
 * ABI stability: these symbols are part of libmochi's versioned
 * surface. Existing prototypes never change shape.
 */
#ifndef MOCHI_CSV_H
#define MOCHI_CSV_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "mochi/list.h"

/*
 * mochi_csv_parse_line(line) -- parse one CSV line (no trailing
 * newline) and return its cells as a list<string>. Each returned
 * cell string is malloc'd. Handles RFC 4180 quoting.
 */
mochi_list_str mochi_csv_parse_line(const char *line);

/*
 * mochi_csv_format_row(row) -- join the cells in row with commas,
 * applying RFC 4180 quoting when a cell contains a comma, double-
 * quote, or newline. Returns a malloc'd NUL-terminated string.
 * The caller owns the returned buffer.
 */
char *mochi_csv_format_row(mochi_list_str row);

#ifdef __cplusplus
}
#endif

#endif /* MOCHI_CSV_H */
