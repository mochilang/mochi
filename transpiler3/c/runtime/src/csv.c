/*
 * libmochi: CSV I/O runtime implementation.
 *
 * MEP-45 Phase 8.4.
 */
#include "mochi/csv.h"
#include "mochi/errors.h"
#include <string.h>

/* mochi_csv_parse_line: RFC 4180 CSV line parser.
 *
 * Walks the input one character at a time. Two modes:
 *   unquoted: accumulate chars until comma or end; comma advances to next field.
 *   quoted: accumulate chars until closing double-quote;
 *           "" inside quotes is an escaped double-quote.
 *
 * Each cell is strdup'd; the list owns those pointers.
 */
mochi_list_str mochi_csv_parse_line(const char *line) {
    mochi_list_str result = {NULL, 0, 0};
    if (line == NULL || *line == '\0') {
        /* Empty line: one empty field. */
        const char *empty = "";
        return mochi_list_str_append(result, empty);
    }

    /* cell accumulation buffer */
    int64_t cap = 64;
    char *buf = (char *)malloc((size_t)cap);
    if (buf == NULL) { mochi_panic_index(); }
    int64_t len = 0;

#define FLUSH_CELL() do { \
        buf[len] = '\0'; \
        char *cell = (char *)malloc((size_t)(len + 1)); \
        if (cell == NULL) { mochi_panic_index(); } \
        memcpy(cell, buf, (size_t)(len + 1)); \
        result = mochi_list_str_append(result, cell); \
        len = 0; \
    } while (0)

#define PUSH(c) do { \
        if (len + 1 >= cap) { \
            cap *= 2; \
            char *nb = (char *)realloc(buf, (size_t)cap); \
            if (nb == NULL) { mochi_panic_index(); } \
            buf = nb; \
        } \
        buf[len++] = (char)(c); \
    } while (0)

    const char *p = line;
    while (1) {
        if (*p == '"') {
            /* Quoted field. */
            p++;
            while (1) {
                if (*p == '\0') {
                    /* Unterminated quoted field: treat as end. */
                    break;
                } else if (*p == '"') {
                    p++;
                    if (*p == '"') {
                        /* Escaped double-quote. */
                        PUSH('"');
                        p++;
                    } else {
                        /* End of quoted field. */
                        break;
                    }
                } else {
                    PUSH(*p);
                    p++;
                }
            }
            /* After closing quote, skip to next comma or end. */
            while (*p != '\0' && *p != ',') { p++; }
        } else {
            /* Unquoted field: read until comma or end. */
            while (*p != '\0' && *p != ',') {
                PUSH(*p);
                p++;
            }
        }
        FLUSH_CELL();
        if (*p == ',') {
            p++;
        } else {
            break;
        }
    }

    free(buf);
    return result;
}

/*
 * cell_needs_quoting: returns 1 if cell contains comma, double-quote, or newline.
 */
static int cell_needs_quoting(const char *cell) {
    for (const char *p = cell; *p; p++) {
        if (*p == ',' || *p == '"' || *p == '\n' || *p == '\r') {
            return 1;
        }
    }
    return 0;
}

/* mochi_csv_format_row: join cells with commas, quoting where needed. */
char *mochi_csv_format_row(mochi_list_str row) {
    /* First pass: compute total length. */
    int64_t total = 0;
    for (int64_t i = 0; i < row.len; i++) {
        const char *cell = row.data[i];
        if (cell_needs_quoting(cell)) {
            /* 2 quote chars + content length + count of internal '"' */
            total += 2;
            for (const char *p = cell; *p; p++) {
                total += (*p == '"') ? 2 : 1;
            }
        } else {
            total += (int64_t)strlen(cell);
        }
        if (i + 1 < row.len) { total++; } /* comma separator */
    }
    total++; /* NUL terminator */

    char *out = (char *)malloc((size_t)total);
    if (out == NULL) { mochi_panic_index(); }

    char *wp = out;
    for (int64_t i = 0; i < row.len; i++) {
        const char *cell = row.data[i];
        if (cell_needs_quoting(cell)) {
            *wp++ = '"';
            for (const char *p = cell; *p; p++) {
                if (*p == '"') { *wp++ = '"'; }
                *wp++ = *p;
            }
            *wp++ = '"';
        } else {
            size_t clen = strlen(cell);
            memcpy(wp, cell, clen);
            wp += clen;
        }
        if (i + 1 < row.len) { *wp++ = ','; }
    }
    *wp = '\0';
    return out;
}
