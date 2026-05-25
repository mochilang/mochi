/*
 * libmochi: file I/O runtime implementation.
 *
 * MEP-45 Phase 6.5. See website/docs/mep/mep-0045.md.
 * MEP-45 Phase 6.6: UTF-8 validation on read via mochi_utf8_valid.
 *
 * All returned buffers are heap-allocated and never freed (leak on
 * program exit). Deallocation is deferred to Phase 17 GC integration.
 *
 * Error handling: file-open failures call mochi_panic_index() which
 * writes a diagnostic to stderr and exits with MOCHI_ERR_INDEX (4).
 * Invalid UTF-8 file content calls mochi_panic_parse() which exits
 * with MOCHI_ERR_PARSE (2).
 */

#include "mochi/fileio.h"
#include "mochi/errors.h"
#include "mochi/strings.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

const char *mochi_read_file(const char *path) {
    FILE *f = fopen(path, "r");
    if (f == NULL) {
        mochi_panic_index();
    }
    if (fseek(f, 0, SEEK_END) != 0) {
        fclose(f);
        mochi_panic_index();
    }
    long size = ftell(f);
    if (size < 0) {
        fclose(f);
        mochi_panic_index();
    }
    rewind(f);
    char *buf = (char *)malloc((size_t)size + 1);
    if (buf == NULL) {
        fclose(f);
        mochi_panic_index();
    }
    size_t nread = fread(buf, 1, (size_t)size, f);
    buf[nread] = '\0';
    fclose(f);
    /* Phase 6.6: reject files with invalid UTF-8 byte sequences. */
    if (!mochi_utf8_valid(buf, nread)) {
        mochi_panic_parse();
    }
    return buf;
}

void mochi_write_file(const char *path, const char *content) {
    FILE *f = fopen(path, "w");
    if (f == NULL) {
        mochi_panic_index();
    }
    if (fputs(content, f) == EOF) {
        fclose(f);
        mochi_panic_index();
    }
    fclose(f);
}

void mochi_append_file(const char *path, const char *content) {
    FILE *f = fopen(path, "a");
    if (f == NULL) {
        mochi_panic_index();
    }
    if (fputs(content, f) == EOF) {
        fclose(f);
        mochi_panic_index();
    }
    fclose(f);
}

mochi_list_str mochi_lines(const char *path) {
    const char *content = mochi_read_file(path);
    size_t len = strlen(content);

    /* Count lines: each '\n' ends a line; content with no trailing '\n'
     * still has one last line if it is non-empty. */
    size_t nlines = 0;
    for (size_t i = 0; i < len; i++) {
        if (content[i] == '\n') {
            nlines++;
        }
    }
    /* If the content does not end with '\n', there is one more line. */
    if (len > 0 && content[len - 1] != '\n') {
        nlines++;
    }

    if (nlines == 0) {
        mochi_list_str empty;
        empty.data = NULL;
        empty.len  = 0;
        empty.cap  = 0;
        return empty;
    }

    const char **lines = (const char **)malloc(nlines * sizeof(const char *));
    if (lines == NULL) {
        mochi_panic_index();
    }

    size_t li   = 0; /* line index */
    size_t start = 0;
    for (size_t i = 0; i <= len; i++) {
        if (i == len || content[i] == '\n') {
            size_t end = i;
            size_t llen = end - start;
            char *line = (char *)malloc(llen + 1);
            if (line == NULL) {
                mochi_panic_index();
            }
            memcpy(line, content + start, llen);
            line[llen] = '\0';
            lines[li++] = line;
            start = i + 1;
            /* Stop after recording the last non-empty line; if i==len and
             * content ended with '\n', li already equals nlines and we
             * must not run off. */
            if (li == nlines) {
                break;
            }
        }
    }

    mochi_list_str result;
    result.data = lines;
    result.len  = (int64_t)nlines;
    result.cap  = (int64_t)nlines;
    return result;
}
