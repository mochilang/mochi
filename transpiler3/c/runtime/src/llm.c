/*
 * libmochi: LLM generation implementation.
 *
 * MEP-45 Phase 14.0: cassette-backed mochi_llm_generate().
 *
 * Cassette mode (MOCHI_LLM_CASSETTE_DIR is set):
 *   Looks up a pre-recorded response file named by the DJB2 hash of the
 *   concatenated key "<provider>\0<model>\0<prompt>" in the cassette
 *   directory. File name format: "<hash_decimal>.txt".
 *
 * Live mode (no cassette dir):
 *   Returns "" and prints a diagnostic. Concrete HTTP providers land in
 *   Phase 14.1 (OpenAI), 14.2 (Anthropic), 14.3 (Google), 14.4 (llama.cpp).
 */
#include "mochi/llm.h"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* DJB2 hash over arbitrary bytes. Separates fields with a NUL byte so that
 * ("a", "bc") hashes differently from ("ab", "c"). */
static uint64_t llm_hash_key(const char *provider, const char *model, const char *prompt) {
    uint64_t h = 5381;
    for (const char *p = provider; *p; p++) h = h * 33 ^ (unsigned char)*p;
    h = h * 33 ^ 0;
    for (const char *p = model;    *p; p++) h = h * 33 ^ (unsigned char)*p;
    h = h * 33 ^ 0;
    for (const char *p = prompt;   *p; p++) h = h * 33 ^ (unsigned char)*p;
    return h;
}

const char *mochi_llm_generate(const char *provider, const char *model, const char *prompt) {
    const char *cassette_dir = getenv("MOCHI_LLM_CASSETTE_DIR");
    if (cassette_dir && *cassette_dir) {
        uint64_t key = llm_hash_key(provider, model, prompt);
        char path[4096];
        snprintf(path, sizeof(path), "%s/%llu.txt", cassette_dir, (unsigned long long)key);

        FILE *f = fopen(path, "rb");
        if (!f) {
            fprintf(stderr, "mochi_llm_generate: cassette not found: %s\n", path);
            return "";
        }
        fseek(f, 0, SEEK_END);
        long sz = ftell(f);
        fseek(f, 0, SEEK_SET);
        if (sz < 0) {
            fclose(f);
            fprintf(stderr, "mochi_llm_generate: ftell failed for %s\n", path);
            return "";
        }
        char *buf = (char *)malloc((size_t)sz + 1);
        if (!buf) {
            fclose(f);
            fprintf(stderr, "mochi_llm_generate: out of memory\n");
            return "";
        }
        size_t nread = fread(buf, 1, (size_t)sz, f);
        fclose(f);
        buf[nread] = '\0';
        /* Strip a single trailing newline so cassette files can be written
         * with a trailing newline (normal for text editors) without changing
         * the effective response. */
        if (nread > 0 && buf[nread - 1] == '\n') {
            buf[nread - 1] = '\0';
        }
        return buf;
    }

    /* Live mode: concrete provider not implemented in Phase 14.0. */
    fprintf(stderr,
            "mochi_llm_generate: live mode not implemented; "
            "set MOCHI_LLM_CASSETTE_DIR for cassette replay\n");
    return "";
}
