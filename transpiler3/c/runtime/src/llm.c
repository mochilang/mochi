/*
 * libmochi: LLM generation implementation.
 *
 * MEP-45 Phase 14.0: cassette-backed mochi_llm_generate().
 * MEP-45 Phase 14.1: OpenAI live provider via libcurl (opt-in).
 *
 * Cassette mode (MOCHI_LLM_CASSETTE_DIR is set):
 *   Looks up a pre-recorded response file named by the DJB2 hash of the
 *   concatenated key "<provider>\0<model>\0<prompt>" in the cassette
 *   directory. File name format: "<hash_decimal>.txt".
 *
 * Live mode (no cassette dir):
 *   Routes to a provider-specific HTTP implementation when compiled with
 *   MOCHI_LLM_HAVE_CURL (adds libcurl dependency). Without that flag,
 *   returns "" with a diagnostic. Enable by compiling with:
 *     -DMOCHI_LLM_HAVE_CURL -lcurl
 *   and setting OPENAI_API_KEY (or provider-specific key) at runtime.
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

/* ---- cassette lookup ---- */

static const char *llm_cassette_lookup(const char *cassette_dir,
                                        const char *provider,
                                        const char *model,
                                        const char *prompt) {
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

/* ---- OpenAI live provider (Phase 14.1) ---- */

#if defined(MOCHI_LLM_HAVE_CURL)
#include <curl/curl.h>

/* Growable response buffer for curl write callback. */
typedef struct {
    char  *data;
    size_t len;
    size_t cap;
} llm_buf_t;

static size_t llm_curl_write(char *ptr, size_t size, size_t nmemb, void *userdata) {
    llm_buf_t *b = (llm_buf_t *)userdata;
    size_t n = size * nmemb;
    if (b->len + n + 1 > b->cap) {
        size_t newcap = b->cap == 0 ? 4096 : b->cap * 2;
        while (newcap < b->len + n + 1) newcap *= 2;
        char *newdata = (char *)realloc(b->data, newcap);
        if (!newdata) return 0; /* signal write error */
        b->data = newdata;
        b->cap = newcap;
    }
    memcpy(b->data + b->len, ptr, n);
    b->len += n;
    b->data[b->len] = '\0';
    return n;
}

/* Minimal JSON string extractor: finds the value of the first occurrence of
 * "key": "VALUE" in json and returns a malloc'd copy of VALUE. Returns NULL
 * on failure. Does not handle nested objects beyond one level of depth. */
static char *llm_json_str(const char *json, const char *key) {
    char needle[256];
    snprintf(needle, sizeof(needle), "\"%s\":", key);
    const char *p = strstr(json, needle);
    if (!p) return NULL;
    p += strlen(needle);
    while (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r') p++;
    if (*p != '"') return NULL;
    p++; /* skip opening quote */
    /* collect characters, handling \" escapes */
    size_t cap = 256, len = 0;
    char *result = (char *)malloc(cap);
    if (!result) return NULL;
    while (*p && *p != '"') {
        if (*p == '\\' && *(p + 1)) {
            p++;
            char c;
            switch (*p) {
            case '"':  c = '"';  break;
            case '\\': c = '\\'; break;
            case '/':  c = '/';  break;
            case 'n':  c = '\n'; break;
            case 'r':  c = '\r'; break;
            case 't':  c = '\t'; break;
            default:   c = *p;   break;
            }
            if (len + 2 > cap) { cap *= 2; result = (char *)realloc(result, cap); if (!result) return NULL; }
            result[len++] = c;
            p++;
        } else {
            if (len + 2 > cap) { cap *= 2; result = (char *)realloc(result, cap); if (!result) return NULL; }
            result[len++] = *p++;
        }
    }
    result[len] = '\0';
    return result;
}

/* Build the OpenAI chat completions JSON body. */
static char *llm_openai_build_body(const char *model_or_default, const char *prompt) {
    const char *model = (model_or_default && *model_or_default) ? model_or_default : "gpt-4o-mini";
    /* Escape the prompt for JSON embedding (handles \, ", newlines). */
    size_t prompt_len = strlen(prompt);
    char *escaped = (char *)malloc(prompt_len * 6 + 1);
    if (!escaped) return NULL;
    size_t j = 0;
    for (size_t i = 0; i < prompt_len; i++) {
        unsigned char c = (unsigned char)prompt[i];
        if (c == '"')       { escaped[j++] = '\\'; escaped[j++] = '"'; }
        else if (c == '\\') { escaped[j++] = '\\'; escaped[j++] = '\\'; }
        else if (c == '\n') { escaped[j++] = '\\'; escaped[j++] = 'n'; }
        else if (c == '\r') { escaped[j++] = '\\'; escaped[j++] = 'r'; }
        else if (c == '\t') { escaped[j++] = '\\'; escaped[j++] = 't'; }
        else { escaped[j++] = (char)c; }
    }
    escaped[j] = '\0';

    /* Estimate body size: model + escaped prompt + template. */
    size_t body_cap = strlen(model) + j + 256;
    char *body = (char *)malloc(body_cap);
    if (!body) { free(escaped); return NULL; }
    snprintf(body, body_cap,
             "{\"model\":\"%s\","
             "\"messages\":[{\"role\":\"user\",\"content\":\"%s\"}]}",
             model, escaped);
    free(escaped);
    return body;
}

/* Call OpenAI chat completions API and return the assistant text. */
static const char *llm_openai_live(const char *model, const char *prompt,
                                    const char *api_key) {
    CURL *curl = curl_easy_init();
    if (!curl) {
        fprintf(stderr, "mochi_llm_generate: curl_easy_init failed\n");
        return "";
    }

    char *body = llm_openai_build_body(model, prompt);
    if (!body) {
        curl_easy_cleanup(curl);
        return "";
    }

    /* Authorization header. */
    char auth_header[1024];
    snprintf(auth_header, sizeof(auth_header), "Authorization: Bearer %s", api_key);

    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = curl_slist_append(headers, auth_header);

    llm_buf_t resp = {NULL, 0, 0};

    curl_easy_setopt(curl, CURLOPT_URL, "https://api.openai.com/v1/chat/completions");
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, body);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, llm_curl_write);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &resp);
    curl_easy_setopt(curl, CURLOPT_TIMEOUT, 30L);

    CURLcode rc = curl_easy_perform(curl);
    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);
    free(body);

    if (rc != CURLE_OK) {
        fprintf(stderr, "mochi_llm_generate: curl error: %s\n", curl_easy_strerror(rc));
        if (resp.data) free(resp.data);
        return "";
    }

    if (!resp.data) return "";

    /* Extract choices[0].message.content.
     * Strategy: find "content" after "message" in the response JSON. */
    const char *msg_pos = strstr(resp.data, "\"message\"");
    if (!msg_pos) {
        fprintf(stderr, "mochi_llm_generate: no 'message' key in OpenAI response: %.200s\n", resp.data);
        free(resp.data);
        return "";
    }
    char *content = llm_json_str(msg_pos, "content");
    free(resp.data);
    if (!content) {
        fprintf(stderr, "mochi_llm_generate: failed to extract content from OpenAI response\n");
        return "";
    }
    return content; /* caller does not free (GC-less model) */
}

/* ---- Anthropic live provider (Phase 14.2) ---- */

/* Build the Anthropic messages API JSON body. */
static char *llm_anthropic_build_body(const char *model, const char *prompt) {
    size_t prompt_len = strlen(prompt);
    char *escaped = (char *)malloc(prompt_len * 6 + 1);
    if (!escaped) return NULL;
    size_t j = 0;
    for (size_t i = 0; i < prompt_len; i++) {
        unsigned char c = (unsigned char)prompt[i];
        if (c == '"')       { escaped[j++] = '\\'; escaped[j++] = '"'; }
        else if (c == '\\') { escaped[j++] = '\\'; escaped[j++] = '\\'; }
        else if (c == '\n') { escaped[j++] = '\\'; escaped[j++] = 'n'; }
        else if (c == '\r') { escaped[j++] = '\\'; escaped[j++] = 'r'; }
        else if (c == '\t') { escaped[j++] = '\\'; escaped[j++] = 't'; }
        else { escaped[j++] = (char)c; }
    }
    escaped[j] = '\0';

    size_t body_cap = strlen(model) + j + 256;
    char *body = (char *)malloc(body_cap);
    if (!body) { free(escaped); return NULL; }
    snprintf(body, body_cap,
             "{\"model\":\"%s\","
             "\"max_tokens\":1024,"
             "\"messages\":[{\"role\":\"user\",\"content\":\"%s\"}]}",
             model, escaped);
    free(escaped);
    return body;
}

/* Call Anthropic messages API and return the assistant text. */
static const char *llm_anthropic_live(const char *model, const char *prompt,
                                       const char *api_key) {
    CURL *curl = curl_easy_init();
    if (!curl) {
        fprintf(stderr, "mochi_llm_generate: curl_easy_init failed\n");
        return "";
    }

    const char *m = (model && *model) ? model : "claude-3-haiku-20240307";
    char *body = llm_anthropic_build_body(m, prompt);
    if (!body) {
        curl_easy_cleanup(curl);
        return "";
    }

    char auth_header[1024];
    snprintf(auth_header, sizeof(auth_header), "x-api-key: %s", api_key);

    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = curl_slist_append(headers, auth_header);
    headers = curl_slist_append(headers, "anthropic-version: 2023-06-01");

    llm_buf_t resp = {NULL, 0, 0};

    curl_easy_setopt(curl, CURLOPT_URL, "https://api.anthropic.com/v1/messages");
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, body);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, llm_curl_write);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &resp);
    curl_easy_setopt(curl, CURLOPT_TIMEOUT, 30L);

    CURLcode rc = curl_easy_perform(curl);
    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);
    free(body);

    if (rc != CURLE_OK) {
        fprintf(stderr, "mochi_llm_generate: curl error: %s\n", curl_easy_strerror(rc));
        if (resp.data) free(resp.data);
        return "";
    }

    if (!resp.data) return "";

    /* Extract content[0].text from Anthropic response JSON.
     * Strategy: find "content" array then "text" field within it. */
    const char *content_pos = strstr(resp.data, "\"content\"");
    if (!content_pos) {
        fprintf(stderr, "mochi_llm_generate: no 'content' key in Anthropic response: %.200s\n", resp.data);
        free(resp.data);
        return "";
    }
    char *text = llm_json_str(content_pos, "text");
    free(resp.data);
    if (!text) {
        fprintf(stderr, "mochi_llm_generate: failed to extract text from Anthropic response\n");
        return "";
    }
    return text;
}

/* ---- Google live provider (Phase 14.3) ---- */

/* Build the Google Generative Language API JSON body. */
static char *llm_google_build_body(const char *prompt) {
    size_t prompt_len = strlen(prompt);
    char *escaped = (char *)malloc(prompt_len * 6 + 1);
    if (!escaped) return NULL;
    size_t j = 0;
    for (size_t i = 0; i < prompt_len; i++) {
        unsigned char c = (unsigned char)prompt[i];
        if (c == '"')       { escaped[j++] = '\\'; escaped[j++] = '"'; }
        else if (c == '\\') { escaped[j++] = '\\'; escaped[j++] = '\\'; }
        else if (c == '\n') { escaped[j++] = '\\'; escaped[j++] = 'n'; }
        else if (c == '\r') { escaped[j++] = '\\'; escaped[j++] = 'r'; }
        else if (c == '\t') { escaped[j++] = '\\'; escaped[j++] = 't'; }
        else { escaped[j++] = (char)c; }
    }
    escaped[j] = '\0';

    size_t body_cap = j + 128;
    char *body = (char *)malloc(body_cap);
    if (!body) { free(escaped); return NULL; }
    snprintf(body, body_cap,
             "{\"contents\":[{\"parts\":[{\"text\":\"%s\"}]}]}",
             escaped);
    free(escaped);
    return body;
}

/* Call Google Generative Language API and return the generated text.
 * API key goes in the URL query parameter (not a header). */
static const char *llm_google_live(const char *model, const char *prompt,
                                    const char *api_key) {
    CURL *curl = curl_easy_init();
    if (!curl) {
        fprintf(stderr, "mochi_llm_generate: curl_easy_init failed\n");
        return "";
    }

    const char *m = (model && *model) ? model : "gemini-1.5-flash";
    char *body = llm_google_build_body(prompt);
    if (!body) {
        curl_easy_cleanup(curl);
        return "";
    }

    /* URL: https://generativelanguage.googleapis.com/v1beta/models/{model}:generateContent?key={key} */
    char url[2048];
    snprintf(url, sizeof(url),
             "https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent?key=%s",
             m, api_key);

    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json");

    llm_buf_t resp = {NULL, 0, 0};

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, body);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, llm_curl_write);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &resp);
    curl_easy_setopt(curl, CURLOPT_TIMEOUT, 30L);

    CURLcode rc = curl_easy_perform(curl);
    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);
    free(body);

    if (rc != CURLE_OK) {
        fprintf(stderr, "mochi_llm_generate: curl error: %s\n", curl_easy_strerror(rc));
        if (resp.data) free(resp.data);
        return "";
    }

    if (!resp.data) return "";

    /* Extract candidates[0].content.parts[0].text from Google response JSON.
     * Strategy: find "candidates" then "text" within it. */
    const char *cand_pos = strstr(resp.data, "\"candidates\"");
    if (!cand_pos) {
        fprintf(stderr, "mochi_llm_generate: no 'candidates' key in Google response: %.200s\n", resp.data);
        free(resp.data);
        return "";
    }
    char *text = llm_json_str(cand_pos, "text");
    free(resp.data);
    if (!text) {
        fprintf(stderr, "mochi_llm_generate: failed to extract text from Google response\n");
        return "";
    }
    return text;
}
#endif /* MOCHI_LLM_HAVE_CURL */

/* ---- llama.cpp local provider (Phase 14.4) ---- */

#if defined(MOCHI_LLM_HAVE_LLAMA)
#include <llama.h>

/* Greedy argmax over the logit vector. */
static llama_token llm_llama_greedy(struct llama_context *ctx) {
    float *logits = llama_get_logits(ctx);
    int n_vocab = llama_n_vocab(llama_get_model(ctx));
    llama_token best = 0;
    float best_val = logits[0];
    for (int i = 1; i < n_vocab; i++) {
        if (logits[i] > best_val) { best_val = logits[i]; best = i; }
    }
    return best;
}

/* Run local llama.cpp inference from a GGUF model file.
 * Loads the model once per call (no persistent state across calls).
 * CPU-only (n_gpu_layers=0) for portability; users can override via
 * llama.cpp environment variables. */
static const char *llm_llama_local(const char *model_path, const char *prompt) {
    llama_backend_init();

    llama_model_params mparams = llama_model_default_params();
    mparams.n_gpu_layers = 0;

    struct llama_model *model = llama_load_model_from_file(model_path, mparams);
    if (!model) {
        fprintf(stderr, "mochi_llm_generate: failed to load llama model: %s\n", model_path);
        return "";
    }

    llama_context_params cparams = llama_context_default_params();
    cparams.n_ctx = 2048;

    struct llama_context *ctx = llama_new_context_with_model(model, cparams);
    if (!ctx) {
        fprintf(stderr, "mochi_llm_generate: failed to create llama context\n");
        llama_free_model(model);
        return "";
    }

    /* Tokenize the prompt. */
    int prompt_len = (int)strlen(prompt);
    int n_tokens_max = prompt_len + 32;
    llama_token *tokens = (llama_token *)malloc(n_tokens_max * sizeof(llama_token));
    if (!tokens) {
        llama_free(ctx); llama_free_model(model);
        return "";
    }
    int n_tokens = llama_tokenize(model, prompt, prompt_len, tokens, n_tokens_max,
                                  /*add_special=*/true, /*parse_special=*/false);
    if (n_tokens < 0) {
        fprintf(stderr, "mochi_llm_generate: llama_tokenize failed (buffer too small?)\n");
        free(tokens); llama_free(ctx); llama_free_model(model);
        return "";
    }

    /* Decode the prompt batch. */
    llama_batch batch = llama_batch_get_one(tokens, n_tokens);
    if (llama_decode(ctx, batch) != 0) {
        fprintf(stderr, "mochi_llm_generate: llama_decode (prompt) failed\n");
        free(tokens); llama_free(ctx); llama_free_model(model);
        return "";
    }
    free(tokens);

    /* Greedy token generation loop. */
    size_t out_cap = 4096, out_len = 0;
    char *out = (char *)malloc(out_cap);
    if (!out) { llama_free(ctx); llama_free_model(model); return ""; }
    out[0] = '\0';

    char piece[256];
    llama_token eos = llama_token_eos(model);
    int max_new = 512;
    for (int i = 0; i < max_new; i++) {
        llama_token tok = llm_llama_greedy(ctx);
        if (tok == eos) break;

        int piece_len = llama_token_to_piece(model, tok, piece, (int)sizeof(piece) - 1, 0, false);
        if (piece_len <= 0) break;
        piece[piece_len] = '\0';

        if (out_len + (size_t)piece_len + 1 > out_cap) {
            out_cap *= 2;
            char *tmp = (char *)realloc(out, out_cap);
            if (!tmp) break;
            out = tmp;
        }
        memcpy(out + out_len, piece, (size_t)piece_len);
        out_len += (size_t)piece_len;
        out[out_len] = '\0';

        /* Feed the generated token back. */
        batch = llama_batch_get_one(&tok, 1);
        if (llama_decode(ctx, batch) != 0) break;
    }

    llama_free(ctx);
    llama_free_model(model);
    llama_backend_free();
    return out;
}
#endif /* MOCHI_LLM_HAVE_LLAMA */

/* ---- provider dispatch ---- */

static const char *llm_live_dispatch(const char *provider, const char *model, const char *prompt) {
#if defined(MOCHI_LLM_HAVE_CURL)
    if (strcmp(provider, "openai") == 0) {
        const char *api_key = getenv("OPENAI_API_KEY");
        if (!api_key || !*api_key) {
            fprintf(stderr,
                    "mochi_llm_generate: OPENAI_API_KEY not set "
                    "(provider=openai, live mode requires an API key)\n");
            return "";
        }
        return llm_openai_live(model, prompt, api_key);
    }
    if (strcmp(provider, "anthropic") == 0) {
        const char *api_key = getenv("ANTHROPIC_API_KEY");
        if (!api_key || !*api_key) {
            fprintf(stderr,
                    "mochi_llm_generate: ANTHROPIC_API_KEY not set "
                    "(provider=anthropic, live mode requires an API key)\n");
            return "";
        }
        return llm_anthropic_live(model, prompt, api_key);
    }
    if (strcmp(provider, "google") == 0) {
        const char *api_key = getenv("GOOGLE_API_KEY");
        if (!api_key || !*api_key) {
            fprintf(stderr,
                    "mochi_llm_generate: GOOGLE_API_KEY not set "
                    "(provider=google, live mode requires an API key)\n");
            return "";
        }
        return llm_google_live(model, prompt, api_key);
    }
#endif /* MOCHI_LLM_HAVE_CURL */

#if defined(MOCHI_LLM_HAVE_LLAMA)
    if (strcmp(provider, "llama") == 0) {
        const char *model_path = getenv("LLAMA_MODEL_PATH");
        if (!model_path || !*model_path) {
            fprintf(stderr,
                    "mochi_llm_generate: LLAMA_MODEL_PATH not set "
                    "(provider=llama, live mode requires a local GGUF model path)\n");
            return "";
        }
        return llm_llama_local(model_path, prompt);
    }
#endif /* MOCHI_LLM_HAVE_LLAMA */

    (void)provider; (void)model; (void)prompt;
    fprintf(stderr,
            "mochi_llm_generate: live mode for provider=%s not implemented; "
            "set MOCHI_LLM_CASSETTE_DIR for cassette replay, or compile with "
            "-DMOCHI_LLM_HAVE_CURL -lcurl and GOOGLE_API_KEY / ANTHROPIC_API_KEY / OPENAI_API_KEY, "
            "or --with-llama and LLAMA_MODEL_PATH for local inference\n",
            provider);
    return "";
}

/* ---- public API ---- */

const char *mochi_llm_generate(const char *provider, const char *model, const char *prompt) {
    const char *cassette_dir = getenv("MOCHI_LLM_CASSETTE_DIR");
    if (cassette_dir && *cassette_dir) {
        return llm_cassette_lookup(cassette_dir, provider, model, prompt);
    }
    return llm_live_dispatch(provider, model, prompt);
}
