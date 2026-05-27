/*
 * libmochi: JavaScript FFI via subprocess RPC. Phase 10.4.
 *
 * On POSIX: forks a node process, sets up bidirectional pipe via
 * pipe()+fork()+execvp(), serialises calls as JSON lines.
 * The companion JS file path is resolved via:
 *   1. MOCHI_JS_RPC_PATH env var (runtime override)
 *   2. MOCHI_JS_RPC_PATH_DEFAULT macro (baked in at build time)
 * On Windows/_WIN32 and __wasm__: stubs that abort on first call.
 */

/* fdopen() is POSIX, not pure C; expose it under musl + -std=c2x. */
#if !defined(_WIN32) && !defined(__wasm__) && !defined(_POSIX_C_SOURCE)
#  define _POSIX_C_SOURCE 200809L
#endif

#include "mochi/js_rpc.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if !defined(_WIN32) && !defined(__wasm__)

#include <unistd.h>
#include <sys/wait.h>
#include <inttypes.h>

static FILE *g_js_in  = NULL;
static FILE *g_js_out = NULL;

static void js_rpc_start(void) {
    const char *path = getenv("MOCHI_JS_RPC_PATH");
#ifdef MOCHI_JS_RPC_PATH_DEFAULT
    if (!path || !*path) path = MOCHI_JS_RPC_PATH_DEFAULT;
#endif
    if (!path || !*path) {
        fprintf(stderr, "js_rpc: MOCHI_JS_RPC_PATH not set; cannot start JS companion\n"); exit(1);
    }
    int to_js[2], from_js[2];
    if (pipe(to_js) != 0 || pipe(from_js) != 0) {
        fprintf(stderr, "js_rpc: pipe() failed\n"); exit(1);
    }
    pid_t pid = fork();
    if (pid < 0) {
        fprintf(stderr, "js_rpc: fork() failed\n"); exit(1);
    }
    if (pid == 0) {
        /* child: become node running the companion script */
        dup2(to_js[0], STDIN_FILENO);
        dup2(from_js[1], STDOUT_FILENO);
        close(to_js[0]);  close(to_js[1]);
        close(from_js[0]); close(from_js[1]);
        execlp("node", "node", path, (char *)NULL);
        _exit(127);
    }
    /* parent */
    close(to_js[0]);
    close(from_js[1]);
    g_js_in  = fdopen(to_js[1],  "w");
    g_js_out = fdopen(from_js[0], "r");
    if (!g_js_in || !g_js_out) {
        fprintf(stderr, "js_rpc: fdopen() failed\n"); exit(1);
    }
}

static void js_rpc_ensure(void) {
    if (!g_js_in) {
        js_rpc_start();
    }
}

const char *mochi_js_rpc_call(const char *request_json) {
    js_rpc_ensure();
    fprintf(g_js_in, "%s\n", request_json);
    fflush(g_js_in);
    static char resp[65536];
    if (!fgets(resp, sizeof(resp), g_js_out)) {
        return "{\"error\":\"js companion closed\"}";
    }
    return resp;
}

int64_t mochi_js_rpc_int(const char *resp) {
    const char *p = strstr(resp, "\"result\"");
    if (!p) return 0;
    p += 8;
    while (*p == ':' || *p == ' ') p++;
    return (int64_t)strtoll(p, NULL, 10);
}

double mochi_js_rpc_float(const char *resp) {
    const char *p = strstr(resp, "\"result\"");
    if (!p) return 0.0;
    p += 8;
    while (*p == ':' || *p == ' ') p++;
    return strtod(p, NULL);
}

const char *mochi_js_rpc_str(const char *resp) {
    static char buf[16384];
    const char *p = strstr(resp, "\"result\"");
    if (!p) return "";
    p += 8;
    while (*p == ':' || *p == ' ') p++;
    if (*p != '"') return "";
    p++;
    size_t i = 0;
    while (*p && *p != '"' && i < sizeof(buf) - 1) {
        if (*p == '\\' && *(p+1) == '"') { buf[i++] = '"'; p += 2; continue; }
        if (*p == '\\' && *(p+1) == 'n') { buf[i++] = '\n'; p += 2; continue; }
        buf[i++] = *p++;
    }
    buf[i] = '\0';
    return buf;
}

int mochi_js_rpc_bool(const char *resp) {
    const char *p = strstr(resp, "\"result\"");
    if (!p) return 0;
    p += 8;
    while (*p == ':' || *p == ' ') p++;
    return (*p == 't') ? 1 : 0;
}

#else /* _WIN32 or __wasm__: stubs */

const char *mochi_js_rpc_call(const char *req) {
    (void)req;
    fprintf(stderr, "js_rpc: JavaScript FFI not supported on this platform\n"); exit(1);
    return "{}";
}
int64_t     mochi_js_rpc_int(const char *r)   { (void)r; return 0; }
double      mochi_js_rpc_float(const char *r) { (void)r; return 0.0; }
const char *mochi_js_rpc_str(const char *r)   { (void)r; return ""; }
int         mochi_js_rpc_bool(const char *r)  { (void)r; return 0; }

#endif
