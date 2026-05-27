/*
 * libmochi: Python FFI via subprocess RPC. Phase 10.3.
 *
 * On POSIX: forks a python3 process, sets up bidirectional pipe via
 * pipe()+fork()+execvp(), serialises calls as JSON lines.
 * The companion Python script path is resolved via:
 *   1. MOCHI_PYTHON_RPC_PATH env var (runtime override)
 *   2. MOCHI_PYTHON_RPC_PATH_DEFAULT macro (baked in at build time)
 * On Windows/_WIN32 and __wasm__: stubs that abort on first call.
 */

/* fdopen() is POSIX, not pure C; expose it under musl + -std=c2x. */
#if !defined(_WIN32) && !defined(__wasm__) && !defined(_POSIX_C_SOURCE)
#  define _POSIX_C_SOURCE 200809L
#endif

#include "mochi/python_rpc.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if !defined(_WIN32) && !defined(__wasm__)

#include <unistd.h>
#include <sys/wait.h>
#include <inttypes.h>

static FILE *g_py_in  = NULL;
static FILE *g_py_out = NULL;

static void py_rpc_start(void) {
    const char *path = getenv("MOCHI_PYTHON_RPC_PATH");
#ifdef MOCHI_PYTHON_RPC_PATH_DEFAULT
    if (!path || !*path) path = MOCHI_PYTHON_RPC_PATH_DEFAULT;
#endif
    if (!path || !*path) {
        fprintf(stderr, "python_rpc: MOCHI_PYTHON_RPC_PATH not set; cannot start Python companion\n"); exit(1);
    }
    int to_py[2], from_py[2];
    if (pipe(to_py) != 0 || pipe(from_py) != 0) {
        fprintf(stderr, "python_rpc: pipe() failed\n"); exit(1);
    }
    pid_t pid = fork();
    if (pid < 0) {
        fprintf(stderr, "python_rpc: fork() failed\n"); exit(1);
    }
    if (pid == 0) {
        /* child: become python3 running the companion script */
        dup2(to_py[0], STDIN_FILENO);
        dup2(from_py[1], STDOUT_FILENO);
        close(to_py[0]);  close(to_py[1]);
        close(from_py[0]); close(from_py[1]);
        execlp("python3", "python3", path, (char *)NULL);
        /* fallback to python if python3 not found */
        execlp("python", "python", path, (char *)NULL);
        _exit(127);
    }
    /* parent */
    close(to_py[0]);
    close(from_py[1]);
    g_py_in  = fdopen(to_py[1],  "w");
    g_py_out = fdopen(from_py[0], "r");
    if (!g_py_in || !g_py_out) {
        fprintf(stderr, "python_rpc: fdopen() failed\n"); exit(1);
    }
}

static void py_rpc_ensure(void) {
    if (!g_py_in) {
        py_rpc_start();
    }
}

const char *mochi_py_rpc_call(const char *request_json) {
    py_rpc_ensure();
    fprintf(g_py_in, "%s\n", request_json);
    fflush(g_py_in);
    static char resp[65536];
    if (!fgets(resp, sizeof(resp), g_py_out)) {
        return "{\"error\":\"python companion closed\"}";
    }
    return resp;
}

int64_t mochi_py_rpc_int(const char *resp) {
    const char *p = strstr(resp, "\"result\"");
    if (!p) return 0;
    p += 8;
    while (*p == ':' || *p == ' ') p++;
    return (int64_t)strtoll(p, NULL, 10);
}

double mochi_py_rpc_float(const char *resp) {
    const char *p = strstr(resp, "\"result\"");
    if (!p) return 0.0;
    p += 8;
    while (*p == ':' || *p == ' ') p++;
    return strtod(p, NULL);
}

const char *mochi_py_rpc_str(const char *resp) {
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

int mochi_py_rpc_bool(const char *resp) {
    const char *p = strstr(resp, "\"result\"");
    if (!p) return 0;
    p += 8;
    while (*p == ':' || *p == ' ') p++;
    return (*p == 't') ? 1 : 0;
}

#else /* _WIN32 or __wasm__: stubs */

const char *mochi_py_rpc_call(const char *req) {
    (void)req;
    fprintf(stderr, "python_rpc: Python FFI not supported on this platform\n"); exit(1);
    return "{}";
}
int64_t     mochi_py_rpc_int(const char *r)   { (void)r; return 0; }
double      mochi_py_rpc_float(const char *r) { (void)r; return 0.0; }
const char *mochi_py_rpc_str(const char *r)   { (void)r; return ""; }
int         mochi_py_rpc_bool(const char *r)  { (void)r; return 0; }

#endif
