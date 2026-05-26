/*
 * libmochi: Go FFI via subprocess RPC — Phase 10.2.
 *
 * On POSIX: forks the Go companion binary, sets up bidirectional
 * pipe via pipe()+fork()+exec(), serialises calls as JSON lines.
 * On Windows/_WIN32 and __wasm__: stubs that abort on first call.
 */

#include "mochi/go_rpc.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if !defined(_WIN32) && !defined(__wasm__)

#include <unistd.h>
#include <sys/wait.h>
#include <inttypes.h>

static FILE *g_go_in  = NULL;
static FILE *g_go_out = NULL;

static void go_rpc_start(void) {
    const char *path = getenv("MOCHI_GO_RPC_PATH");
#ifdef MOCHI_GO_RPC_PATH_DEFAULT
    if (!path || !*path) path = MOCHI_GO_RPC_PATH_DEFAULT;
#endif
    if (!path || !*path) {
        fprintf(stderr, "go_rpc: MOCHI_GO_RPC_PATH not set; cannot start Go companion\n"); exit(1);
    }
    int to_go[2], from_go[2];
    if (pipe(to_go) != 0 || pipe(from_go) != 0) {
        fprintf(stderr, "go_rpc: pipe() failed\n"); exit(1);
    }
    pid_t pid = fork();
    if (pid < 0) {
        fprintf(stderr, "go_rpc: fork() failed\n"); exit(1);
    }
    if (pid == 0) {
        /* child — become the Go companion process */
        dup2(to_go[0], STDIN_FILENO);
        dup2(from_go[1], STDOUT_FILENO);
        close(to_go[0]);  close(to_go[1]);
        close(from_go[0]); close(from_go[1]);
        execlp(path, path, (char *)NULL);
        _exit(127);
    }
    /* parent */
    close(to_go[0]);
    close(from_go[1]);
    g_go_in  = fdopen(to_go[1],  "w");
    g_go_out = fdopen(from_go[0], "r");
    if (!g_go_in || !g_go_out) {
        fprintf(stderr, "go_rpc: fdopen() failed\n"); exit(1);
    }
}

static void go_rpc_ensure(void) {
    if (!g_go_in) {
        go_rpc_start();
    }
}

const char *mochi_go_rpc_call(const char *request_json) {
    go_rpc_ensure();
    fprintf(g_go_in, "%s\n", request_json);
    fflush(g_go_in);
    static char resp[65536];
    if (!fgets(resp, sizeof(resp), g_go_out)) {
        return "{\"error\":\"go companion closed\"}";
    }
    return resp;
}

int64_t mochi_go_rpc_int(const char *resp) {
    const char *p = strstr(resp, "\"result\"");
    if (!p) return 0;
    p += 8;
    while (*p == ':' || *p == ' ') p++;
    return (int64_t)strtoll(p, NULL, 10);
}

double mochi_go_rpc_float(const char *resp) {
    const char *p = strstr(resp, "\"result\"");
    if (!p) return 0.0;
    p += 8;
    while (*p == ':' || *p == ' ') p++;
    return strtod(p, NULL);
}

const char *mochi_go_rpc_str(const char *resp) {
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

int mochi_go_rpc_bool(const char *resp) {
    const char *p = strstr(resp, "\"result\"");
    if (!p) return 0;
    p += 8;
    while (*p == ':' || *p == ' ') p++;
    return (*p == 't') ? 1 : 0;
}

#else /* _WIN32 or __wasm__: stubs */

const char *mochi_go_rpc_call(const char *req) {
    (void)req;
    fprintf(stderr, "go_rpc: Go FFI not supported on this platform\n"); exit(1);
    return "{}";
}
int64_t     mochi_go_rpc_int(const char *r)   { (void)r; return 0; }
double      mochi_go_rpc_float(const char *r) { (void)r; return 0.0; }
const char *mochi_go_rpc_str(const char *r)   { (void)r; return ""; }
int         mochi_go_rpc_bool(const char *r)  { (void)r; return 0; }

#endif
