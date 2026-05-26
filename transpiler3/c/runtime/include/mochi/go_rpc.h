/*
 * mochi/go_rpc.h — Go FFI via subprocess RPC (Phase 10.2).
 *
 * Provides the runtime support for `extern go fun` declarations. Each
 * Mochi program that uses Go FFI runs a companion Go executable as a
 * subprocess; function calls are serialised as newline-delimited JSON
 * on the subprocess's stdin, and results are read back as JSON on
 * stdout.
 *
 * The Go executable path is resolved at runtime from MOCHI_GO_RPC_PATH
 * (set by the build driver to the compiled companion binary).
 *
 * Protocol (one request per call, synchronous):
 *   request:  {"fn":"name","args":[...values...]}\n
 *   response: {"result":<value>}\n  or  {"error":"<message>"}\n
 *
 * Supported argument/result types: int64, float64, string.
 *
 * Platform: POSIX only (Linux, macOS, Cosmopolitan). Windows and WASM
 * compile to stubs that abort on the first Go FFI call.
 */

#pragma once

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * mochi_go_rpc_call — send a JSON request to the Go companion process
 * and return the raw JSON response line. The returned pointer is valid
 * until the next call. Never returns NULL.
 */
const char *mochi_go_rpc_call(const char *request_json);

/*
 * mochi_go_rpc_int — extract an int64 result from a response line.
 */
int64_t mochi_go_rpc_int(const char *response_json);

/*
 * mochi_go_rpc_float — extract a float64 result from a response line.
 */
double mochi_go_rpc_float(const char *response_json);

/*
 * mochi_go_rpc_str — extract a string result from a response line.
 * The returned pointer is valid until the next call.
 */
const char *mochi_go_rpc_str(const char *response_json);

/*
 * mochi_go_rpc_bool — extract a bool result (0 or 1) from a response.
 */
int mochi_go_rpc_bool(const char *response_json);

#ifdef __cplusplus
}
#endif
