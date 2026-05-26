/*
 * mochi/js_rpc.h — JavaScript FFI via subprocess RPC. Phase 10.4.
 *
 * The RPC protocol is identical to go_rpc / python_rpc: newline-delimited
 * JSON on stdin/stdout of a companion JavaScript file launched via node.
 * The companion script path is baked in via MOCHI_JS_RPC_PATH_DEFAULT at
 * build time or overridden at runtime via MOCHI_JS_RPC_PATH.
 *
 * Request:  {"fn":"name","args":[arg0, arg1, ...]}
 * Response: {"result": value}  or  {"error": "message"}
 */

#ifndef MOCHI_JS_RPC_H
#define MOCHI_JS_RPC_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * mochi_js_rpc_call — send one JSON request to the JS companion and return
 * the raw JSON response line. The returned pointer is valid until the next
 * call. Returns a JSON error object on communication failure.
 */
const char *mochi_js_rpc_call(const char *request_json);

/* Result extractors — parse the "result" field from a response. */
int64_t     mochi_js_rpc_int(const char *response_json);
double      mochi_js_rpc_float(const char *response_json);
const char *mochi_js_rpc_str(const char *response_json);
int         mochi_js_rpc_bool(const char *response_json);

#ifdef __cplusplus
}
#endif

#endif /* MOCHI_JS_RPC_H */
