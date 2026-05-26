/*
 * mochi/python_rpc.h — Python FFI via subprocess RPC. Phase 10.3.
 *
 * The RPC protocol is identical to go_rpc: newline-delimited JSON on
 * stdin/stdout of a companion Python script launched via python3.
 * The companion script is compiled alongside the Mochi source and the
 * path is baked in via MOCHI_PYTHON_RPC_PATH_DEFAULT at build time.
 *
 * Request:  {"fn":"name","args":[arg0, arg1, ...]}
 * Response: {"result": value}  or  {"error": "message"}
 */

#ifndef MOCHI_PYTHON_RPC_H
#define MOCHI_PYTHON_RPC_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * mochi_py_rpc_call — send one JSON request to the Python companion and
 * return the raw JSON response line. The returned pointer is valid until
 * the next call. Returns a JSON error object on communication failure.
 */
const char *mochi_py_rpc_call(const char *request_json);

/* Result extractors — parse the "result" field from a response. */
int64_t     mochi_py_rpc_int(const char *response_json);
double      mochi_py_rpc_float(const char *response_json);
const char *mochi_py_rpc_str(const char *response_json);
int         mochi_py_rpc_bool(const char *response_json);

#ifdef __cplusplus
}
#endif

#endif /* MOCHI_PYTHON_RPC_H */
