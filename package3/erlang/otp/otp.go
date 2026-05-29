// Package otp provides typed Go bindings for OTP behavior calls dispatched
// through the MEP-66 Erlang Port bridge.
//
// All three standard OTP behaviors are covered:
//
//   - GenServer  -- gen_server:call / gen_server:cast
//   - Supervisor -- supervisor:which_children / count_children /
//     terminate_child / restart_child / delete_child
//   - Application -- application:start / stop / ensure_all_started /
//     get_env / set_env
//
// Every wrapper calls through the Caller interface, which is satisfied by
// *port.Manager from package3/erlang/port. Tests inject a mock Caller so
// no real `erl` binary is required.
//
// ETF term conventions (all values are returned by the Port runner after
// evaluating the named function call):
//
//	ok                    → etf.Atom("ok")
//	{ok, Term}            → etf.Tuple{etf.Atom("ok"), Term}
//	{error, Reason}       → etf.Tuple{etf.Atom("error"), Reason}
//	undefined             → etf.Atom("undefined")
//	Pid                   → etf.Pid{...}
package otp

import (
	"fmt"

	"mochi/package3/erlang/etf"
)

// Caller dispatches a function call to the connected Erlang node.
// *port.Manager satisfies this interface. Tests use mockCaller.
type Caller interface {
	Call(fun string, args []interface{}) (interface{}, error)
}

// isAtom reports whether v is etf.Atom(a).
func isAtom(v interface{}, a string) bool {
	atom, ok := v.(etf.Atom)
	return ok && string(atom) == a
}

// expectOK returns nil when v is the atom `ok`, or an error otherwise.
func expectOK(v interface{}) error {
	if isAtom(v, "ok") {
		return nil
	}
	if tup, ok := v.(etf.Tuple); ok && len(tup) == 2 && isAtom(tup[0], "error") {
		return fmt.Errorf("otp: erlang error: %v", tup[1])
	}
	return fmt.Errorf("otp: unexpected reply: %v", v)
}

// unwrapOKValue expects either `{ok, Value}` or `{error, Reason}`.
// Returns (Value, nil) or (nil, error).
func unwrapOKValue(v interface{}) (interface{}, error) {
	tup, ok := v.(etf.Tuple)
	if !ok || len(tup) < 2 {
		return nil, fmt.Errorf("otp: unexpected reply shape: %v", v)
	}
	switch etf.Atom(fmt.Sprintf("%v", tup[0])) {
	case "ok":
		return tup[1], nil
	case "error":
		return nil, fmt.Errorf("otp: erlang error: %v", tup[1])
	default:
		return nil, fmt.Errorf("otp: unexpected reply tag: %v", tup[0])
	}
}

// termToString converts an ETF term to a string best-effort.
func termToString(v interface{}) string {
	switch t := v.(type) {
	case etf.Atom:
		return string(t)
	case string:
		return t
	default:
		return fmt.Sprintf("%v", v)
	}
}

// termToInt64 extracts an integer from an ETF integer term.
func termToInt64(v interface{}) (int64, bool) {
	switch n := v.(type) {
	case int:
		return int64(n), true
	case int64:
		return n, true
	case uint32:
		return int64(n), true
	}
	return 0, false
}
