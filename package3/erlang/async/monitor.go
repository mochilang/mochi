package async

import (
	"fmt"

	"mochi/package3/erlang/etf"
)

// Monitor calls erlang:monitor(process, Pid) and returns the monitor
// reference. A {'DOWN', Ref, process, Pid, Reason} message will be sent
// to the monitoring process (the runner) when the target exits.
func Monitor(caller Caller, pid etf.Pid) (etf.Reference, error) {
	v, err := caller.Call("erlang:monitor", []interface{}{etf.Atom("process"), pidArg(pid)})
	if err != nil {
		return etf.Reference{}, err
	}
	ref, ok := v.(etf.Reference)
	if !ok {
		return etf.Reference{}, fmt.Errorf("async: erlang:monitor returned non-ref: %T", v)
	}
	return ref, nil
}

// Demonitor calls erlang:demonitor(Ref). The flush option is included to
// discard any pending DOWN message for this ref.
func Demonitor(caller Caller, ref etf.Reference) error {
	v, err := caller.Call("erlang:demonitor", []interface{}{ref, etf.List{etf.Atom("flush")}})
	if err != nil {
		return err
	}
	// demonitor returns `true` on success (the bool atom, not the atom ok).
	if isAtom(v, "true") {
		return nil
	}
	return expectOK(v)
}

// Link calls erlang:link(Pid) to establish a bidirectional link between the
// runner process and Pid. Returns nil if linking succeeded (the BIF returns
// `true`).
func Link(caller Caller, pid etf.Pid) error {
	v, err := caller.Call("erlang:link", []interface{}{pidArg(pid)})
	if err != nil {
		return err
	}
	if isAtom(v, "true") {
		return nil
	}
	return expectOK(v)
}

// Unlink calls erlang:unlink(Pid).
func Unlink(caller Caller, pid etf.Pid) error {
	v, err := caller.Call("erlang:unlink", []interface{}{pidArg(pid)})
	if err != nil {
		return err
	}
	if isAtom(v, "true") {
		return nil
	}
	return expectOK(v)
}

// DownMessage is the decoded form of a {'DOWN', Ref, process, Pid, Reason}
// message received from the Erlang side when a monitored process exits.
type DownMessage struct {
	Ref    etf.Reference
	Pid    etf.Pid
	Reason interface{}
}

// ParseDownMessage attempts to parse an ETF term as a DOWN message.
// Returns (msg, true) on success; (zero, false) if the shape does not match.
func ParseDownMessage(v interface{}) (DownMessage, bool) {
	tup, ok := v.(etf.Tuple)
	if !ok || len(tup) != 5 {
		return DownMessage{}, false
	}
	if !isAtom(tup[0], "DOWN") {
		return DownMessage{}, false
	}
	ref, ok := tup[1].(etf.Reference)
	if !ok {
		return DownMessage{}, false
	}
	if !isAtom(tup[2], "process") {
		return DownMessage{}, false
	}
	pid, ok := tup[3].(etf.Pid)
	if !ok {
		return DownMessage{}, false
	}
	return DownMessage{Ref: ref, Pid: pid, Reason: tup[4]}, true
}
