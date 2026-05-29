package async

import (
	"fmt"

	"mochi/package3/erlang/etf"
)

// Spawn calls erlang:spawn(Module, Fun, Args) and returns the new Pid.
// The spawned process runs independently; its lifetime is not tied to
// the Port manager connection.
func Spawn(caller Caller, module, fun string, args []interface{}) (etf.Pid, error) {
	v, err := caller.Call("erlang:spawn", []interface{}{
		etf.Atom(module),
		etf.Atom(fun),
		etf.List(args),
	})
	if err != nil {
		return etf.Pid{}, err
	}
	pid, ok := v.(etf.Pid)
	if !ok {
		return etf.Pid{}, fmt.Errorf("async: erlang:spawn returned non-pid: %T", v)
	}
	return pid, nil
}

// SpawnLink calls erlang:spawn_link(Module, Fun, Args). The calling process
// (the port runner) is linked to the spawned process.
func SpawnLink(caller Caller, module, fun string, args []interface{}) (etf.Pid, error) {
	v, err := caller.Call("erlang:spawn_link", []interface{}{
		etf.Atom(module),
		etf.Atom(fun),
		etf.List(args),
	})
	if err != nil {
		return etf.Pid{}, err
	}
	pid, ok := v.(etf.Pid)
	if !ok {
		return etf.Pid{}, fmt.Errorf("async: erlang:spawn_link returned non-pid: %T", v)
	}
	return pid, nil
}

// SpawnMonitor calls erlang:spawn_monitor(Module, Fun, Args) and returns the
// Pid and its MonitorRef together. The Erlang side returns {Pid, Ref}.
func SpawnMonitor(caller Caller, module, fun string, args []interface{}) (etf.Pid, etf.Reference, error) {
	v, err := caller.Call("erlang:spawn_monitor", []interface{}{
		etf.Atom(module),
		etf.Atom(fun),
		etf.List(args),
	})
	if err != nil {
		return etf.Pid{}, etf.Reference{}, err
	}
	tup, ok := v.(etf.Tuple)
	if !ok || len(tup) != 2 {
		return etf.Pid{}, etf.Reference{}, fmt.Errorf("async: erlang:spawn_monitor unexpected reply: %v", v)
	}
	pid, ok := tup[0].(etf.Pid)
	if !ok {
		return etf.Pid{}, etf.Reference{}, fmt.Errorf("async: spawn_monitor: first element not pid: %T", tup[0])
	}
	ref, ok := tup[1].(etf.Reference)
	if !ok {
		return etf.Pid{}, etf.Reference{}, fmt.Errorf("async: spawn_monitor: second element not ref: %T", tup[1])
	}
	return pid, ref, nil
}

// Send calls erlang:send(Pid, Msg). The Erlang BIF returns Msg on success.
// The return value is discarded; errors only surface for transport failures.
func Send(caller Caller, pid etf.Pid, msg interface{}) error {
	_, err := caller.Call("erlang:send", []interface{}{pidArg(pid), msg})
	return err
}

// SendNamed calls erlang:send(Name, Msg) using a registered name atom.
func SendNamed(caller Caller, name string, msg interface{}) error {
	_, err := caller.Call("erlang:send", []interface{}{etf.Atom(name), msg})
	return err
}

// Exit calls erlang:exit(Pid, Reason) to send an exit signal to the process.
func Exit(caller Caller, pid etf.Pid, reason interface{}) error {
	v, err := caller.Call("erlang:exit", []interface{}{pidArg(pid), reason})
	if err != nil {
		return err
	}
	return expectOK(v)
}

// IsAlive calls erlang:is_process_alive(Pid) and returns true when the
// process is alive. The Erlang side returns `true` or `false` atoms.
func IsAlive(caller Caller, pid etf.Pid) (bool, error) {
	v, err := caller.Call("erlang:is_process_alive", []interface{}{pidArg(pid)})
	if err != nil {
		return false, err
	}
	switch {
	case isAtom(v, "true"):
		return true, nil
	case isAtom(v, "false"):
		return false, nil
	default:
		return false, fmt.Errorf("async: is_process_alive: unexpected reply: %v", v)
	}
}

// ProcessInfo calls erlang:process_info(Pid, Key) and returns the value.
// Common keys: "status", "message_queue_len", "current_function", "memory",
// "reductions". The Erlang side returns {Key, Value} or `undefined`.
// Returns (nil, false, nil) when the process is not alive or key is unknown.
func ProcessInfo(caller Caller, pid etf.Pid, key string) (interface{}, bool, error) {
	v, err := caller.Call("erlang:process_info", []interface{}{pidArg(pid), etf.Atom(key)})
	if err != nil {
		return nil, false, err
	}
	if isAtom(v, "undefined") {
		return nil, false, nil
	}
	tup, ok := v.(etf.Tuple)
	if !ok || len(tup) != 2 {
		return nil, false, fmt.Errorf("async: process_info: unexpected reply: %v", v)
	}
	return tup[1], true, nil
}

// Self calls erlang:self() and returns the Pid of the current Erlang process
// (the port runner). Useful for setting up monitors pointing back to the runner.
func Self(caller Caller) (etf.Pid, error) {
	v, err := caller.Call("erlang:self", []interface{}{})
	if err != nil {
		return etf.Pid{}, err
	}
	pid, ok := v.(etf.Pid)
	if !ok {
		return etf.Pid{}, fmt.Errorf("async: erlang:self returned non-pid: %T", v)
	}
	return pid, nil
}
