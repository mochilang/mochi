package otp

import (
	"fmt"

	"mochi/package3/erlang/etf"
)

// GenServer wraps gen_server:call, gen_server:cast, and gen_server:stop for
// a single registered server name.
//
// The server name is passed to every Erlang call as an atom, so it must match
// the name used in gen_server:start_link({local, Name}, ...).
type GenServer struct {
	caller Caller
	name   string
}

// NewGenServer returns a GenServer bound to the registered server name.
// name is an Erlang atom string, e.g. "my_server".
func NewGenServer(caller Caller, name string) *GenServer {
	return &GenServer{caller: caller, name: name}
}

// Call dispatches gen_server:call(Name, Request) and returns the raw ETF
// reply value. Errors from the Erlang side (noproc, timeout, etc.) surface
// as Go errors.
func (g *GenServer) Call(request interface{}) (interface{}, error) {
	return g.caller.Call("gen_server:call", []interface{}{etf.Atom(g.name), request})
}

// CallTimeout dispatches gen_server:call(Name, Request, TimeoutMs).
// timeoutMs is the Erlang-side call timeout in milliseconds; pass a value
// greater than the default 5000 for long-running handlers.
func (g *GenServer) CallTimeout(request interface{}, timeoutMs int) (interface{}, error) {
	return g.caller.Call("gen_server:call", []interface{}{
		etf.Atom(g.name), request, int64(timeoutMs),
	})
}

// Cast dispatches gen_server:cast(Name, Message). Always returns nil on
// success; the Erlang side never replies to a cast.
func (g *GenServer) Cast(message interface{}) error {
	v, err := g.caller.Call("gen_server:cast", []interface{}{etf.Atom(g.name), message})
	if err != nil {
		return err
	}
	return expectOK(v)
}

// Stop calls gen_server:stop(Name) and waits for the process to terminate.
func (g *GenServer) Stop() error {
	v, err := g.caller.Call("gen_server:stop", []interface{}{etf.Atom(g.name)})
	if err != nil {
		return err
	}
	return expectOK(v)
}

// StartLink calls gen_server:start_link({local, Name}, Module, InitArgs, [])
// and returns the PID of the newly started process on success.
func StartLink(caller Caller, module, name string, initArgs []interface{}) (etf.Pid, error) {
	localReg := etf.Tuple{etf.Atom("local"), etf.Atom(name)}
	v, err := caller.Call("gen_server:start_link", []interface{}{
		localReg,
		etf.Atom(module),
		etf.List(initArgs),
		etf.List(nil),
	})
	if err != nil {
		return etf.Pid{}, err
	}
	val, err := unwrapOKValue(v)
	if err != nil {
		return etf.Pid{}, err
	}
	pid, ok := val.(etf.Pid)
	if !ok {
		return etf.Pid{}, fmt.Errorf("otp: gen_server:start_link returned non-pid: %T", val)
	}
	return pid, nil
}
