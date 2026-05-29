package otp

import (
	"fmt"

	"mochi/package3/erlang/etf"
)

// Supervisor wraps the supervisor:* functions for a single supervisor process.
type Supervisor struct {
	caller Caller
	name   string
}

// NewSupervisor returns a Supervisor bound to the registered supervisor name.
func NewSupervisor(caller Caller, name string) *Supervisor {
	return &Supervisor{caller: caller, name: name}
}

// ChildInfo holds the data returned by supervisor:which_children for one child.
type ChildInfo struct {
	// ID is the child spec identifier (any Erlang term).
	ID interface{}
	// PID is non-nil when the child process is running.
	PID *etf.Pid
	// Type is "worker" or "supervisor".
	Type string
	// Modules is the list of callback module names.
	Modules []string
}

// WhichChildren calls supervisor:which_children(Name) and returns the parsed
// child list. Each element of the Erlang list has the shape:
//
//	{Id, Child, Type, Modules}
//
// where Child is a Pid, the atom `undefined`, or the atom `restarting`.
func (s *Supervisor) WhichChildren() ([]ChildInfo, error) {
	v, err := s.caller.Call("supervisor:which_children", []interface{}{etf.Atom(s.name)})
	if err != nil {
		return nil, err
	}
	list, ok := v.(etf.List)
	if !ok {
		return nil, fmt.Errorf("otp: supervisor:which_children expected list, got %T", v)
	}
	children := make([]ChildInfo, 0, len(list))
	for _, item := range list {
		tup, ok := item.(etf.Tuple)
		if !ok || len(tup) < 4 {
			continue
		}
		ci := ChildInfo{
			ID:   tup[0],
			Type: termToString(tup[2]),
		}
		if pid, ok := tup[1].(etf.Pid); ok {
			ci.PID = &pid
		}
		if mods, ok := tup[3].(etf.List); ok {
			for _, m := range mods {
				ci.Modules = append(ci.Modules, termToString(m))
			}
		}
		children = append(children, ci)
	}
	return children, nil
}

// ChildCounts holds the values from supervisor:count_children.
type ChildCounts struct {
	// Specs is the total number of child specifications.
	Specs int
	// Active is the number of actively running child processes.
	Active int
	// Supervisors is the number of running child supervisors.
	Supervisors int
	// Workers is the number of running child worker processes.
	Workers int
}

// CountChildren calls supervisor:count_children(Name) and returns the parsed
// counts. The Erlang return value is a proplist:
//
//	[{specs,N},{active,N},{supervisors,N},{workers,N}]
func (s *Supervisor) CountChildren() (ChildCounts, error) {
	v, err := s.caller.Call("supervisor:count_children", []interface{}{etf.Atom(s.name)})
	if err != nil {
		return ChildCounts{}, err
	}
	list, ok := v.(etf.List)
	if !ok {
		return ChildCounts{}, fmt.Errorf("otp: supervisor:count_children expected list, got %T", v)
	}
	var cc ChildCounts
	for _, item := range list {
		tup, ok := item.(etf.Tuple)
		if !ok || len(tup) != 2 {
			continue
		}
		key := termToString(tup[0])
		n, _ := termToInt64(tup[1])
		switch key {
		case "specs":
			cc.Specs = int(n)
		case "active":
			cc.Active = int(n)
		case "supervisors":
			cc.Supervisors = int(n)
		case "workers":
			cc.Workers = int(n)
		}
	}
	return cc, nil
}

// TerminateChild calls supervisor:terminate_child(Name, ChildID).
func (s *Supervisor) TerminateChild(childID interface{}) error {
	v, err := s.caller.Call("supervisor:terminate_child", []interface{}{etf.Atom(s.name), childID})
	if err != nil {
		return err
	}
	return expectOK(v)
}

// RestartChild calls supervisor:restart_child(Name, ChildID) and returns the
// PID of the restarted child on success.
func (s *Supervisor) RestartChild(childID interface{}) (etf.Pid, error) {
	v, err := s.caller.Call("supervisor:restart_child", []interface{}{etf.Atom(s.name), childID})
	if err != nil {
		return etf.Pid{}, err
	}
	val, err := unwrapOKValue(v)
	if err != nil {
		return etf.Pid{}, err
	}
	// restart_child returns {ok, Child} or {ok, Child, Info}.
	// Child may itself be a tuple if the reply is {ok, {Pid, Info}}.
	if pid, ok := val.(etf.Pid); ok {
		return pid, nil
	}
	// Some supervisors return {ok, {Pid, _Extra}} — unwrap one more level.
	if tup, ok := val.(etf.Tuple); ok && len(tup) >= 1 {
		if pid, ok := tup[0].(etf.Pid); ok {
			return pid, nil
		}
	}
	return etf.Pid{}, fmt.Errorf("otp: supervisor:restart_child returned unexpected child term: %v", val)
}

// DeleteChild calls supervisor:delete_child(Name, ChildID).
func (s *Supervisor) DeleteChild(childID interface{}) error {
	v, err := s.caller.Call("supervisor:delete_child", []interface{}{etf.Atom(s.name), childID})
	if err != nil {
		return err
	}
	return expectOK(v)
}
