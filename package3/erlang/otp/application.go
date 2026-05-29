package otp

import (
	"fmt"

	"mochi/package3/erlang/etf"
)

// Application wraps application:start, stop, ensure_all_started, get_env,
// and set_env for a single OTP application.
type Application struct {
	caller Caller
	name   string
}

// NewApplication returns an Application wrapper for the named OTP application.
// name is the atom name of the application, e.g. "cowboy".
func NewApplication(caller Caller, name string) *Application {
	return &Application{caller: caller, name: name}
}

// Start calls application:start(Name) and returns an error when the Erlang
// side replies with {error, Reason}. The atom `already_started` is not
// treated as an error here; callers that need to distinguish it should use
// StartResult instead.
func (a *Application) Start() error {
	v, err := a.caller.Call("application:start", []interface{}{etf.Atom(a.name)})
	if err != nil {
		return err
	}
	if isAtom(v, "ok") {
		return nil
	}
	if tup, ok := v.(etf.Tuple); ok && len(tup) == 2 && isAtom(tup[0], "error") {
		// {error, {already_started, App}} is benign — normalise to nil.
		if inner, ok := tup[1].(etf.Tuple); ok && len(inner) == 2 && isAtom(inner[0], "already_started") {
			return nil
		}
		return fmt.Errorf("otp: application:start(%s): %v", a.name, tup[1])
	}
	return fmt.Errorf("otp: application:start(%s): unexpected reply: %v", a.name, v)
}

// StartPermanent calls application:start(Name, permanent). A permanent
// application crashes the entire node if it terminates abnormally.
func (a *Application) StartPermanent() error {
	v, err := a.caller.Call("application:start", []interface{}{
		etf.Atom(a.name), etf.Atom("permanent"),
	})
	if err != nil {
		return err
	}
	if isAtom(v, "ok") {
		return nil
	}
	if tup, ok := v.(etf.Tuple); ok && len(tup) == 2 && isAtom(tup[0], "error") {
		if inner, ok := tup[1].(etf.Tuple); ok && len(inner) == 2 && isAtom(inner[0], "already_started") {
			return nil
		}
		return fmt.Errorf("otp: application:start(%s, permanent): %v", a.name, tup[1])
	}
	return fmt.Errorf("otp: application:start(%s, permanent): unexpected reply: %v", a.name, v)
}

// Stop calls application:stop(Name).
func (a *Application) Stop() error {
	v, err := a.caller.Call("application:stop", []interface{}{etf.Atom(a.name)})
	if err != nil {
		return err
	}
	return expectOK(v)
}

// EnsureAllStarted calls application:ensure_all_started(Name) and returns
// the list of application names that were started as a result. The Erlang
// side returns {ok, [App]} on success.
func (a *Application) EnsureAllStarted() ([]string, error) {
	v, err := a.caller.Call("application:ensure_all_started", []interface{}{etf.Atom(a.name)})
	if err != nil {
		return nil, err
	}
	val, err := unwrapOKValue(v)
	if err != nil {
		return nil, err
	}
	list, ok := val.(etf.List)
	if !ok {
		return nil, fmt.Errorf("otp: application:ensure_all_started: expected list, got %T", val)
	}
	apps := make([]string, 0, len(list))
	for _, item := range list {
		apps = append(apps, termToString(item))
	}
	return apps, nil
}

// GetEnv calls application:get_env(Name, Key) and returns the value when
// present. The Erlang side returns {ok, Val} or the atom `undefined`.
// The second return value is true when the key is set.
func (a *Application) GetEnv(key string) (interface{}, bool, error) {
	v, err := a.caller.Call("application:get_env", []interface{}{
		etf.Atom(a.name), etf.Atom(key),
	})
	if err != nil {
		return nil, false, err
	}
	if isAtom(v, "undefined") {
		return nil, false, nil
	}
	tup, ok := v.(etf.Tuple)
	if !ok || len(tup) != 2 || !isAtom(tup[0], "ok") {
		return nil, false, fmt.Errorf("otp: application:get_env unexpected reply: %v", v)
	}
	return tup[1], true, nil
}

// SetEnv calls application:set_env(Name, Key, Val). Persistent is false.
func (a *Application) SetEnv(key string, value interface{}) error {
	v, err := a.caller.Call("application:set_env", []interface{}{
		etf.Atom(a.name), etf.Atom(key), value,
	})
	if err != nil {
		return err
	}
	return expectOK(v)
}

// LoadedApplications calls application:loaded_applications() and returns the
// names of all currently loaded applications. The Erlang side returns
// [{Name, Desc, Vsn}].
func LoadedApplications(caller Caller) ([]string, error) {
	v, err := caller.Call("application:loaded_applications", []interface{}{})
	if err != nil {
		return nil, err
	}
	return parseApplicationList(v)
}

// WhichApplications calls application:which_applications() and returns the
// names of all currently running applications.
func WhichApplications(caller Caller) ([]string, error) {
	v, err := caller.Call("application:which_applications", []interface{}{})
	if err != nil {
		return nil, err
	}
	return parseApplicationList(v)
}

func parseApplicationList(v interface{}) ([]string, error) {
	list, ok := v.(etf.List)
	if !ok {
		return nil, fmt.Errorf("otp: expected application list, got %T", v)
	}
	apps := make([]string, 0, len(list))
	for _, item := range list {
		tup, ok := item.(etf.Tuple)
		if !ok || len(tup) < 1 {
			continue
		}
		apps = append(apps, termToString(tup[0]))
	}
	return apps, nil
}
