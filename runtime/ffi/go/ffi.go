package goffi

import (
	"fmt"
	"reflect"
)

// Runtime maintains a registry of Go functions available to Mochi.
type Runtime struct {
	registry map[string]any
}

// NewRuntime creates an empty FFI runtime.
func NewRuntime() *Runtime {
	return &Runtime{registry: make(map[string]any)}
}

// defaultRuntime is used by package-level helpers.
var defaultRuntime = NewRuntime()

// Register makes a Go function available to Mochi by name.
// It panics if fn is not a function.
func Register(name string, fn any) {
	if err := defaultRuntime.Register(name, fn); err != nil {
		panic(err)
	}
}

// Call invokes the function previously registered under name with the given arguments.
// Arguments and return values are passed as 'any'. If the function's final return value
// is an error, it is returned.
func Call(name string, args ...any) (any, error) {
	return defaultRuntime.Call(name, args...)
}

// Register implements ffi.Registerer.
func (r *Runtime) Register(name string, value any) error {
	r.registry[name] = value
	return nil
}

// Call implements ffi.Caller.
func (r *Runtime) Call(name string, args ...any) (any, error) {
	val, ok := r.registry[name]
	if !ok {
		return nil, fmt.Errorf("goffi: unknown symbol %s", name)
	}

	rv := reflect.ValueOf(val)
	if rv.Kind() == reflect.Func {
		if len(args) != rv.Type().NumIn() {
			return nil, fmt.Errorf("goffi: %s expects %d args, got %d", name, rv.Type().NumIn(), len(args))
		}
		in := make([]reflect.Value, len(args))
		for i, a := range args {
			in[i] = reflect.ValueOf(a)
		}
		outs := rv.Call(in)
		switch len(outs) {
		case 0:
			return nil, nil
		case 1:
			return outs[0].Interface(), nil
		case 2:
			if errv := outs[1]; !errv.IsNil() {
				if err, ok := errv.Interface().(error); ok {
					return outs[0].Interface(), err
				}
			}
			return outs[0].Interface(), nil
		default:
			res := make([]any, len(outs))
			for i, v := range outs {
				res[i] = v.Interface()
			}
			return res, nil
		}
	}

	if len(args) != 0 {
		return nil, fmt.Errorf("goffi: %s is not callable", name)
	}
	return val, nil
}
