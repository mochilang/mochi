package goffi

import (
	"fmt"
	"reflect"
)

// registry holds all functions registered for FFI access.
var registry = map[string]reflect.Value{}

// Register makes a Go function available to Mochi by name.
// It panics if fn is not a function.
func Register(name string, fn any) {
	v := reflect.ValueOf(fn)
	if v.Kind() != reflect.Func {
		panic("goffi: Register expects a function")
	}
	registry[name] = v
}

// Call invokes the function previously registered under name with the given arguments.
// Arguments and return values are passed as 'any'. If the function's final return value
// is an error, it is returned.
func Call(name string, args ...any) (any, error) {
	fn, ok := registry[name]
	if !ok {
		return nil, fmt.Errorf("goffi: unknown function %s", name)
	}

	if len(args) != fn.Type().NumIn() {
		return nil, fmt.Errorf("goffi: %s expects %d args, got %d", name, fn.Type().NumIn(), len(args))
	}

	in := make([]reflect.Value, len(args))
	for i, a := range args {
		in[i] = reflect.ValueOf(a)
	}

	outs := fn.Call(in)
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
