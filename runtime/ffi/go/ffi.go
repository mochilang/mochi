package goffi

import (
	"fmt"
	"reflect"
)

// Runtime maintains a registry of Go functions available to Mochi.
type Runtime struct {
	registry     map[string]any
	typeRegistry map[string]reflect.Type
}

// NewRuntime creates an empty FFI runtime.
func NewRuntime() *Runtime {
	return &Runtime{registry: make(map[string]any), typeRegistry: make(map[string]reflect.Type)}
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

// RegisterType registers a Go struct type under the provided name. The exemplar
// value may be either the struct type or a pointer to the struct type.
func RegisterType(name string, exemplar any) { defaultRuntime.RegisterType(name, exemplar) }

// New creates a new instance of the registered type and populates exported
// fields from the provided map.
func New(name string, fields map[string]any) (any, error) { return defaultRuntime.New(name, fields) }

// CallMethod calls a method on obj with the given name and arguments.
func CallMethod(obj any, name string, args ...any) (any, error) {
	return defaultRuntime.CallMethod(obj, name, args...)
}

// RegisterType registers a Go struct type under the provided name. The exemplar
// value may be either the struct type or a pointer to it.
func (r *Runtime) RegisterType(name string, exemplar any) error {
	if exemplar == nil {
		return fmt.Errorf("goffi: exemplar is nil")
	}
	r.typeRegistry[name] = reflect.TypeOf(exemplar)
	return nil
}

// New creates a new instance of the registered type and populates exported
// fields from the provided map.
func (r *Runtime) New(name string, fields map[string]any) (any, error) {
	typ, ok := r.typeRegistry[name]
	if !ok {
		return nil, fmt.Errorf("goffi: unknown type %s", name)
	}
	var inst reflect.Value
	if typ.Kind() == reflect.Pointer {
		inst = reflect.New(typ.Elem())
	} else {
		inst = reflect.New(typ)
	}
	elem := inst
	if elem.Kind() == reflect.Pointer {
		elem = elem.Elem()
	}
	for k, v := range fields {
		f := elem.FieldByName(k)
		if !f.IsValid() || !f.CanSet() {
			return nil, fmt.Errorf("goffi: unknown or unsettable field %s", k)
		}
		val := reflect.ValueOf(v)
		if val.Type().ConvertibleTo(f.Type()) {
			val = val.Convert(f.Type())
		}
		f.Set(val)
	}
	if typ.Kind() == reflect.Pointer {
		return inst.Interface(), nil
	}
	return inst.Elem().Interface(), nil
}

// CallMethod calls a method on obj with the given name and arguments.
func (r *Runtime) CallMethod(obj any, name string, args ...any) (any, error) {
	rv := reflect.ValueOf(obj)
	m := rv.MethodByName(name)
	if !m.IsValid() {
		return nil, fmt.Errorf("goffi: method not found: %s", name)
	}
	if len(args) != m.Type().NumIn() {
		return nil, fmt.Errorf("goffi: %s expects %d args, got %d", name, m.Type().NumIn(), len(args))
	}
	in := make([]reflect.Value, len(args))
	for i, a := range args {
		val := reflect.ValueOf(a)
		param := m.Type().In(i)
		if val.Type().ConvertibleTo(param) {
			val = val.Convert(param)
		}
		in[i] = val
	}
	outs := m.Call(in)
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
