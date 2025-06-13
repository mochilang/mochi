package ts

import (
	"encoding/json"
	"fmt"
	"reflect"

	"mochi/runtime/ffi/deno"
)

// Runtime provides access to registered Go values and TypeScript module exports.
type Runtime struct {
	registry map[string]any
	packages map[string]PackageInfo
}

// ExportInfo describes a single exported symbol from a TypeScript module.
type ExportInfo struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

// PackageInfo describes a loaded TypeScript module and its exports.
type PackageInfo struct {
	Path    string
	Exports []ExportInfo
}

type tsSymbol struct {
	Module string
	Name   string
}

// NewRuntime returns an empty Runtime.
func NewRuntime() *Runtime {
	return &Runtime{
		registry: make(map[string]any),
		packages: make(map[string]PackageInfo),
	}
}

// Register makes a Go value available by name.
func (r *Runtime) Register(name string, value any) error {
	r.registry[name] = value
	return nil
}

// Call invokes a registered symbol by name with optional arguments.
func (r *Runtime) Call(name string, args ...any) (any, error) {
	val, ok := r.registry[name]
	if !ok {
		return nil, fmt.Errorf("unknown ffi symbol: %s", name)
	}
	switch v := val.(type) {
	case tsSymbol:
		return deno.Attr(v.Module, v.Name, args...)
	default:
		rv := reflect.ValueOf(val)
		if rv.Kind() == reflect.Func {
			if len(args) != rv.Type().NumIn() {
				return nil, fmt.Errorf("%s expects %d args, got %d", name, rv.Type().NumIn(), len(args))
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
				for i, o := range outs {
					res[i] = o.Interface()
				}
				return res, nil
			}
		}
		if len(args) != 0 {
			return nil, fmt.Errorf("ffi symbol %s is not callable", name)
		}
		return val, nil
	}
}

// LoadModule imports the given TypeScript module via Deno and registers its exports.
func (r *Runtime) LoadModule(path string) error {
	exports, err := moduleExports(path)
	if err != nil {
		return err
	}
	info := PackageInfo{Path: path}
	for _, e := range exports {
		r.registry[e.Name] = tsSymbol{Module: path, Name: e.Name}
		info.Exports = append(info.Exports, e)
	}
	r.packages[path] = info
	return nil
}

// ListPackages returns information on all loaded modules.
func (r *Runtime) ListPackages() []PackageInfo {
	out := make([]PackageInfo, 0, len(r.packages))
	for _, p := range r.packages {
		out = append(out, p)
	}
	return out
}

func moduleExports(path string) ([]ExportInfo, error) {
	code := fmt.Sprintf(`
import * as mod from "%s";
let exportsObj = mod;
const entries = Object.entries(mod);
if (entries.length === 1 && entries[0][0] === 'default' && typeof mod.default === 'object') {
  exportsObj = mod.default;
}
return Object.entries(exportsObj).map(([name, value]) => ({name, type: typeof value}));
`, path)
	res, err := deno.Exec(code)
	if err != nil {
		return nil, err
	}
	data, err := json.Marshal(res)
	if err != nil {
		return nil, err
	}
	var out []ExportInfo
	if err := json.Unmarshal(data, &out); err != nil {
		return nil, err
	}
	return out, nil
}

var defaultRuntime = NewRuntime()

// Register adds a value to the default runtime.
func Register(name string, value any) { defaultRuntime.Register(name, value) }

// Call invokes a symbol using the default runtime.
func Call(name string, args ...any) (any, error) { return defaultRuntime.Call(name, args...) }

// LoadModule loads a module using the default runtime.
func LoadModule(path string) error { return defaultRuntime.LoadModule(path) }

// ListPackages returns loaded modules from the default runtime.
func ListPackages() []PackageInfo { return defaultRuntime.ListPackages() }
