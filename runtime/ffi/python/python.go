package python

import (
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"strings"

	"mochi/runtime/ffi"
)

// Ensure *Runtime implements ffi.Caller.
var _ ffi.Caller = (*Runtime)(nil)

// Runtime provides access to Python code via subprocess execution.
type Runtime struct{}

// NewRuntime returns a new Python FFI runtime.
func NewRuntime() *Runtime { return &Runtime{} }

var defaultRuntime = NewRuntime()

// Invoke calls a Python function "module:function" using the default runtime.
func Invoke(name string, args ...any) (any, error) {
	return defaultRuntime.Call(name, args...)
}

// ExecDefault executes a code snippet using the default runtime.
func ExecDefault(code string, args ...any) (any, error) {
	return defaultRuntime.Exec(code, args...)
}

// Call invokes a Python function specified as "module:function".
func (r *Runtime) Call(name string, args ...any) (any, error) {
	parts := strings.SplitN(name, ":", 2)
	if len(parts) != 2 {
		return nil, fmt.Errorf("python: name must be module:function")
	}
	return Call(parts[0], parts[1], args...)
}

// Exec runs an arbitrary Python code block using this runtime.
func (r *Runtime) Exec(code string, args ...any) (any, error) {
	return Exec(code, args...)
}

// Call executes fn from the given module with the provided arguments.
//
// The arguments are encoded to JSON and exposed to the Python process
// via the MOCHI_ARGS environment variable. The function's return value
// must be JSON serialisable and is decoded into an arbitrary Go value.
func Call(module, fn string, args ...any) (any, error) {
	src := fmt.Sprintf(`import json, os, importlib, sys
args = json.loads(os.environ.get("MOCHI_ARGS", "[]"))
mod = importlib.import_module("%s")
res = getattr(mod, "%s")(*args)
json.dump(res, sys.stdout)
`, module, fn)
	return run(src, args)
}

// Exec runs an arbitrary Python code block. The code is wrapped into
// a function that receives the argument list as `args` and should
// return the result. The returned value must be JSON serialisable.
func Exec(code string, args ...any) (any, error) {
	// indent user code for the wrapper function
	indented := indent(code, "    ")
	src := fmt.Sprintf("import json, os, sys\nargs = json.loads(os.environ.get('MOCHI_ARGS', '[]'))\n"+
		"def __mochi_fn(args):\n%s\nres = __mochi_fn(args)\njson.dump(res, sys.stdout)\n", indented)
	return run(src, args)
}

func run(code string, args []any) (any, error) {
	file, err := os.CreateTemp("", "mochi_py_*.py")
	if err != nil {
		return nil, err
	}
	defer os.Remove(file.Name())
	if _, err := file.WriteString(code); err != nil {
		file.Close()
		return nil, err
	}
	file.Close()

	data, err := json.Marshal(args)
	if err != nil {
		return nil, err
	}

	cmd := exec.Command("python3", file.Name())
	cmd.Env = append(os.Environ(), "MOCHI_ARGS="+string(data))
	out, err := cmd.CombinedOutput()
	if err != nil {
		return nil, fmt.Errorf("python error: %w\n%s", err, out)
	}

	var result any
	if err := json.Unmarshal(out, &result); err != nil {
		return nil, fmt.Errorf("decode error: %w\noutput: %s", err, out)
	}
	return result, nil
}

func indent(code, prefix string) string {
	if code == "" {
		return ""
	}
	lines := strings.Split(code, "\n")
	for i, l := range lines {
		lines[i] = prefix + l
	}
	return strings.Join(lines, "\n")
}
