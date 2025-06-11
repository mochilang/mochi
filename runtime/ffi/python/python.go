package python

import (
	"encoding/json"
	"fmt"
	"os"

	pyruntime "mochi/runtime/python"
)

// Attr returns a module attribute. If the attribute is a callable it is
// invoked with the provided arguments, otherwise the arguments must be empty
// and the raw value is returned. The arguments are passed via the MOCHI_ARGS
// environment variable as JSON and the result is decoded from the process
// output.
func Attr(module, name string, args ...any) (any, error) {
	src := fmt.Sprintf(`import json, os, importlib, sys, inspect
args = json.loads(os.environ.get("MOCHI_ARGS", "[]"))
mod = importlib.import_module("%s")
attr = getattr(mod, "%s")
if inspect.isroutine(attr):
    res = attr(*args)
else:
    if len(args) != 0:
        raise TypeError("%s.%s is not callable")
    res = attr
json.dump(res, sys.stdout)
`, module, name, module, name)

	file, err := os.CreateTemp("", "mochi_py_*.py")
	if err != nil {
		return nil, err
	}
	defer os.Remove(file.Name())
	if _, err := file.WriteString(src); err != nil {
		file.Close()
		return nil, err
	}
	file.Close()

	if args == nil {
		args = []any{}
	}
	data, err := json.Marshal(args)
	if err != nil {
		return nil, err
	}

	cmd := pyruntime.Cmd(file.Name())
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
