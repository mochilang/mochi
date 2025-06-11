package python

import (
	"encoding/json"
	"fmt"
	"os"
	"os/exec"

	ffiinfo "mochi/runtime/ffi/infer"
)

// Infer loads the Python module at path and returns information about its
// exported symbols. Only attributes that do not start with '_' are considered
// exported. Constants are identified by names consisting solely of uppercase
// letters.
func Infer(module string) (*ffiinfo.ModuleInfo, error) {
	script := fmt.Sprintf(`import json, importlib, inspect, sys, os
sys.path.insert(0, os.getcwd())
mod = importlib.import_module("%s")
info = {"Path": mod.__name__, "Functions": [], "Vars": [], "Consts": [], "Types": []}
for name in dir(mod):
    if name.startswith("_"):
        continue
    attr = getattr(mod, name)
    if inspect.isroutine(attr):
        try:
            sig = str(inspect.signature(attr))
        except Exception:
            sig = ""
        info["Functions"].append({"Name": name, "Signature": sig})
    elif inspect.isclass(attr):
        info["Types"].append({"Name": name, "Kind": "class"})
    elif name.isupper():
        info["Consts"].append({"Name": name, "Type": type(attr).__name__, "Value": repr(attr)})
    else:
        info["Vars"].append({"Name": name, "Type": type(attr).__name__})
json.dump(info, sys.stdout)
`, module)

	file, err := os.CreateTemp("", "mochi_py_infer_*.py")
	if err != nil {
		return nil, err
	}
	defer os.Remove(file.Name())
	if _, err := file.WriteString(script); err != nil {
		file.Close()
		return nil, err
	}
	file.Close()

	cmd := exec.Command("python3", file.Name())
	out, err := cmd.CombinedOutput()
	if err != nil {
		return nil, fmt.Errorf("python error: %w\n%s", err, out)
	}

	var info ffiinfo.ModuleInfo
	if err := json.Unmarshal(out, &info); err != nil {
		return nil, fmt.Errorf("decode error: %w\noutput: %s", err, out)
	}
	return &info, nil
}
