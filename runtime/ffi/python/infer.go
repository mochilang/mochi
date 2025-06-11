package python

import (
	"encoding/json"
	"fmt"
	"os"
	"os/exec"

	ffiinfo "mochi/runtime/ffi/infer"
)

// Infer loads the Python package at path and returns information about its
// public objects such as functions, constants, variables and classes. It
// recursively walks submodules to gather as much detail as possible.
func Infer(pkg string) (*ffiinfo.ModuleInfo, error) {
	src := `import importlib, pkgutil, inspect, json, sys
pkg_name = sys.argv[1]
modules = []
stack = [pkg_name]
seen = set()
while stack:
    name = stack.pop()
    if name in seen:
        continue
    seen.add(name)
    try:
        m = importlib.import_module(name)
    except Exception:
        continue
    modules.append(m)
    if hasattr(m, "__path__"):
        for info in pkgutil.iter_modules(m.__path__, m.__name__ + "."):
            stack.append(info.name)
result = {"Path": pkg_name, "Functions": [], "Vars": [], "Consts": [], "Types": []}
for mod in modules:
    for name, obj in vars(mod).items():
        if name.startswith("_"):
            continue
        qname = mod.__name__ + "." + name
        if inspect.isclass(obj):
            result["Types"].append({"Name": qname, "Kind": "class"})
        elif inspect.isfunction(obj) or inspect.isbuiltin(obj) or inspect.ismethod(obj):
            try:
                sig = str(inspect.signature(obj))
            except Exception:
                sig = ""
            result["Functions"].append({"Name": qname, "Signature": sig})
        elif inspect.ismodule(obj):
            continue
        else:
            typ = type(obj).__name__
            if isinstance(obj, (int, float, str, bool)) or obj is None:
                result["Consts"].append({"Name": qname, "Type": typ, "Value": repr(obj)})
            else:
                result["Vars"].append({"Name": qname, "Type": typ})
json.dump(result, sys.stdout)`

	file, err := os.CreateTemp("", "mochi_py_inf_*.py")
	if err != nil {
		return nil, err
	}
	defer os.Remove(file.Name())
	if _, err := file.WriteString(src); err != nil {
		file.Close()
		return nil, err
	}
	file.Close()

	cmd := exec.Command("python3", file.Name(), pkg)
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
