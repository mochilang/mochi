package python

import (
	"encoding/json"
	"fmt"
	"os"

	ffiinfo "mochi/runtime/ffi/infer"
	pyruntime "mochi/runtime/python"
)

// Infer loads the given Python module and returns information about its exported symbols.
func Infer(module string) (*ffiinfo.ModuleInfo, error) {
	const pySrc = `import importlib, inspect, json, sys

modname = sys.argv[1]
mod = importlib.import_module(modname)

exported = getattr(mod, "__all__", None)
if exported is None:
    exported = [n for n in dir(mod) if not n.startswith("_")]

def ann_to_str(ann):
    if ann is inspect._empty:
        return ""
    if isinstance(ann, str):
        return ann
    return getattr(ann, "__name__", str(ann))

def process_func(fn):
    sig = inspect.signature(fn)
    params = []
    for p in sig.parameters.values():
        if p.name in ("self", "cls"):
            continue
        params.append({"Name": p.name, "Type": ann_to_str(p.annotation)})
    res = []
    ra = ann_to_str(sig.return_annotation)
    if ra:
        res.append({"Name": "", "Type": ra})
    return {"Name": fn.__name__, "Doc": inspect.getdoc(fn) or "", "Params": params, "Results": res, "Examples": []}

def process_class(cls):
    fields = []
    methods = []
    for name, val in inspect.getmembers(cls):
        if name.startswith("_"):
            continue
        if inspect.isfunction(val) or inspect.ismethoddescriptor(val):
            sig = inspect.signature(val)
            params = []
            for p in sig.parameters.values():
                if p.name in ("self", "cls"):
                    continue
                params.append({"Name": p.name, "Type": ann_to_str(p.annotation)})
            res = []
            ra = ann_to_str(sig.return_annotation)
            if ra:
                res.append({"Name": "", "Type": ra})
            methods.append({"Name": name, "Doc": inspect.getdoc(val) or "", "Params": params, "Results": res, "Examples": []})
        else:
            if not callable(val):
                fields.append({"Name": name, "Type": type(val).__name__, "Tag": ""})
    return {"Name": cls.__name__, "Kind": "class", "Doc": inspect.getdoc(cls) or "", "Fields": fields, "Methods": methods, "Examples": []}

info = {
    "Path": mod.__name__,
    "Doc": inspect.getdoc(mod) or "",
    "Examples": [],
    "Functions": [],
    "Vars": [],
    "Consts": [],
    "Types": [],
}

for name in exported:
    obj = getattr(mod, name)
    if inspect.isfunction(obj):
        info["Functions"].append(process_func(obj))
    elif inspect.isclass(obj):
        info["Types"].append(process_class(obj))
    elif not inspect.ismodule(obj):
        if callable(obj):
            continue
        ann = getattr(mod, "__annotations__", {}).get(name)
        typ = ann_to_str(ann) if ann is not None else type(obj).__name__
        if name.isupper():
            info["Consts"].append({"Name": name, "Type": typ, "Value": repr(obj), "Doc": ""})
        else:
            info["Vars"].append({"Name": name, "Type": typ, "Doc": ""})

json.dump(info, sys.stdout)`

	file, err := os.CreateTemp("", "mochi_py_infer_*.py")
	if err != nil {
		return nil, err
	}
	defer os.Remove(file.Name())
	if _, err := file.WriteString(pySrc); err != nil {
		file.Close()
		return nil, err
	}
	file.Close()

	cmd := pyruntime.Cmd(file.Name(), module)
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
