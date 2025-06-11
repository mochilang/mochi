package deno

import (
	"encoding/json"
	"fmt"
	"path/filepath"
	"strings"

	ffiinfo "mochi/runtime/ffi/infer"
)

// Infer loads the TypeScript module at path using Deno and returns
// information about its exported symbols. Path should be a fully
// qualified module specifier (e.g. file:// or https://).
func Infer(path string) (*ffiinfo.ModuleInfo, error) {
	abs := path
	// If this looks like a local path without scheme, prefix with file://
	if !strings.HasPrefix(path, "http://") && !strings.HasPrefix(path, "https://") && !strings.HasPrefix(path, "file://") {
		p, err := filepath.Abs(path)
		if err != nil {
			return nil, err
		}
		abs = "file://" + p
	}
	src := fmt.Sprintf(`import * as mod from "%s";
const info = { path: "%s", functions: [], vars: [], consts: [], types: [] };
for (const [name, value] of Object.entries(mod)) {
  if (typeof value === 'function') {
    info.functions.push({ name, signature: '' });
  } else if (["number","string","boolean"].includes(typeof value)) {
    info.consts.push({ name, type: typeof value, value: String(value) });
  } else {
    info.vars.push({ name, type: typeof value });
  }
}
console.log(JSON.stringify(info));
`, abs, abs)

	res, err := run(src, nil)
	if err != nil {
		return nil, err
	}
	data, err := json.Marshal(res)
	if err != nil {
		return nil, err
	}

	var js struct {
		Path      string `json:"path"`
		Functions []struct {
			Name      string `json:"name"`
			Signature string `json:"signature"`
		} `json:"functions"`
		Vars []struct {
			Name string `json:"name"`
			Type string `json:"type"`
		} `json:"vars"`
		Consts []struct {
			Name  string `json:"name"`
			Type  string `json:"type"`
			Value string `json:"value"`
		} `json:"consts"`
		Types []struct {
			Name string `json:"name"`
			Kind string `json:"kind"`
		} `json:"types"`
	}
	if err := json.Unmarshal(data, &js); err != nil {
		return nil, err
	}

	info := &ffiinfo.ModuleInfo{Path: js.Path}
	for _, f := range js.Functions {
		info.Functions = append(info.Functions, ffiinfo.FuncInfo{Name: f.Name, Signature: f.Signature})
	}
	for _, v := range js.Vars {
		info.Vars = append(info.Vars, ffiinfo.VarInfo{Name: v.Name, Type: v.Type})
	}
	for _, c := range js.Consts {
		info.Consts = append(info.Consts, ffiinfo.ConstInfo{Name: c.Name, Type: c.Type, Value: c.Value})
	}
	for _, t := range js.Types {
		info.Types = append(info.Types, ffiinfo.TypeInfo{Name: t.Name, Kind: t.Kind})
	}
	return info, nil
}
