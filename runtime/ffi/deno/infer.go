package deno

import (
	"encoding/json"
	"fmt"
	"os/exec"
	"strings"

	ffiinfo "mochi/runtime/ffi/infer"
)

// Infer runs `deno doc --json` on the provided module path and converts the
// output into a ModuleInfo structure describing exported symbols.
func Infer(path string) (*ffiinfo.ModuleInfo, error) {
	cmd := exec.Command("deno", "doc", "--json", path)
	out, err := cmd.CombinedOutput()
	if err != nil {
		return nil, fmt.Errorf("deno doc failed: %w\n%s", err, out)
	}

	var nodes []map[string]any
	if err := json.Unmarshal(out, &nodes); err != nil {
		return nil, fmt.Errorf("decode error: %w", err)
	}

	info := &ffiinfo.ModuleInfo{
		Path:      path,
		Functions: []ffiinfo.FuncInfo{},
		Vars:      []ffiinfo.VarInfo{},
		Consts:    []ffiinfo.ConstInfo{},
		Types:     []ffiinfo.TypeInfo{},
	}

	for _, n := range nodes {
		kind, _ := n["kind"].(string)
		name, _ := n["name"].(string)
		doc := extractDoc(n)

		switch kind {
		case "moduleDoc":
			info.Doc = doc
		case "function":
			fn := ffiinfo.FuncInfo{Name: name, Doc: doc}
			if def, ok := n["functionDef"].(map[string]any); ok {
				fn.Params = parseParams(def)
				if rt, ok := def["returnType"].(map[string]any); ok {
					fn.Results = []ffiinfo.ParamInfo{{Type: tsType(rt)}}
				}
			}
			info.Functions = append(info.Functions, fn)
		case "variable":
			if def, ok := n["variableDef"].(map[string]any); ok {
				t := tsTypeFromAny(def["tsType"])
				kindStr, _ := def["kind"].(string)
				if kindStr == "const" {
					info.Consts = append(info.Consts, ffiinfo.ConstInfo{Name: name, Type: t, Doc: doc})
				} else {
					info.Vars = append(info.Vars, ffiinfo.VarInfo{Name: name, Type: t, Doc: doc})
				}
			}
		case "class", "interface", "enum", "typeAlias", "namespace":
			info.Types = append(info.Types, ffiinfo.TypeInfo{Name: name, Kind: kind, Doc: doc})
		}
	}

	return info, nil
}

func extractDoc(n map[string]any) string {
	if js, ok := n["jsDoc"].(map[string]any); ok {
		if d, ok := js["doc"].(string); ok {
			return strings.TrimSpace(d)
		}
	}
	return ""
}

func parseParams(def map[string]any) []ffiinfo.ParamInfo {
	params := []ffiinfo.ParamInfo{}
	if ps, ok := def["params"].([]any); ok {
		for _, p := range ps {
			if pm, ok := p.(map[string]any); ok {
				name := paramName(pm)
				t := tsTypeFromAny(pm["tsType"])
				params = append(params, ffiinfo.ParamInfo{Name: name, Type: t})
			}
		}
	}
	return params
}

func paramName(pm map[string]any) string {
	if n, ok := pm["name"].(string); ok {
		return n
	}
	if left, ok := pm["left"].(map[string]any); ok {
		return paramName(left)
	}
	if arg, ok := pm["arg"].(map[string]any); ok {
		return paramName(arg)
	}
	return ""
}

func tsType(m map[string]any) string {
	if m == nil {
		return ""
	}
	if r, ok := m["repr"].(string); ok {
		return r
	}
	return ""
}

func tsTypeFromAny(v any) string {
	if m, ok := v.(map[string]any); ok {
		return tsType(m)
	}
	return ""
}
