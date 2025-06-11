package deno

import (
	"encoding/json"
	"fmt"
	"os/exec"
	"strings"

	ffiinfo "mochi/runtime/ffi/infer"
)

// Infer loads the TypeScript module at path using `deno doc` and returns
// information about its exported symbols.
func Infer(path string) (*ffiinfo.ModuleInfo, error) {
	out, err := exec.Command("deno", "doc", "--json", path).Output()
	if err != nil {
		if ee, ok := err.(*exec.ExitError); ok {
			return nil, fmt.Errorf("deno doc failed: %w\n%s", err, ee.Stderr)
		}
		return nil, fmt.Errorf("deno doc failed: %w", err)
	}

	var doc struct {
		Nodes []docNode `json:"nodes"`
	}
	if err := json.Unmarshal(out, &doc); err != nil {
		return nil, fmt.Errorf("parse doc json: %w", err)
	}

	info := &ffiinfo.ModuleInfo{Path: path}
	for _, n := range doc.Nodes {
		if n.DeclarationKind != "export" && n.DeclarationKind != "exportDefault" {
			continue
		}
		switch n.Kind {
		case "function":
			sig := buildFuncSignature(n.FunctionDef)
			info.Functions = append(info.Functions, ffiinfo.FuncInfo{Name: n.Name, Signature: sig})
		case "variable":
			t := tsTypeString(n.VariableDef.TsType)
			if isFunctionVar(path, n.Name) {
				info.Functions = append(info.Functions, ffiinfo.FuncInfo{Name: n.Name, Signature: fmt.Sprintf("(%s)", t)})
				continue
			}
			if n.VariableDef.Kind == "const" {
				info.Consts = append(info.Consts, ffiinfo.ConstInfo{Name: n.Name, Type: t})
			} else {
				info.Vars = append(info.Vars, ffiinfo.VarInfo{Name: n.Name, Type: t})
			}
		case "class", "interface", "typeAlias", "enum":
			info.Types = append(info.Types, ffiinfo.TypeInfo{Name: n.Name, Kind: n.Kind})
		}
	}
	return info, nil
}

type docNode struct {
	Name            string       `json:"name"`
	Kind            string       `json:"kind"`
	DeclarationKind string       `json:"declarationKind"`
	VariableDef     variableDef  `json:"variableDef"`
	FunctionDef     *functionDef `json:"functionDef"`
}

type variableDef struct {
	Kind   string  `json:"kind"`
	TsType *tsType `json:"tsType"`
}

type functionDef struct {
	Params     []param `json:"params"`
	ReturnType *tsType `json:"returnType"`
}

type param struct {
	Name   string  `json:"name"`
	TsType *tsType `json:"tsType"`
}

type tsType struct {
	Repr string `json:"repr"`
}

func tsTypeString(t *tsType) string {
	if t == nil {
		return ""
	}
	return t.Repr
}

func buildFuncSignature(f *functionDef) string {
	if f == nil {
		return ""
	}
	parts := make([]string, len(f.Params))
	for i, p := range f.Params {
		typ := tsTypeString(p.TsType)
		if typ != "" {
			parts[i] = p.Name + ": " + typ
		} else {
			parts[i] = p.Name
		}
	}
	ret := tsTypeString(f.ReturnType)
	if ret != "" {
		return fmt.Sprintf("(%s) => %s", strings.Join(parts, ", "), ret)
	}
	return fmt.Sprintf("(%s)", strings.Join(parts, ", "))
}

// isFunctionVar checks if the exported variable evaluates to a function.
func isFunctionVar(path, name string) bool {
	src := fmt.Sprintf("import * as m from \"%s\"; console.log(typeof m['%s']);", path, name)
	out, err := exec.Command("deno", "eval", src).Output()
	if err != nil {
		return false
	}
	return strings.TrimSpace(string(out)) == "function"
}
