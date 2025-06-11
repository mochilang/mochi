package goffi

import (
	"fmt"
	"go/ast"
	"go/types"

	"golang.org/x/tools/go/packages"

	ffiinfo "mochi/runtime/ffi/infer"
)

// Infer loads the Go package at path and returns information about its exported symbols.
func Infer(path string) (*ffiinfo.ModuleInfo, error) {
	cfg := &packages.Config{Mode: packages.NeedTypes | packages.NeedTypesInfo | packages.NeedName}
	pkgs, err := packages.Load(cfg, path)
	if err != nil {
		return nil, err
	}
	if len(pkgs) == 0 {
		return nil, fmt.Errorf("no package found: %s", path)
	}
	pkg := pkgs[0]

	info := &ffiinfo.ModuleInfo{Path: pkg.PkgPath}
	scope := pkg.Types.Scope()
	for _, name := range scope.Names() {
		if !ast.IsExported(name) {
			continue
		}
		obj := scope.Lookup(name)
		switch o := obj.(type) {
		case *types.Func:
			info.Functions = append(info.Functions, ffiinfo.FuncInfo{Name: name, Signature: o.Type().String()})
		case *types.Var:
			info.Vars = append(info.Vars, ffiinfo.VarInfo{Name: name, Type: o.Type().String()})
		case *types.Const:
			info.Consts = append(info.Consts, ffiinfo.ConstInfo{Name: name, Type: o.Type().String(), Value: o.Val().ExactString()})
		case *types.TypeName:
			kind := typeKind(o.Type().Underlying())
			info.Types = append(info.Types, ffiinfo.TypeInfo{Name: name, Kind: kind})
		}
	}

	return info, nil
}

func typeKind(t types.Type) string {
	switch t.(type) {
	case *types.Struct:
		return "struct"
	case *types.Interface:
		return "interface"
	case *types.Slice:
		return "slice"
	case *types.Array:
		return "array"
	case *types.Map:
		return "map"
	case *types.Pointer:
		return "pointer"
	case *types.Signature:
		return "func"
	case *types.Basic:
		return "basic"
	default:
		return fmt.Sprintf("%T", t)
	}
}
