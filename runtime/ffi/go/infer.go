package goffi

import (
	"fmt"
	"go/ast"
	"go/types"
	"sort"

	"golang.org/x/tools/go/packages"

	"mochi/runtime/ffi/infer"
)

// Infer loads the Go package at path and returns information about its exported symbols.
func Infer(path string) (*infer.Package, error) {
	cfg := &packages.Config{Mode: packages.NeedName | packages.NeedTypes | packages.NeedTypesInfo | packages.NeedSyntax}
	pkgs, err := packages.Load(cfg, path)
	if err != nil {
		return nil, err
	}
	if packages.PrintErrors(pkgs) > 0 {
		return nil, fmt.Errorf("failed to load package: %s", path)
	}
	if len(pkgs) == 0 {
		return nil, fmt.Errorf("no packages found for %s", path)
	}
	pkg := pkgs[0]

	info := &infer.Package{Name: pkg.Types.Name()}
	scope := pkg.Types.Scope()
	names := scope.Names()
	sort.Strings(names)
	for _, name := range names {
		if !ast.IsExported(name) {
			continue
		}
		obj := scope.Lookup(name)
		if obj == nil {
			continue
		}
		switch o := obj.(type) {
		case *types.Func:
			info.Symbols = append(info.Symbols, infer.Symbol{Name: name, Kind: infer.SymbolFunc, Type: o.Type().String()})
		case *types.Var:
			info.Symbols = append(info.Symbols, infer.Symbol{Name: name, Kind: infer.SymbolVar, Type: o.Type().String()})
		case *types.Const:
			info.Symbols = append(info.Symbols, infer.Symbol{Name: name, Kind: infer.SymbolConst, Type: o.Type().String()})
		case *types.TypeName:
			kind := infer.SymbolType
			switch u := o.Type().Underlying().(type) {
			case *types.Struct:
				kind = infer.SymbolStruct
			case *types.Interface:
				kind = infer.SymbolInterface
			default:
				_ = u
			}
			info.Symbols = append(info.Symbols, infer.Symbol{Name: name, Kind: kind, Type: o.Type().String()})
		}
	}
	return info, nil
}
