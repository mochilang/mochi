package goffi

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/doc"
	"go/printer"
	"go/token"
	"go/types"
	"strings"

	"golang.org/x/tools/go/packages"

	ffiinfo "mochi/runtime/ffi/infer"
)

// Infer loads the Go package at path and returns information about its exported symbols.
func Infer(path string) (*ffiinfo.ModuleInfo, error) {
	cfg := &packages.Config{Mode: packages.NeedTypes | packages.NeedTypesInfo | packages.NeedSyntax | packages.NeedName, Tests: true}
	pkgs, err := packages.Load(cfg, path)
	if err != nil {
		return nil, err
	}
	if len(pkgs) == 0 {
		return nil, fmt.Errorf("no package found: %s", path)
	}
	pkg := pkgs[0]
	if cfg.Tests && len(pkgs) > 1 {
		for _, p := range pkgs {
			if p.PkgPath == pkg.PkgPath && strings.Contains(p.ID, "[") {
				pkg = p
				break
			}
		}
	}

	docPkg, err := doc.NewFromFiles(pkg.Fset, pkg.Syntax, pkg.PkgPath)
	if err != nil {
		return nil, err
	}

	qf := types.RelativeTo(pkg.Types)

	info := &ffiinfo.ModuleInfo{
		Path:      pkg.PkgPath,
		Doc:       strings.TrimSpace(docPkg.Doc),
		Examples:  convertExamples(docPkg.Examples, pkg.Fset),
		Functions: []ffiinfo.FuncInfo{},
		Vars:      []ffiinfo.VarInfo{},
		Consts:    []ffiinfo.ConstInfo{},
		Types:     []ffiinfo.TypeInfo{},
	}

	for _, f := range docPkg.Funcs {
		obj, _ := pkg.Types.Scope().Lookup(f.Name).(*types.Func)
		sig, _ := obj.Type().(*types.Signature)
		params, results := signatureInfo(sig, qf)
		info.Functions = append(info.Functions, ffiinfo.FuncInfo{
			Name:     f.Name,
			Doc:      strings.TrimSpace(f.Doc),
			Params:   params,
			Results:  results,
			Examples: convertExamples(f.Examples, pkg.Fset),
		})
	}

	for _, v := range docPkg.Vars {
		docStr := strings.TrimSpace(v.Doc)
		for _, name := range v.Names {
			if !ast.IsExported(name) {
				continue
			}
			if obj, ok := pkg.Types.Scope().Lookup(name).(*types.Var); ok {
				info.Vars = append(info.Vars, ffiinfo.VarInfo{
					Name: name,
					Type: types.TypeString(obj.Type(), qf),
					Doc:  docStr,
				})
			}
		}
	}

	for _, c := range docPkg.Consts {
		docStr := strings.TrimSpace(c.Doc)
		for _, name := range c.Names {
			if !ast.IsExported(name) {
				continue
			}
			if obj, ok := pkg.Types.Scope().Lookup(name).(*types.Const); ok {
				info.Consts = append(info.Consts, ffiinfo.ConstInfo{
					Name:  name,
					Type:  types.TypeString(obj.Type(), qf),
					Value: obj.Val().String(),
					Doc:   docStr,
				})
			}
		}
	}

	for _, t := range docPkg.Types {
		if !ast.IsExported(t.Name) {
			continue
		}
		obj, _ := pkg.Types.Scope().Lookup(t.Name).(*types.TypeName)
		kind := typeKind(obj.Type().Underlying())
		ti := ffiinfo.TypeInfo{
			Name:     t.Name,
			Kind:     kind,
			Doc:      strings.TrimSpace(t.Doc),
			Examples: convertExamples(t.Examples, pkg.Fset),
		}

		if s, ok := obj.Type().Underlying().(*types.Struct); ok {
			for i := 0; i < s.NumFields(); i++ {
				f := s.Field(i)
				ti.Fields = append(ti.Fields, ffiinfo.FieldInfo{
					Name: f.Name(),
					Type: types.TypeString(f.Type(), qf),
					Tag:  s.Tag(i),
				})
			}
		}

		for _, m := range t.Methods {
			ident := m.Decl.Name
			obj := pkg.TypesInfo.Defs[ident].(*types.Func)
			sig := obj.Type().(*types.Signature)
			params, results := signatureInfo(sig, qf)
			ti.Methods = append(ti.Methods, ffiinfo.FuncInfo{
				Name:     m.Name,
				Doc:      strings.TrimSpace(m.Doc),
				Params:   params,
				Results:  results,
				Examples: convertExamples(m.Examples, pkg.Fset),
			})
		}

		info.Types = append(info.Types, ti)
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

func signatureInfo(sig *types.Signature, qf types.Qualifier) ([]ffiinfo.ParamInfo, []ffiinfo.ParamInfo) {
	params := make([]ffiinfo.ParamInfo, 0, sig.Params().Len())
	for i := 0; i < sig.Params().Len(); i++ {
		v := sig.Params().At(i)
		params = append(params, ffiinfo.ParamInfo{
			Name: v.Name(),
			Type: types.TypeString(v.Type(), qf),
		})
	}

	results := make([]ffiinfo.ParamInfo, 0, sig.Results().Len())
	for i := 0; i < sig.Results().Len(); i++ {
		v := sig.Results().At(i)
		results = append(results, ffiinfo.ParamInfo{
			Name: v.Name(),
			Type: types.TypeString(v.Type(), qf),
		})
	}

	return params, results
}

func convertExamples(ex []*doc.Example, fset *token.FileSet) []ffiinfo.ExampleInfo {
	out := make([]ffiinfo.ExampleInfo, 0, len(ex))
	for _, e := range ex {
		var buf bytes.Buffer
		if e.Code != nil {
			printer.Fprint(&buf, fset, e.Code)
		}
		out = append(out, ffiinfo.ExampleInfo{
			Name:   e.Name,
			Suffix: e.Suffix,
			Doc:    strings.TrimSpace(e.Doc),
			Code:   buf.String(),
			Output: e.Output,
		})
	}
	return out
}
