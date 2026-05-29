package apisurface

import (
	"errors"
	"fmt"
	"sort"
	"strings"
)

// Surface is the typed bridge-side view of an ApiSurface JSON
// document. Every type expression in the document has been parsed
// into a GoType and every cross-reference has been validated.
// Construct via Load.
type Surface struct {
	// File is the raw JSON document the Surface was derived from.
	// Retained so phase 5+ can re-encode the surface, dump it for
	// diagnostics, or check fields the typed view does not expose.
	File *File
	// Packages is indexed by canonical import path.
	Packages map[string]*PackageView
}

// PackageView is a typed view of one Package. Lists are sorted by
// name (matching the underlying JSON layout).
type PackageView struct {
	Pkg *Package
	// Funcs maps func name -> typed declaration.
	Funcs map[string]*FuncDecl
	// Types maps type name -> typed declaration.
	Types map[string]*TypeDecl
	// Consts maps const name -> typed declaration.
	Consts map[string]*ValueDecl
	// Vars maps var name -> typed declaration.
	Vars map[string]*ValueDecl
}

// FuncDecl is a typed view of a Func entry. The Underlying field
// retains the raw JSON record.
type FuncDecl struct {
	Underlying *Func
	Params     []ParamDecl
	Results    []ParamDecl
	TypeParams []TypeParamDecl
}

// ParamDecl is a typed parameter.
type ParamDecl struct {
	Name string
	Type GoType
}

// TypeParamDecl is a typed type parameter (Constraint may be nil for
// "any" elided in source).
type TypeParamDecl struct {
	Name       string
	Constraint GoType
}

// TypeDecl is a typed view of a Type entry.
type TypeDecl struct {
	Underlying       *Type
	Kind             TypeKind
	UnderlyingType   GoType
	AliasOf          GoType
	Fields           []FieldDecl
	Methods          []*FuncDecl
	InterfaceMethods []*FuncDecl
	EmbeddedTypes    []GoType
	TypeParams       []TypeParamDecl
}

// FieldDecl is a typed struct field.
type FieldDecl struct {
	Name     string
	Type     GoType
	Tag      string
	Doc      string
	Exported bool
	Embedded bool
}

// ValueDecl is a typed const or var.
type ValueDecl struct {
	Underlying *Value
	Type       GoType
}

// LoadOptions tweak Surface.Load.
type LoadOptions struct {
	// StrictCrossReferences, when true, makes Load return an error if
	// a NamedType references a package that is not in the document's
	// Imports list (for the consuming package) and is not the self
	// package. The default (false) is permissive because Go stdlib
	// names (io, fmt, ...) are not always in Imports.
	StrictCrossReferences bool
}

// ErrSurfaceLoad is returned for unrecoverable Load failures.
var ErrSurfaceLoad = errors.New("apisurface: surface load")

// Load parses every type expression in f and builds a typed Surface.
// Returns the first parse error encountered, wrapped in
// ErrSurfaceLoad.
func Load(f *File, opts LoadOptions) (*Surface, error) {
	if f == nil {
		return nil, fmt.Errorf("%w: nil file", ErrSurfaceLoad)
	}
	if f.SchemaVersion != SchemaVersion {
		return nil, fmt.Errorf("%w: schema version %d (want %d)", ErrSurfaceLoad, f.SchemaVersion, SchemaVersion)
	}
	s := &Surface{File: f, Packages: make(map[string]*PackageView, len(f.Packages))}
	for i := range f.Packages {
		p := &f.Packages[i]
		view := &PackageView{
			Pkg:    p,
			Funcs:  make(map[string]*FuncDecl, len(p.Funcs)),
			Types:  make(map[string]*TypeDecl, len(p.Types)),
			Consts: make(map[string]*ValueDecl, len(p.Consts)),
			Vars:   make(map[string]*ValueDecl, len(p.Vars)),
		}
		ctx := &loadCtx{selfPkg: p.ImportPath, imports: importSetFromSlice(p.Imports), opts: opts}
		for j := range p.Funcs {
			fd, err := loadFunc(ctx, &p.Funcs[j])
			if err != nil {
				return nil, fmt.Errorf("%w: %s.%s: %v", ErrSurfaceLoad, p.ImportPath, p.Funcs[j].Name, err)
			}
			view.Funcs[p.Funcs[j].Name] = fd
		}
		for j := range p.Types {
			td, err := loadType(ctx, &p.Types[j])
			if err != nil {
				return nil, fmt.Errorf("%w: %s.%s: %v", ErrSurfaceLoad, p.ImportPath, p.Types[j].Name, err)
			}
			view.Types[p.Types[j].Name] = td
		}
		for j := range p.Consts {
			vd, err := loadValue(ctx, &p.Consts[j])
			if err != nil {
				return nil, fmt.Errorf("%w: %s.%s: %v", ErrSurfaceLoad, p.ImportPath, p.Consts[j].Name, err)
			}
			view.Consts[p.Consts[j].Name] = vd
		}
		for j := range p.Vars {
			vd, err := loadValue(ctx, &p.Vars[j])
			if err != nil {
				return nil, fmt.Errorf("%w: %s.%s: %v", ErrSurfaceLoad, p.ImportPath, p.Vars[j].Name, err)
			}
			view.Vars[p.Vars[j].Name] = vd
		}
		s.Packages[p.ImportPath] = view
	}
	return s, nil
}

// PackagePaths returns the import paths of all packages in the
// Surface, sorted lexicographically.
func (s *Surface) PackagePaths() []string {
	out := make([]string, 0, len(s.Packages))
	for k := range s.Packages {
		out = append(out, k)
	}
	sort.Strings(out)
	return out
}

// LookupFunc returns the typed declaration for pkg.fn, or nil if
// either the package or the function is absent.
func (s *Surface) LookupFunc(pkg, fn string) *FuncDecl {
	view, ok := s.Packages[pkg]
	if !ok {
		return nil
	}
	return view.Funcs[fn]
}

// LookupType returns the typed declaration for pkg.name, or nil.
func (s *Surface) LookupType(pkg, name string) *TypeDecl {
	view, ok := s.Packages[pkg]
	if !ok {
		return nil
	}
	return view.Types[name]
}

// LookupMethod returns the typed declaration for pkg.typ.method, or
// nil.
func (s *Surface) LookupMethod(pkg, typ, method string) *FuncDecl {
	td := s.LookupType(pkg, typ)
	if td == nil {
		return nil
	}
	for _, m := range td.Methods {
		if m.Underlying.Name == method {
			return m
		}
	}
	return nil
}

// loadCtx is shared across one package's loaders. It carries the
// canonical import set and the self-package path so we can validate
// cross-references.
type loadCtx struct {
	selfPkg string
	imports map[string]struct{}
	opts    LoadOptions
}

func importSetFromSlice(paths []string) map[string]struct{} {
	out := make(map[string]struct{}, len(paths))
	for _, p := range paths {
		out[p] = struct{}{}
	}
	return out
}

func (c *loadCtx) checkRef(t GoType) error {
	if !c.opts.StrictCrossReferences {
		return nil
	}
	return walkTypes(t, func(t GoType) error {
		n, ok := t.(NamedType)
		if !ok {
			return nil
		}
		if n.PackagePath == "" || n.PackagePath == c.selfPkg {
			return nil
		}
		if _, ok := c.imports[n.PackagePath]; !ok {
			return fmt.Errorf("named type %q references package %q which is not in the import set", n.Name, n.PackagePath)
		}
		return nil
	})
}

// walkTypes calls fn for every GoType in t, including t itself.
// Returns the first non-nil error.
func walkTypes(t GoType, fn func(GoType) error) error {
	if t == nil {
		return nil
	}
	if err := fn(t); err != nil {
		return err
	}
	switch v := t.(type) {
	case PointerType:
		return walkTypes(v.Elem, fn)
	case SliceType:
		return walkTypes(v.Elem, fn)
	case ArrayType:
		return walkTypes(v.Elem, fn)
	case MapType:
		if err := walkTypes(v.Key, fn); err != nil {
			return err
		}
		return walkTypes(v.Value, fn)
	case ChanType:
		return walkTypes(v.Elem, fn)
	case FuncType:
		for _, p := range v.Params {
			if err := walkTypes(p, fn); err != nil {
				return err
			}
		}
		for _, r := range v.Results {
			if err := walkTypes(r, fn); err != nil {
				return err
			}
		}
	case EllipsisType:
		return walkTypes(v.Elem, fn)
	case NamedType:
		for _, a := range v.TypeArgs {
			if err := walkTypes(a, fn); err != nil {
				return err
			}
		}
	}
	return nil
}

func loadFunc(ctx *loadCtx, f *Func) (*FuncDecl, error) {
	fd := &FuncDecl{Underlying: f}
	for _, tp := range f.TypeParams {
		var c GoType
		if tp.Constraint != "" {
			t, err := ParseType(tp.Constraint)
			if err != nil {
				return nil, fmt.Errorf("type param %q constraint %q: %w", tp.Name, tp.Constraint, err)
			}
			c = t
		}
		fd.TypeParams = append(fd.TypeParams, TypeParamDecl{Name: tp.Name, Constraint: c})
	}
	for _, p := range f.Params {
		gt, err := parseAndCheck(ctx, p.Type)
		if err != nil {
			return nil, fmt.Errorf("param %q: %w", p.Name, err)
		}
		fd.Params = append(fd.Params, ParamDecl{Name: p.Name, Type: gt})
	}
	for _, r := range f.Results {
		gt, err := parseAndCheck(ctx, r.Type)
		if err != nil {
			return nil, fmt.Errorf("result %q: %w", r.Name, err)
		}
		fd.Results = append(fd.Results, ParamDecl{Name: r.Name, Type: gt})
	}
	return fd, nil
}

func loadType(ctx *loadCtx, tp *Type) (*TypeDecl, error) {
	td := &TypeDecl{Underlying: tp, Kind: tp.Kind}
	for _, p := range tp.TypeParams {
		var c GoType
		if p.Constraint != "" {
			t, err := ParseType(p.Constraint)
			if err != nil {
				return nil, fmt.Errorf("type param %q constraint %q: %w", p.Name, p.Constraint, err)
			}
			c = t
		}
		td.TypeParams = append(td.TypeParams, TypeParamDecl{Name: p.Name, Constraint: c})
	}
	if tp.AliasOf != "" {
		t, err := parseAndCheck(ctx, tp.AliasOf)
		if err != nil {
			return nil, fmt.Errorf("alias target %q: %w", tp.AliasOf, err)
		}
		td.AliasOf = t
	}
	if tp.Underlying != "" {
		// Underlying may be a literal struct{...}/interface{...}; if
		// the kind already gives the structure, we still keep the raw
		// underlying parsed for diagnostics.
		t, err := ParseType(tp.Underlying)
		if err != nil {
			return nil, fmt.Errorf("underlying %q: %w", tp.Underlying, err)
		}
		td.UnderlyingType = t
	}
	for _, f := range tp.Fields {
		gt, err := parseAndCheck(ctx, f.Type)
		if err != nil {
			return nil, fmt.Errorf("field %q: %w", f.Name, err)
		}
		td.Fields = append(td.Fields, FieldDecl{
			Name:     f.Name,
			Type:     gt,
			Tag:      f.Tag,
			Doc:      f.Doc,
			Exported: f.Exported,
			Embedded: f.Embedded,
		})
	}
	for i := range tp.Methods {
		m, err := loadFunc(ctx, &tp.Methods[i])
		if err != nil {
			return nil, fmt.Errorf("method %q: %w", tp.Methods[i].Name, err)
		}
		td.Methods = append(td.Methods, m)
	}
	for i := range tp.InterfaceMethods {
		m, err := loadFunc(ctx, &tp.InterfaceMethods[i])
		if err != nil {
			return nil, fmt.Errorf("interface method %q: %w", tp.InterfaceMethods[i].Name, err)
		}
		td.InterfaceMethods = append(td.InterfaceMethods, m)
	}
	for _, e := range tp.EmbeddedTypes {
		gt, err := parseAndCheck(ctx, e)
		if err != nil {
			return nil, fmt.Errorf("embedded type %q: %w", e, err)
		}
		td.EmbeddedTypes = append(td.EmbeddedTypes, gt)
	}
	return td, nil
}

func loadValue(ctx *loadCtx, v *Value) (*ValueDecl, error) {
	vd := &ValueDecl{Underlying: v}
	if v.Type != "" {
		gt, err := parseAndCheck(ctx, v.Type)
		if err != nil {
			return nil, err
		}
		vd.Type = gt
	}
	return vd, nil
}

func parseAndCheck(ctx *loadCtx, expr string) (GoType, error) {
	if strings.TrimSpace(expr) == "" {
		return nil, nil
	}
	t, err := ParseType(expr)
	if err != nil {
		return nil, err
	}
	if err := ctx.checkRef(t); err != nil {
		return nil, err
	}
	return t, nil
}
