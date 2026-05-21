package resolve

import (
	"fmt"
	"go/ast"
	"go/types"
	"sort"
	"strings"

	"mochi/compiler3/ffi/typebridge"

	"golang.org/x/tools/go/packages"
)

// MochiResolverVersion is the bridge-and-resolver wire version. Bump
// when the binding format changes; the cache loader refuses entries
// with a mismatched version, forcing a clean re-resolve.
const MochiResolverVersion = "mep-0043/v1"

// Resolver loads Go packages and produces typed Mochi bindings.
// Stateless apart from the optional disk cache; safe for concurrent
// use as long as the cache is concurrency-safe (the disk cache uses
// per-entry temp-file rename, see cache.go).
type Resolver struct {
	// Cache is the optional persistent cache. Nil means resolve
	// always (used in tests and `--no-cache` builds).
	Cache *Cache
	// GoSumHash is the SHA-256 of the user's go.sum. Resolve passes
	// this through into the PackageBinding so cache invalidation can
	// key on it. Empty means "no module context" (stdlib walks).
	GoSumHash string
}

// New constructs a Resolver with no cache.
func New() *Resolver { return &Resolver{} }

// Resolve loads one Go package and produces its binding. The
// importPath is interpreted by `go/packages` in the calling process's
// module context.
//
// Resolve returns a Go-error only on hard failures (package load
// failed, the loader returned no packages). Non-fatal per-symbol
// issues (opaque-only mapping, an uninstantiated generic) attach to
// PackageBinding.Errors.
func (r *Resolver) Resolve(importPath string) (*PackageBinding, error) {
	if r.Cache != nil {
		if pb, ok := r.Cache.Load(importPath, r.GoSumHash); ok {
			return pb, nil
		}
	}

	cfg := &packages.Config{
		Mode: packages.NeedTypes | packages.NeedTypesInfo | packages.NeedSyntax | packages.NeedDeps | packages.NeedImports | packages.NeedName,
	}
	pkgs, err := packages.Load(cfg, importPath)
	if err != nil {
		return nil, fmt.Errorf("resolve %q: packages.Load: %w", importPath, err)
	}
	if len(pkgs) == 0 {
		return nil, fmt.Errorf("resolve %q: no packages returned", importPath)
	}
	p := pkgs[0]
	if p.Types == nil {
		return nil, fmt.Errorf("resolve %q: no type info (%d errors)", importPath, len(p.Errors))
	}
	if p.Name == "" && len(p.Errors) > 0 {
		// packages.Load returns a stub package with errors for an
		// unresolvable import (no such package on disk, no module
		// found, etc.). Surface the first error verbatim.
		return nil, fmt.Errorf("resolve %q: %s", importPath, p.Errors[0].Msg)
	}
	if len(p.Types.Scope().Names()) == 0 && len(p.Errors) > 0 {
		return nil, fmt.Errorf("resolve %q: empty package (%d errors): %s", importPath, len(p.Errors), p.Errors[0].Msg)
	}

	pb := buildBinding(p, r.GoSumHash)
	if r.Cache != nil {
		_ = r.Cache.Store(pb)
	}
	return pb, nil
}

// buildBinding walks p.Types.Scope() once and produces the binding.
// It is pure given a *packages.Package; the on-disk cache wraps this.
func buildBinding(p *packages.Package, goSumHash string) *PackageBinding {
	pb := &PackageBinding{
		ImportPath:   p.PkgPath,
		Name:         p.Name,
		GoSumHash:    goSumHash,
		MochiVersion: MochiResolverVersion,
	}

	scope := p.Types.Scope()
	names := scope.Names()
	sort.Strings(names)

	pkgAlias := lastSegment(p.PkgPath)
	if p.Name != "" {
		pkgAlias = p.Name
	}

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
			fb, errs := buildFunc(o, pkgAlias, nil)
			pb.Funcs = append(pb.Funcs, fb)
			pb.Errors = append(pb.Errors, errs...)
		case *types.TypeName:
			tb, errs := buildTypeName(o, pkgAlias)
			pb.Types = append(pb.Types, tb)
			pb.Errors = append(pb.Errors, errs...)
		case *types.Var:
			vb, errs := buildVar(o, pkgAlias)
			pb.Vars = append(pb.Vars, vb)
			pb.Errors = append(pb.Errors, errs...)
		case *types.Const:
			cb, errs := buildConst(o, pkgAlias)
			pb.Consts = append(pb.Consts, cb)
			pb.Errors = append(pb.Errors, errs...)
		}
	}
	return pb
}

func buildFunc(o *types.Func, pkgAlias string, recv *typebridge.Type) (FuncBinding, []BindingError) {
	var errs []BindingError
	sig, _ := o.Type().(*types.Signature)
	var mt typebridge.Type
	if sig != nil {
		mt = typebridge.GoToMochi(sig)
	} else {
		mt = typebridge.GoToMochi(o.Type())
	}
	if mt.Kind == typebridge.KindOpaque && mt.GoType == "" {
		errs = append(errs, BindingError{Symbol: o.Name(), Reason: ErrOpaqueOnly})
	}
	fb := FuncBinding{
		Name:       o.Name(),
		Signature:  mt,
		GoCallExpr: pkgAlias + "." + o.Name(),
		Recv:       recv,
		Exported:   o.Exported(),
	}
	if sig != nil {
		fb.TypeParams, _ = collectTypeParams(sig.TypeParams())
		if sig.TypeParams() != nil && sig.TypeParams().Len() > 0 {
			errs = append(errs, BindingError{Symbol: o.Name(), Reason: ErrGenericNotInstantiated, Detail: fmt.Sprintf("%d type parameter(s)", sig.TypeParams().Len())})
		}
	}
	return fb, errs
}

func buildTypeName(o *types.TypeName, pkgAlias string) (TypeBinding, []BindingError) {
	var errs []BindingError
	t := o.Type()
	mt := typebridge.GoToMochi(t)
	tb := TypeBinding{
		Name:       o.Name(),
		PkgPath:    o.Pkg().Path(),
		Underlying: mt,
	}
	if iface, ok := t.Underlying().(*types.Interface); ok {
		_ = iface
		tb.IsInterface = true
	}
	if n, ok := t.(*types.Named); ok {
		tps, _ := collectTypeParams(n.TypeParams())
		tb.TypeParams = tps
		// Methods are already populated on mt.Methods via the bridge;
		// mirror them into the binding so consumers can look up methods
		// without re-walking the structural type.
		for _, m := range mt.Methods {
			recv := mt
			tb.Methods = append(tb.Methods, FuncBinding{
				Name:       m.Name,
				Signature:  m.Signature,
				GoCallExpr: pkgAlias + "." + o.Name() + "." + m.Name,
				Recv:       &recv,
				Exported:   m.Exported,
			})
		}
	}
	if mt.Kind == typebridge.KindOpaque && mt.GoType == "" {
		errs = append(errs, BindingError{Symbol: o.Name(), Reason: ErrOpaqueOnly})
	}
	return tb, errs
}

func buildVar(o *types.Var, pkgAlias string) (VarBinding, []BindingError) {
	var errs []BindingError
	mt := typebridge.GoToMochi(o.Type())
	if mt.Kind == typebridge.KindOpaque && mt.GoType == "" {
		errs = append(errs, BindingError{Symbol: o.Name(), Reason: ErrOpaqueOnly})
	}
	return VarBinding{
		Name:   o.Name(),
		Type:   mt,
		GoExpr: pkgAlias + "." + o.Name(),
	}, errs
}

func buildConst(o *types.Const, pkgAlias string) (ConstBinding, []BindingError) {
	var errs []BindingError
	mt := typebridge.GoToMochi(o.Type())
	if mt.Kind == typebridge.KindOpaque && mt.GoType == "" {
		errs = append(errs, BindingError{Symbol: o.Name(), Reason: ErrOpaqueOnly})
	}
	v := o.Val()
	val := ""
	if v != nil {
		val = v.ExactString()
	}
	return ConstBinding{
		Name:   o.Name(),
		Type:   mt,
		Value:  val,
		GoExpr: pkgAlias + "." + o.Name(),
	}, errs
}

func collectTypeParams(tps *types.TypeParamList) ([]TypeParamBinding, []BindingError) {
	var out []TypeParamBinding
	var errs []BindingError
	if tps == nil {
		return nil, nil
	}
	for i := 0; i < tps.Len(); i++ {
		tp := tps.At(i)
		constraint := tp.Constraint()
		mc := typebridge.GoToMochi(constraint)
		kind, ok := typebridge.LookupConstraint(constraint)
		out = append(out, TypeParamBinding{
			Name:       tp.Obj().Name(),
			Constraint: mc,
			Kind:       kind,
			KindKnown:  ok,
		})
		if !ok && mc.Kind != typebridge.KindIface {
			errs = append(errs, BindingError{Symbol: tp.Obj().Name(), Reason: ErrConstraintUnsupported})
		}
	}
	return out, errs
}

func lastSegment(importPath string) string {
	i := strings.LastIndex(importPath, "/")
	if i < 0 {
		return importPath
	}
	return importPath[i+1:]
}

// LookupFunc finds a func binding by name. Returns nil if not present.
func (pb *PackageBinding) LookupFunc(name string) *FuncBinding {
	for i := range pb.Funcs {
		if pb.Funcs[i].Name == name {
			return &pb.Funcs[i]
		}
	}
	return nil
}

// LookupType finds a type binding by name. Returns nil if not present.
func (pb *PackageBinding) LookupType(name string) *TypeBinding {
	for i := range pb.Types {
		if pb.Types[i].Name == name {
			return &pb.Types[i]
		}
	}
	return nil
}

// LookupVar finds a var binding by name. Returns nil if not present.
func (pb *PackageBinding) LookupVar(name string) *VarBinding {
	for i := range pb.Vars {
		if pb.Vars[i].Name == name {
			return &pb.Vars[i]
		}
	}
	return nil
}

// LookupConst finds a const binding by name. Returns nil if not present.
func (pb *PackageBinding) LookupConst(name string) *ConstBinding {
	for i := range pb.Consts {
		if pb.Consts[i].Name == name {
			return &pb.Consts[i]
		}
	}
	return nil
}

// Stats returns a small textual summary suitable for diagnostics.
func (pb *PackageBinding) Stats() string {
	return fmt.Sprintf("%s: %d funcs, %d types, %d vars, %d consts, %d warnings",
		pb.ImportPath, len(pb.Funcs), len(pb.Types), len(pb.Vars), len(pb.Consts), len(pb.Errors))
}

