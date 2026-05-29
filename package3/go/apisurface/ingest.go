package apisurface

import (
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
	"path/filepath"
	"runtime"
	"sort"
	"strings"

	"golang.org/x/tools/go/packages"
)

// IngestOptions controls Ingest.
type IngestOptions struct {
	// Module is the canonical module path the ingest is associated
	// with. Populates File.Module verbatim. Required.
	Module string
	// Version is the resolved version. Optional; left empty when
	// ingesting a source tree without a known semver.
	Version string
	// GeneratedBy is the producer identifier written into the JSON.
	// Defaults to "mochi-go-bridge go-ingest".
	GeneratedBy string
}

// Ingest converts a list of go/packages.Package values into a File
// ready for JSON serialisation. The packages are typically the
// result of packages.Load(LoadAllSyntax) over a module root.
//
// Ingest is pure (no I/O): all data is read from the loaded packages.
// The caller is responsible for invoking packages.Load.
func Ingest(pkgs []*packages.Package, opts IngestOptions) (*File, error) {
	if opts.Module == "" {
		return nil, fmt.Errorf("apisurface: ingest: Module is required")
	}
	out := &File{
		SchemaVersion: SchemaVersion,
		Module:        opts.Module,
		Version:       opts.Version,
		GoVersion:     runtime.Version(),
		GeneratedBy:   opts.GeneratedBy,
	}
	if out.GeneratedBy == "" {
		out.GeneratedBy = "mochi-go-bridge go-ingest"
	}
	for _, pkg := range pkgs {
		if pkg == nil || pkg.Types == nil {
			continue
		}
		ap, err := ingestPackage(pkg)
		if err != nil {
			return nil, fmt.Errorf("apisurface: ingest %s: %w", pkg.PkgPath, err)
		}
		out.Packages = append(out.Packages, *ap)
	}
	sort.Slice(out.Packages, func(i, j int) bool {
		return out.Packages[i].ImportPath < out.Packages[j].ImportPath
	})
	return out, nil
}

func ingestPackage(p *packages.Package) (*Package, error) {
	ap := &Package{
		ImportPath: p.PkgPath,
		Name:       p.Types.Name(),
		IsMain:     p.Types.Name() == "main",
	}
	if p.Fset == nil {
		p.Fset = token.NewFileSet()
	}
	docMap := collectDocs(p)
	ap.Doc = docMap.pkgDoc
	importSet := make(map[string]struct{})

	scope := p.Types.Scope()
	for _, name := range scope.Names() {
		obj := scope.Lookup(name)
		if !obj.Exported() {
			ap.Skipped = append(ap.Skipped, SkipNote{
				Kind:     objKind(obj),
				Name:     name,
				Position: posOf(p, obj.Pos()),
				Reason:   "Unexported",
			})
			continue
		}
		pos := posOf(p, obj.Pos())
		switch o := obj.(type) {
		case *types.Func:
			ap.Funcs = append(ap.Funcs, funcFromObj(o, pos, docMap.funcDoc[name], importSet))
		case *types.TypeName:
			t := typeFromObj(o, pos, docMap.typeDoc[name], p, importSet)
			ap.Types = append(ap.Types, t)
		case *types.Const:
			ap.Consts = append(ap.Consts, Value{
				Name:     name,
				Type:     typeString(o.Type(), p.PkgPath, importSet),
				Doc:      docMap.constDoc[name],
				Position: pos,
				Const:    o.Val().ExactString(),
			})
		case *types.Var:
			ap.Vars = append(ap.Vars, Value{
				Name:     name,
				Type:     typeString(o.Type(), p.PkgPath, importSet),
				Doc:      docMap.varDoc[name],
				Position: pos,
			})
		default:
			ap.Skipped = append(ap.Skipped, SkipNote{
				Kind:     "unknown",
				Name:     name,
				Position: pos,
				Reason:   "UnknownObject",
			})
		}
	}

	// Attach methods to types. types.NewMethodSet over each named
	// type's pointer or value receiver gathers everything declared
	// on it. We render the un-pointer'd version because the
	// receiver-pointer flag on Func captures that distinction.
	for i := range ap.Types {
		obj := scope.Lookup(ap.Types[i].Name)
		if obj == nil {
			continue
		}
		named, ok := obj.Type().(*types.Named)
		if !ok {
			continue
		}
		methods := []Func{}
		seen := map[string]bool{}
		for j := 0; j < named.NumMethods(); j++ {
			m := named.Method(j)
			if !m.Exported() {
				continue
			}
			if seen[m.Name()] {
				continue
			}
			seen[m.Name()] = true
			methods = append(methods, funcFromObj(m, posOf(p, m.Pos()), docMap.methodDoc[ap.Types[i].Name+"."+m.Name()], importSet))
		}
		sort.Slice(methods, func(a, b int) bool { return methods[a].Name < methods[b].Name })
		ap.Types[i].Methods = methods
	}

	// Sorted imports of the exported surface.
	if len(importSet) > 0 {
		ap.Imports = make([]string, 0, len(importSet))
		for k := range importSet {
			ap.Imports = append(ap.Imports, k)
		}
		sort.Strings(ap.Imports)
	}
	// Deterministic ordering across the rest of the surface too.
	sort.Slice(ap.Funcs, func(i, j int) bool { return ap.Funcs[i].Name < ap.Funcs[j].Name })
	sort.Slice(ap.Types, func(i, j int) bool { return ap.Types[i].Name < ap.Types[j].Name })
	sort.Slice(ap.Consts, func(i, j int) bool { return ap.Consts[i].Name < ap.Consts[j].Name })
	sort.Slice(ap.Vars, func(i, j int) bool { return ap.Vars[i].Name < ap.Vars[j].Name })
	sort.Slice(ap.Skipped, func(i, j int) bool {
		if ap.Skipped[i].Name != ap.Skipped[j].Name {
			return ap.Skipped[i].Name < ap.Skipped[j].Name
		}
		return ap.Skipped[i].Kind < ap.Skipped[j].Kind
	})
	return ap, nil
}

// objKind maps a types.Object to a SkipNote.Kind token.
func objKind(obj types.Object) string {
	switch obj.(type) {
	case *types.Func:
		return "func"
	case *types.TypeName:
		return "type"
	case *types.Const:
		return "const"
	case *types.Var:
		return "var"
	default:
		return "unknown"
	}
}

func funcFromObj(o *types.Func, pos, doc string, importSet map[string]struct{}) Func {
	sig := o.Type().(*types.Signature)
	out := Func{
		Name:     o.Name(),
		Doc:      doc,
		Position: pos,
		Variadic: sig.Variadic(),
	}
	if recv := sig.Recv(); recv != nil {
		out.Receiver, out.ReceiverPointer = receiverName(recv.Type())
	}
	if tp := sig.TypeParams(); tp != nil {
		out.TypeParams = make([]TypeParam, tp.Len())
		for i := range out.TypeParams {
			tparam := tp.At(i)
			out.TypeParams[i] = TypeParam{
				Name:       tparam.Obj().Name(),
				Constraint: typeString(tparam.Constraint(), "", importSet),
			}
		}
	}
	for i := 0; i < sig.Params().Len(); i++ {
		p := sig.Params().At(i)
		out.Params = append(out.Params, Param{
			Name: p.Name(),
			Type: typeString(p.Type(), o.Pkg().Path(), importSet),
		})
	}
	for i := 0; i < sig.Results().Len(); i++ {
		p := sig.Results().At(i)
		out.Results = append(out.Results, Param{
			Name: p.Name(),
			Type: typeString(p.Type(), o.Pkg().Path(), importSet),
		})
	}
	return out
}

func typeFromObj(o *types.TypeName, pos, doc string, p *packages.Package, importSet map[string]struct{}) Type {
	t := Type{
		Name:     o.Name(),
		Position: pos,
		Doc:      doc,
	}
	if o.IsAlias() {
		t.Kind = KindAlias
		t.AliasOf = typeString(types.Unalias(o.Type()), p.PkgPath, importSet)
		return t
	}
	named, ok := o.Type().(*types.Named)
	if !ok {
		t.Kind = KindNamed
		t.Underlying = typeString(o.Type(), p.PkgPath, importSet)
		return t
	}
	if tp := named.TypeParams(); tp != nil {
		t.TypeParams = make([]TypeParam, tp.Len())
		for i := range t.TypeParams {
			tparam := tp.At(i)
			t.TypeParams[i] = TypeParam{
				Name:       tparam.Obj().Name(),
				Constraint: typeString(tparam.Constraint(), "", importSet),
			}
		}
	}
	u := named.Underlying()
	t.Underlying = typeString(u, p.PkgPath, importSet)
	switch ut := u.(type) {
	case *types.Struct:
		t.Kind = KindStruct
		for i := 0; i < ut.NumFields(); i++ {
			f := ut.Field(i)
			fld := Field{
				Name:     f.Name(),
				Type:     typeString(f.Type(), p.PkgPath, importSet),
				Tag:      ut.Tag(i),
				Exported: f.Exported(),
				Embedded: f.Embedded(),
			}
			t.Fields = append(t.Fields, fld)
		}
	case *types.Interface:
		t.Kind = KindInterface
		for i := 0; i < ut.NumExplicitMethods(); i++ {
			m := ut.ExplicitMethod(i)
			t.InterfaceMethods = append(t.InterfaceMethods, funcFromObj(m, posOf(p, m.Pos()), "", importSet))
		}
		for i := 0; i < ut.NumEmbeddeds(); i++ {
			e := ut.EmbeddedType(i)
			t.EmbeddedTypes = append(t.EmbeddedTypes, typeString(e, p.PkgPath, importSet))
		}
	case *types.Basic:
		t.Kind = KindBasic
	case *types.Slice:
		t.Kind = KindSlice
	case *types.Map:
		t.Kind = KindMap
	case *types.Chan:
		t.Kind = KindChan
	case *types.Signature:
		t.Kind = KindFunc
	case *types.Array:
		t.Kind = KindArray
	case *types.Pointer:
		t.Kind = KindPointer
	default:
		t.Kind = KindNamed
	}
	return t
}

// receiverName extracts the named-type name from a method receiver
// type. Returns the name and whether the receiver was a pointer
// type. For an unexpected shape, returns "" and false.
func receiverName(t types.Type) (string, bool) {
	if ptr, ok := t.(*types.Pointer); ok {
		if named, ok := ptr.Elem().(*types.Named); ok {
			return named.Obj().Name(), true
		}
		return "", true
	}
	if named, ok := t.(*types.Named); ok {
		return named.Obj().Name(), false
	}
	return "", false
}

// typeString renders a go/types.Type as its source-level string,
// with import paths rewritten to be valid in the *consuming*
// context. References to types defined in selfPkg are rendered
// without a package qualifier; references to types from other
// packages are rendered with their full import path (e.g.
// "io.Reader" → "io.Reader", "github.com/foo/bar.Baz" → that path).
//
// The importSet is populated as a side effect with every external
// package referenced by the rendered type.
func typeString(t types.Type, selfPkg string, importSet map[string]struct{}) string {
	if t == nil {
		return ""
	}
	// Resolve untyped-basic constants to their default type so the
	// rendered string matches the closed grammar of typeexpr. For
	// example, an untyped string constant renders as "string", not
	// "untyped string".
	if b, ok := t.(*types.Basic); ok && b.Info()&types.IsUntyped != 0 {
		t = types.Default(t)
		if b2, ok2 := t.(*types.Basic); ok2 && b2.Info()&types.IsUntyped != 0 {
			// types.Default returned an untyped value (e.g. for
			// untyped nil); strip the "untyped " prefix manually.
			return strings.TrimPrefix(b2.Name(), "untyped ")
		}
	}
	q := func(p *types.Package) string {
		if p == nil {
			return ""
		}
		if p.Path() == selfPkg {
			return ""
		}
		if importSet != nil {
			importSet[p.Path()] = struct{}{}
		}
		return p.Path()
	}
	return types.TypeString(t, q)
}

// posOf renders an obj.Pos() as "file:line:col" relative to the
// package directory. Returns "" if the position is invalid.
func posOf(p *packages.Package, pos token.Pos) string {
	if !pos.IsValid() || p.Fset == nil {
		return ""
	}
	fp := p.Fset.Position(pos)
	if !fp.IsValid() {
		return ""
	}
	rel := fp.Filename
	if p.Dir != "" && strings.HasPrefix(rel, p.Dir) {
		rel = strings.TrimPrefix(rel, p.Dir+string(filepath.Separator))
	}
	return fmt.Sprintf("%s:%d:%d", rel, fp.Line, fp.Column)
}

// docCollector accumulates godoc strings for top-level declarations
// in a package. The fields are populated by walking each file's AST.
type docCollector struct {
	pkgDoc    string
	funcDoc   map[string]string
	typeDoc   map[string]string
	constDoc  map[string]string
	varDoc    map[string]string
	methodDoc map[string]string // "TypeName.MethodName" -> doc
}

func collectDocs(p *packages.Package) *docCollector {
	dc := &docCollector{
		funcDoc:   make(map[string]string),
		typeDoc:   make(map[string]string),
		constDoc:  make(map[string]string),
		varDoc:    make(map[string]string),
		methodDoc: make(map[string]string),
	}
	for _, f := range p.Syntax {
		if f.Doc != nil && dc.pkgDoc == "" {
			dc.pkgDoc = strings.TrimSpace(f.Doc.Text())
		}
		for _, decl := range f.Decls {
			switch d := decl.(type) {
			case *ast.FuncDecl:
				if d.Doc == nil {
					continue
				}
				doc := strings.TrimSpace(d.Doc.Text())
				if d.Recv != nil && len(d.Recv.List) == 1 {
					rt := recvTypeName(d.Recv.List[0].Type)
					if rt != "" {
						dc.methodDoc[rt+"."+d.Name.Name] = doc
					}
				} else {
					dc.funcDoc[d.Name.Name] = doc
				}
			case *ast.GenDecl:
				groupDoc := ""
				if d.Doc != nil {
					groupDoc = strings.TrimSpace(d.Doc.Text())
				}
				for _, spec := range d.Specs {
					switch s := spec.(type) {
					case *ast.TypeSpec:
						doc := groupDoc
						if s.Doc != nil {
							doc = strings.TrimSpace(s.Doc.Text())
						}
						if doc != "" {
							dc.typeDoc[s.Name.Name] = doc
						}
					case *ast.ValueSpec:
						doc := groupDoc
						if s.Doc != nil {
							doc = strings.TrimSpace(s.Doc.Text())
						}
						for _, name := range s.Names {
							if doc == "" {
								continue
							}
							if d.Tok == token.CONST {
								dc.constDoc[name.Name] = doc
							} else if d.Tok == token.VAR {
								dc.varDoc[name.Name] = doc
							}
						}
					}
				}
			}
		}
	}
	return dc
}

func recvTypeName(e ast.Expr) string {
	switch t := e.(type) {
	case *ast.Ident:
		return t.Name
	case *ast.StarExpr:
		return recvTypeName(t.X)
	case *ast.IndexExpr: // generic, e.g. Foo[T]
		return recvTypeName(t.X)
	case *ast.IndexListExpr: // generic, e.g. Foo[T, U]
		return recvTypeName(t.X)
	}
	return ""
}
