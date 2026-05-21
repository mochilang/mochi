package typebridge

import (
	"go/types"
	"testing"

	"golang.org/x/tools/go/packages"
)

// loadStdPackages loads a small list of std packages and returns
// them. Cached across tests in the same binary run.
func loadStdPackages(t *testing.T, paths ...string) []*types.Package {
	t.Helper()
	cfg := &packages.Config{
		Mode: packages.NeedTypes | packages.NeedTypesInfo | packages.NeedSyntax | packages.NeedDeps | packages.NeedImports | packages.NeedName,
	}
	pkgs, err := packages.Load(cfg, paths...)
	if err != nil {
		t.Fatalf("packages.Load(%v): %v", paths, err)
	}
	out := make([]*types.Package, 0, len(pkgs))
	for _, p := range pkgs {
		if p.Types == nil {
			t.Fatalf("package %s: no types", p.PkgPath)
		}
		if len(p.Errors) > 0 {
			for _, e := range p.Errors {
				t.Logf("pkg %s err: %v", p.PkgPath, e)
			}
		}
		out = append(out, p.Types)
	}
	return out
}

// findExportedType walks pkg.Scope() and returns the named type with
// the given name (must be exported).
func findExportedType(t *testing.T, pkg *types.Package, name string) types.Type {
	t.Helper()
	obj := pkg.Scope().Lookup(name)
	if obj == nil {
		t.Fatalf("%s.%s: not found", pkg.Path(), name)
	}
	return obj.Type()
}

// TestNamedOsFile confirms that *os.File maps to a KindRef of a
// KindNamed with a non-empty method set including Close, Read, Write.
func TestNamedOsFile(t *testing.T) {
	pkgs := loadStdPackages(t, "os")
	file := findExportedType(t, pkgs[0], "File")
	ptr := types.NewPointer(file)
	mt := GoToMochi(ptr)
	if mt.Kind != KindRef || mt.Elem == nil || mt.Elem.Kind != KindNamed {
		t.Fatalf("*os.File -> %+v", mt)
	}
	n := *mt.Elem
	if n.Name != "File" || n.PkgPath != "os" {
		t.Fatalf("named id = %s/%s", n.PkgPath, n.Name)
	}
	required := []string{"Close", "Read", "Write", "Name"}
	got := map[string]bool{}
	for _, m := range n.Methods {
		got[m.Name] = true
	}
	for _, name := range required {
		if !got[name] {
			t.Errorf("*os.File missing method %s; got methods: %v", name, methodNames(n.Methods))
		}
	}
}

// TestNamedIoReader confirms that io.Reader maps to a KindIface
// named "Reader" with PkgPath "io" and one method Read.
func TestNamedIoReader(t *testing.T) {
	pkgs := loadStdPackages(t, "io")
	rdr := findExportedType(t, pkgs[0], "Reader")
	mt := GoToMochi(rdr)
	if mt.Kind != KindIface {
		t.Fatalf("io.Reader -> %s", mt.Kind)
	}
	if mt.Name != "Reader" || mt.PkgPath != "io" {
		t.Fatalf("identity = %s/%s", mt.PkgPath, mt.Name)
	}
	if len(mt.Methods) != 1 || mt.Methods[0].Name != "Read" {
		t.Fatalf("methods = %v", methodNames(mt.Methods))
	}
	sig := mt.Methods[0].Signature
	if sig.Kind != KindFunc {
		t.Fatalf("sig kind = %s", sig.Kind)
	}
	if len(sig.Params) != 1 || sig.Params[0].Kind != KindBytes {
		t.Fatalf("params = %+v", sig.Params)
	}
	if len(sig.Results) != 2 || sig.Results[0].Kind != KindInt {
		t.Fatalf("results = %+v", sig.Results)
	}
	// second result is `error`, a named interface
	if sig.Results[1].Kind != KindIface || sig.Results[1].Name != "error" {
		t.Fatalf("err result = %+v", sig.Results[1])
	}
}

// TestNamedTimeTime confirms time.Time produces a KindNamed with
// methods including Add, Sub, Unix, String, UnmarshalJSON.
func TestNamedTimeTime(t *testing.T) {
	pkgs := loadStdPackages(t, "time")
	tm := findExportedType(t, pkgs[0], "Time")
	mt := GoToMochi(tm)
	if mt.Kind != KindNamed {
		t.Fatalf("time.Time -> %s", mt.Kind)
	}
	if mt.PkgPath != "time" || mt.Name != "Time" {
		t.Fatalf("id = %s/%s", mt.PkgPath, mt.Name)
	}
	required := []string{"Add", "Sub", "Unix", "String", "MarshalJSON", "UnmarshalJSON"}
	got := map[string]bool{}
	for _, m := range mt.Methods {
		got[m.Name] = true
	}
	for _, r := range required {
		if !got[r] {
			t.Errorf("time.Time missing method %s; got %v", r, methodNames(mt.Methods))
		}
	}
}

// TestNamedErrorBuiltin confirms the builtin `error` interface maps
// to KindIface{Name:"error"} with a single Error() string method.
// `error` is in the universe scope, not a package scope.
func TestNamedErrorBuiltin(t *testing.T) {
	errObj := types.Universe.Lookup("error")
	if errObj == nil {
		t.Fatal("universe has no error type")
	}
	mt := GoToMochi(errObj.Type())
	if mt.Kind != KindIface {
		t.Fatalf("error kind = %s", mt.Kind)
	}
	if mt.Name != "error" {
		t.Fatalf("error name = %q", mt.Name)
	}
	if len(mt.Methods) != 1 || mt.Methods[0].Name != "Error" {
		t.Fatalf("methods = %v", methodNames(mt.Methods))
	}
}

// TestRecursiveNamedTerminates exercises the seen-map guard. We
// build a self-referential struct via a synthetic package and assert
// goToMochi returns rather than overflows the stack.
func TestRecursiveNamedTerminates(t *testing.T) {
	pkgs := loadStdPackages(t, "container/list")
	elem := findExportedType(t, pkgs[0], "Element")
	mt := GoToMochi(elem)
	// We do not assert structural depth here; we only assert termination
	// and the right top-level kind.
	if mt.Kind != KindNamed {
		t.Fatalf("Element kind = %s", mt.Kind)
	}
}

func methodNames(ms []Method) []string {
	out := make([]string, len(ms))
	for i, m := range ms {
		out[i] = m.Name
	}
	return out
}
