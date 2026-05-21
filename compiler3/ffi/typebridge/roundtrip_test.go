package typebridge

import (
	"go/types"
	"strings"
	"testing"

	"golang.org/x/tools/go/packages"
)

// lastSegmentQualifier produces the same package qualifier MochiToGo
// uses (the last component of the import path).
func lastSegmentQualifier(p *types.Package) string {
	if p == nil {
		return ""
	}
	path := p.Path()
	if i := strings.LastIndex(path, "/"); i >= 0 {
		return path[i+1:]
	}
	return path
}

// loadSyntheticPackage builds a package that declares one variable
// per shape we care about, runs go/types over it, and returns the
// resulting types.Package. The test then iterates Scope().Names()
// and asserts that MochiToGo(GoToMochi(t)) == types.TypeString(t,
// lastSegmentQualifier) for every var.
func loadSyntheticPackage(t *testing.T) *types.Package {
	t.Helper()
	src := `package syn

import (
	"io"
	"os"
	"strings"
	"time"
)

type Node struct {
	Value    int
	Children []*Node
}

type Pair[K comparable, V any] struct {
	Key   K
	Value V
}

var (
	VBool       bool
	VInt        int
	VInt8       int8
	VInt16      int16
	VInt32      int32
	VInt64      int64
	VUint       uint
	VUint8      uint8
	VUint16     uint16
	VUint32     uint32
	VUint64     uint64
	VFloat32    float32
	VFloat64    float64
	VString     string
	VBytes      []byte
	VListInt    []int
	VListStr    []string
	VArr3F32    [3]float32
	VMapSI      map[string]int
	VRefInt     *int
	VChanInt    chan int
	VSendInt    chan<- int
	VRecvInt    <-chan int
	VFuncAdd    func(int, int) int
	VFuncVar    func(...int)
	VFuncMulti  func(string) (int, error)
	VAny        any
	VError      error
	VIoReader   io.Reader
	VFile       *os.File
	VReader     *strings.Reader
	VTime       time.Time
	VNode       *Node
	VPair       Pair[string, int]
	VEmptyStr   struct{}
	VInlineStr  struct {
		A string
		B int ` + "`json:\"b\"`" + `
	}
)
`
	dir := t.TempDir()
	gomod := "module syn\n\ngo 1.22\n"
	if err := writeFile(dir+"/go.mod", gomod); err != nil {
		t.Fatal(err)
	}
	if err := writeFile(dir+"/syn.go", src); err != nil {
		t.Fatal(err)
	}
	cfg := &packages.Config{
		Mode: packages.NeedTypes | packages.NeedTypesInfo | packages.NeedSyntax | packages.NeedDeps | packages.NeedImports | packages.NeedName | packages.NeedFiles,
		Dir:  dir,
	}
	pkgs, err := packages.Load(cfg, ".")
	if err != nil {
		t.Fatalf("packages.Load: %v", err)
	}
	if len(pkgs) == 0 {
		t.Fatal("no packages loaded")
	}
	for _, p := range pkgs {
		if len(p.Errors) > 0 {
			for _, e := range p.Errors {
				t.Logf("pkg %s err: %v", p.PkgPath, e)
			}
		}
	}
	if pkgs[0].Types == nil {
		t.Fatal("first package has no types")
	}
	return pkgs[0].Types
}

func writeFile(path, content string) error {
	return writeFileImpl(path, content)
}

// TestRoundTripIdentity asserts that for every variable in the
// synthetic package, MochiToGo(GoToMochi(t)) == types.TypeString(t,
// lastSegmentQualifier). This is the bridge's correctness contract
// (MEP-44 §5).
func TestRoundTripIdentity(t *testing.T) {
	pkg := loadSyntheticPackage(t)
	scope := pkg.Scope()
	exceptions := map[string]string{
		// none expected
	}
	for _, name := range scope.Names() {
		if !strings.HasPrefix(name, "V") {
			continue
		}
		obj := scope.Lookup(name)
		if obj == nil {
			t.Fatalf("scope lookup %s: nil", name)
		}
		gt := obj.Type()
		mt := GoToMochi(gt)
		if mt.Kind == KindInvalid {
			t.Errorf("%s: KindInvalid", name)
			continue
		}
		got := MochiToGo(mt)
		want := types.TypeString(gt, lastSegmentQualifier)
		if ex, ok := exceptions[name]; ok {
			if got != ex {
				t.Errorf("%s: %s -> %q, expected exception %q", name, want, got, ex)
			}
			continue
		}
		if got != want {
			t.Errorf("%s:\n  GoToMochi: %+v\n  MochiToGo: %q\n  want:      %q", name, mt, got, want)
		}
	}
}

// TestStructInlineFieldsPreserveTag confirms that struct tags
// survive the bridge unchanged.
func TestStructInlineFieldsPreserveTag(t *testing.T) {
	pkg := loadSyntheticPackage(t)
	obj := pkg.Scope().Lookup("VInlineStr")
	if obj == nil {
		t.Fatal("VInlineStr missing")
	}
	mt := GoToMochi(obj.Type())
	if mt.Kind != KindStruct {
		t.Fatalf("kind = %s", mt.Kind)
	}
	if len(mt.Fields) != 2 {
		t.Fatalf("fields = %d", len(mt.Fields))
	}
	if mt.Fields[1].Tag != `json:"b"` {
		t.Fatalf("tag = %q", mt.Fields[1].Tag)
	}
}
