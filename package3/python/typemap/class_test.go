package typemap

import (
	"testing"

	"mochi/package3/python/errors"
	"mochi/package3/python/stubs"
)

func TestMapTypedDict(t *testing.T) {
	m := &Mapper{}
	cd := stubs.ClassDecl{
		Name:        "Point",
		IsTypedDict: true,
		Fields: []stubs.FieldDecl{
			{Name: "x", Type: "int"},
			{Name: "y", Type: "int"},
		},
	}
	d := m.MapClass(cd)
	if !d.OK() {
		t.Fatalf("skipped: %v", d.Skip)
	}
	if d.Type.Kind != KindRecord || d.Type.Name != "Point" {
		t.Errorf("kind/name = %+v", d.Type)
	}
	if len(d.Type.Fields) != 2 {
		t.Errorf("fields = %+v", d.Type.Fields)
	}
}

func TestMapFrozenDataclass(t *testing.T) {
	m := &Mapper{}
	cd := stubs.ClassDecl{
		Name:        "Point",
		IsDataclass: true,
		Decorators:  []string{"dataclass(frozen=True)"},
		Fields: []stubs.FieldDecl{
			{Name: "x", Type: "int"},
			{Name: "y", Type: "int"},
		},
	}
	d := m.MapClass(cd)
	if !d.OK() {
		t.Fatalf("skipped: %v", d.Skip)
	}
	if d.Type.Kind != KindRecord {
		t.Errorf("got %+v", d.Type)
	}
}

func TestMapMutableDataclassRefused(t *testing.T) {
	m := &Mapper{}
	cd := stubs.ClassDecl{
		Name:        "Point",
		IsDataclass: true,
		Decorators:  []string{"dataclass"},
		Fields:      []stubs.FieldDecl{{Name: "x", Type: "int"}},
	}
	d := m.MapClass(cd)
	if d.OK() {
		t.Fatal("expected refusal: mutable @dataclass")
	}
	if d.Skip.Override == "" {
		t.Error("expected override suggestion in skip report")
	}
}

func TestMapFrozenDataclassWithWhitespace(t *testing.T) {
	m := &Mapper{}
	cd := stubs.ClassDecl{
		Name:        "Point",
		IsDataclass: true,
		Decorators:  []string{"dataclasses.dataclass(  frozen = True  )"},
	}
	d := m.MapClass(cd)
	if !d.OK() {
		t.Errorf("whitespace-tolerant detection failed: %v", d.Skip)
	}
}

func TestMapProtocol(t *testing.T) {
	m := &Mapper{}
	cd := stubs.ClassDecl{
		Name:       "Greeter",
		IsProtocol: true,
		Methods: []stubs.FuncDecl{
			{
				Name: "hello",
				Params: []stubs.ParamDecl{
					{Name: "self"},
					{Name: "who", Type: "str"},
				},
				ReturnType: "str",
			},
		},
	}
	d := m.MapClass(cd)
	if !d.OK() {
		t.Fatalf("skipped: %v", d.Skip)
	}
	if d.Type.Kind != KindInterface || d.Type.Name != "Greeter" {
		t.Errorf("got %+v", d.Type)
	}
	if len(d.Type.Methods) != 1 {
		t.Errorf("methods = %+v", d.Type.Methods)
	}
}

func TestMapProtocolSkipsDunder(t *testing.T) {
	m := &Mapper{}
	cd := stubs.ClassDecl{
		Name:       "P",
		IsProtocol: true,
		Methods: []stubs.FuncDecl{
			{Name: "__init__", Params: []stubs.ParamDecl{{Name: "self"}}, ReturnType: "None"},
			{Name: "ok", Params: []stubs.ParamDecl{{Name: "self"}}, ReturnType: "int"},
		},
	}
	d := m.MapClass(cd)
	if !d.OK() {
		t.Fatalf("skipped: %v", d.Skip)
	}
	if len(d.Type.Methods) != 1 {
		t.Errorf("methods = %+v (dunders should be skipped)", d.Type.Methods)
	}
	if len(d.Skipped) != 1 || d.Skipped[0].Reason != errors.SkipDunder {
		t.Errorf("Skipped = %+v", d.Skipped)
	}
}

func TestMapProtocolRefusesVarArgs(t *testing.T) {
	m := &Mapper{}
	cd := stubs.ClassDecl{
		Name:       "P",
		IsProtocol: true,
		Methods: []stubs.FuncDecl{
			{
				Name: "fmt",
				Params: []stubs.ParamDecl{
					{Name: "self"},
					{Name: "args", Kind: stubs.ParamVarArgs, Type: "Any"},
				},
				ReturnType: "str",
			},
		},
	}
	d := m.MapClass(cd)
	if !d.OK() {
		t.Fatalf("skipped: %v", d.Skip)
	}
	if len(d.Skipped) != 1 || d.Skipped[0].Reason != errors.SkipParamSpec {
		t.Errorf("Skipped = %+v", d.Skipped)
	}
}

func TestMapProtocolUnannotatedParamSkipped(t *testing.T) {
	m := &Mapper{}
	cd := stubs.ClassDecl{
		Name:       "P",
		IsProtocol: true,
		Methods: []stubs.FuncDecl{
			{
				Name: "go",
				Params: []stubs.ParamDecl{
					{Name: "self"},
					{Name: "x"}, // no annotation
				},
				ReturnType: "int",
			},
		},
	}
	d := m.MapClass(cd)
	if !d.OK() {
		t.Fatalf("skipped: %v", d.Skip)
	}
	if len(d.Skipped) != 1 {
		t.Errorf("Skipped = %+v", d.Skipped)
	}
}

func TestMapPlainClassRefused(t *testing.T) {
	m := &Mapper{}
	cd := stubs.ClassDecl{Name: "Plain"}
	d := m.MapClass(cd)
	if d.OK() {
		t.Fatal("expected refusal: plain class")
	}
}

func TestMapTypedDictFieldSkipped(t *testing.T) {
	m := &Mapper{}
	cd := stubs.ClassDecl{
		Name:        "Mixed",
		IsTypedDict: true,
		Fields: []stubs.FieldDecl{
			{Name: "x", Type: "int"},
			{Name: "y", Type: "complex"}, // refusal
		},
	}
	d := m.MapClass(cd)
	if !d.OK() {
		t.Fatalf("class skipped: %v", d.Skip)
	}
	if len(d.Type.Fields) != 1 {
		t.Errorf("fields = %+v (complex should be skipped)", d.Type.Fields)
	}
	if len(d.Skipped) != 1 || d.Skipped[0].Reason != errors.SkipNoComplexType {
		t.Errorf("Skipped = %+v", d.Skipped)
	}
}

func TestMapFunction(t *testing.T) {
	m := &Mapper{}
	fn := stubs.FuncDecl{
		Name: "add",
		Params: []stubs.ParamDecl{
			{Name: "a", Type: "int"},
			{Name: "b", Type: "int"},
		},
		ReturnType: "int",
	}
	d := m.MapFunction(fn)
	if !d.OK() {
		t.Fatalf("skipped: %v", d.Skip)
	}
	if got := d.Type.Render(); got != "fun(int, int): int" {
		t.Errorf("got %q", got)
	}
}

func TestMapFunctionAsync(t *testing.T) {
	m := &Mapper{}
	fn := stubs.FuncDecl{
		Name:       "fetch",
		IsAsync:    true,
		Params:     []stubs.ParamDecl{{Name: "url", Type: "str"}},
		ReturnType: "bytes",
	}
	d := m.MapFunction(fn)
	if !d.OK() {
		t.Fatalf("skipped: %v", d.Skip)
	}
	if got := d.Type.Render(); got != "fun(string): async bytes" {
		t.Errorf("got %q", got)
	}
}

func TestMapFunctionNoReturnDefaultsToNone(t *testing.T) {
	m := &Mapper{}
	fn := stubs.FuncDecl{Name: "noop"}
	d := m.MapFunction(fn)
	if !d.OK() {
		t.Fatalf("skipped: %v", d.Skip)
	}
	if got := d.Type.Render(); got != "fun(): None" {
		t.Errorf("got %q", got)
	}
}

func TestMapFunctionVarArgsRefused(t *testing.T) {
	m := &Mapper{}
	fn := stubs.FuncDecl{
		Name: "fmt",
		Params: []stubs.ParamDecl{
			{Name: "args", Kind: stubs.ParamVarArgs, Type: "Any"},
		},
		ReturnType: "str",
	}
	d := m.MapFunction(fn)
	if d.OK() {
		t.Fatal("expected refusal")
	}
	if d.Skip.Reason != errors.SkipParamSpec {
		t.Errorf("reason = %v", d.Skip.Reason)
	}
}

func TestClassDecoratedAsFrozen(t *testing.T) {
	cases := []struct {
		decorators []string
		want       bool
	}{
		{nil, false},
		{[]string{"dataclass"}, false},
		{[]string{"dataclass(frozen=True)"}, true},
		{[]string{"dataclass( frozen = True )"}, true},
		{[]string{"dataclasses.dataclass(frozen=True)"}, true},
		{[]string{"dataclass(eq=True, frozen=True)"}, true},
		{[]string{"dataclass(frozen=False)"}, false},
		{[]string{"property"}, false},
	}
	for _, c := range cases {
		got := classDecoratedAsFrozen(stubs.ClassDecl{Decorators: c.decorators})
		if got != c.want {
			t.Errorf("decorators=%v: got %v, want %v", c.decorators, got, c.want)
		}
	}
}
