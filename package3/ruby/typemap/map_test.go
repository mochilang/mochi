package typemap

import (
	"strings"
	"testing"

	pkgerr "mochi/package3/ruby/errors"
	"mochi/package3/ruby/rbs"
)

// helpers for building test fixtures.
func typeOf(k rbs.TypeKind) *rbs.Type { return &rbs.Type{Kind: k} }
func arrayOf(elem rbs.TypeKind) *rbs.Type {
	return &rbs.Type{Kind: rbs.TypeArray, Elem: typeOf(elem)}
}
func hashOf(k, v rbs.TypeKind) *rbs.Type {
	return &rbs.Type{Kind: rbs.TypeHash, Key: typeOf(k), Value: typeOf(v)}
}
func optionalOf(elem rbs.TypeKind) *rbs.Type {
	return &rbs.Type{Kind: rbs.TypeOptional, Elem: typeOf(elem)}
}
func tupleOf(kinds ...rbs.TypeKind) *rbs.Type {
	members := make([]*rbs.Type, len(kinds))
	for i, k := range kinds {
		members[i] = typeOf(k)
	}
	return &rbs.Type{Kind: rbs.TypeTuple, Members: members}
}
func procOf(ret rbs.TypeKind, params ...rbs.TypeKind) *rbs.Type {
	ps := make([]*rbs.Type, len(params))
	for i, k := range params {
		ps[i] = typeOf(k)
	}
	return &rbs.Type{Kind: rbs.TypeProc, Params: ps, Return: typeOf(ret)}
}
func namedType(name string) *rbs.Type {
	return &rbs.Type{Kind: rbs.TypeNamed, Name: name}
}

func TestTranslate(t *testing.T) {
	// wantSkip=true means we expect a SkipReport (skipReason used only when wantSkip=true).
	// wantSkip=false means we expect a successful translation matching want.
	cases := []struct {
		name       string
		rbs        *rbs.Type
		want       string
		wantSkip   bool
		skipReason pkgerr.SkipReason
	}{
		// Scalars
		{"nil_input", nil, "unit", false, 0},
		{"integer", typeOf(rbs.TypeInteger), "int", false, 0},
		{"float", typeOf(rbs.TypeFloat), "float", false, 0},
		{"string", typeOf(rbs.TypeString), "string", false, 0},
		{"symbol", typeOf(rbs.TypeSymbol), "string", false, 0},
		{"bool", typeOf(rbs.TypeBool), "bool", false, 0},
		{"nil_type", typeOf(rbs.TypeNil), "nil", false, 0},
		{"void", typeOf(rbs.TypeVoid), "unit", false, 0},

		// Array
		{"array_string", arrayOf(rbs.TypeString), "list<string>", false, 0},
		{"array_int", arrayOf(rbs.TypeInteger), "list<int>", false, 0},
		{"array_float", arrayOf(rbs.TypeFloat), "list<float>", false, 0},
		{"array_bool", arrayOf(rbs.TypeBool), "list<bool>", false, 0},
		{"array_no_elem", &rbs.Type{Kind: rbs.TypeArray}, "", true, pkgerr.SkipUntyped},

		// Hash
		{"hash_str_int", hashOf(rbs.TypeString, rbs.TypeInteger), "map<string, int>", false, 0},
		{"hash_sym_str", hashOf(rbs.TypeSymbol, rbs.TypeString), "map<string, string>", false, 0},
		{"hash_str_bool", hashOf(rbs.TypeString, rbs.TypeBool), "map<string, bool>", false, 0},
		{"hash_no_key", &rbs.Type{Kind: rbs.TypeHash, Value: typeOf(rbs.TypeString)}, "", true, pkgerr.SkipUntyped},

		// Optional
		{"optional_str", optionalOf(rbs.TypeString), "string?", false, 0},
		{"optional_int", optionalOf(rbs.TypeInteger), "int?", false, 0},
		{"optional_float", optionalOf(rbs.TypeFloat), "float?", false, 0},
		{"optional_bool", optionalOf(rbs.TypeBool), "bool?", false, 0},
		{"optional_no_elem", &rbs.Type{Kind: rbs.TypeOptional}, "", true, pkgerr.SkipComplexUnion},

		// Tuple
		{"tuple_str_int", tupleOf(rbs.TypeString, rbs.TypeInteger), "tuple<string, int>", false, 0},
		{"tuple_three", tupleOf(rbs.TypeString, rbs.TypeInteger, rbs.TypeFloat), "tuple<string, int, float>", false, 0},
		{"tuple_empty", &rbs.Type{Kind: rbs.TypeTuple, Members: nil}, "", true, pkgerr.SkipUntyped},

		// Proc
		{"proc_str_ret", procOf(rbs.TypeString, rbs.TypeInteger), "fun(int): string", false, 0},
		{"proc_no_params", procOf(rbs.TypeBool), "fun(): bool", false, 0},
		{"proc_void_ret", procOf(rbs.TypeVoid, rbs.TypeString), "fun(string): unit", false, 0},

		// Refused types
		{"untyped", typeOf(rbs.TypeUntyped), "", true, pkgerr.SkipUntyped},
		{"top", typeOf(rbs.TypeTop), "", true, pkgerr.SkipTopBot},
		{"bot", typeOf(rbs.TypeBot), "", true, pkgerr.SkipTopBot},
		{"self_type", typeOf(rbs.TypeSelf), "", true, pkgerr.SkipSelfType},
		{"instance_type", typeOf(rbs.TypeInstance), "", true, pkgerr.SkipSelfType},
		{"class_type", typeOf(rbs.TypeClass), "", true, pkgerr.SkipSelfType},

		// Named types
		{"named_known_handle_io", namedType("IO"), "handle<IO>", false, 0},
		{"named_known_handle_file", namedType("File"), "handle<File>", false, 0},
		{"named_unknown", namedType("SomeRubyClass"), "", true, pkgerr.SkipUnknown},
	}

	for _, tc := range cases {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			got, sr := Translate(tc.rbs, "testgem", "TestItem#method")
			if tc.wantSkip {
				if sr == nil {
					t.Fatalf("expected SkipReport with reason %v, got nil (result=%q)", tc.skipReason, got)
				}
				if sr.Reason != tc.skipReason {
					t.Errorf("SkipReport.Reason = %v, want %v", sr.Reason, tc.skipReason)
				}
				return
			}
			if sr != nil {
				t.Fatalf("unexpected SkipReport: %v", sr)
			}
			if got != tc.want {
				t.Errorf("Translate = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestTranslate_VoidInParamPosition(t *testing.T) {
	// A method whose parameter type is void should be skipped.
	voidParam := &rbs.Type{Kind: rbs.TypeVoid}
	methodType := &rbs.MethodDecl{
		Name: "bad",
		Kind: rbs.MethodInstance,
		Types: []rbs.MethodType{
			{
				Params: []rbs.Param{{Name: "x", Type: voidParam}},
				Return: typeOf(rbs.TypeString),
			},
		},
	}
	_, sr := TranslateMethod("testgem", *methodType, "Foo")
	if sr == nil {
		t.Fatal("expected SkipReport for void param, got nil")
	}
	if sr.Reason != pkgerr.SkipVoidParam {
		t.Errorf("reason = %v, want SkipVoidParam", sr.Reason)
	}
}

func TestTranslate_ProcArityOver5(t *testing.T) {
	bigProc := &rbs.Type{
		Kind: rbs.TypeProc,
		Params: []*rbs.Type{
			typeOf(rbs.TypeString), typeOf(rbs.TypeString), typeOf(rbs.TypeString),
			typeOf(rbs.TypeString), typeOf(rbs.TypeString), typeOf(rbs.TypeString),
		},
		Return: typeOf(rbs.TypeVoid),
	}
	_, sr := Translate(bigProc, "gem", "item")
	if sr == nil || sr.Reason != pkgerr.SkipProc {
		t.Errorf("expected SkipProc for arity>5, got %v", sr)
	}
}

func TestTranslateMethod_Redis(t *testing.T) {
	// extern fun redis_get(key: string): string? from ruby "Redis#get"
	method := rbs.MethodDecl{
		Name: "get",
		Kind: rbs.MethodInstance,
		Types: []rbs.MethodType{
			{
				Params: []rbs.Param{{Name: "key", Type: typeOf(rbs.TypeString)}},
				Return: optionalOf(rbs.TypeString),
			},
		},
	}
	line, sr := TranslateMethod("redis", method, "Redis")
	if sr != nil {
		t.Fatalf("unexpected skip: %v", sr)
	}
	if !strings.Contains(line, "redis_get") {
		t.Errorf("expected fn name 'redis_get' in %q", line)
	}
	if !strings.Contains(line, "key: string") {
		t.Errorf("expected param 'key: string' in %q", line)
	}
	if !strings.Contains(line, "): string?") {
		t.Errorf("expected return 'string?' in %q", line)
	}
	if !strings.Contains(line, `from ruby "Redis#get"`) {
		t.Errorf("expected 'from ruby \"Redis#get\"' in %q", line)
	}
}

func TestTranslateMethod_JSONParse(t *testing.T) {
	// JSON.parse returns Hash{String => String}
	method := rbs.MethodDecl{
		Name: "parse",
		Kind: rbs.MethodSingleton,
		Types: []rbs.MethodType{
			{
				Params: []rbs.Param{{Name: "source", Type: typeOf(rbs.TypeString)}},
				Return: hashOf(rbs.TypeString, rbs.TypeString),
			},
		},
	}
	line, sr := TranslateMethod("json", method, "JSON")
	if sr != nil {
		t.Fatalf("unexpected skip: %v", sr)
	}
	if !strings.Contains(line, "json_parse") {
		t.Errorf("expected fn name 'json_parse' in %q", line)
	}
	if !strings.Contains(line, "source: string") {
		t.Errorf("expected param 'source: string' in %q", line)
	}
	if !strings.Contains(line, "map<string, string>") {
		t.Errorf("expected return type 'map<string, string>' in %q", line)
	}
	// Singleton method uses "." separator.
	if !strings.Contains(line, `from ruby "JSON.parse"`) {
		t.Errorf("expected 'from ruby \"JSON.parse\"' in %q", line)
	}
}

func TestTranslateMethod_NoOverloads(t *testing.T) {
	method := rbs.MethodDecl{Name: "mystery", Kind: rbs.MethodInstance}
	_, sr := TranslateMethod("mygem", method, "MyClass")
	if sr == nil {
		t.Fatal("expected skip for method with no overloads")
	}
}

func TestMochiTypeString(t *testing.T) {
	cases := []struct {
		t    *rbs.Type
		want string
	}{
		{typeOf(rbs.TypeString), "string"},
		{typeOf(rbs.TypeInteger), "int"},
		{arrayOf(rbs.TypeFloat), "list<float>"},
		{optionalOf(rbs.TypeBool), "bool?"},
		{typeOf(rbs.TypeUntyped), ""}, // skip returns empty
	}
	for _, tc := range cases {
		got := MochiTypeString(tc.t)
		if got != tc.want {
			t.Errorf("MochiTypeString(%v) = %q, want %q", tc.t, got, tc.want)
		}
	}
}
