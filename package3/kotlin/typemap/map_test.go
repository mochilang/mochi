package typemap

import (
	"testing"

	"github.com/mochilang/mochi/package3/kotlin/metadata"
)

// kt is a shorthand for building a KotlinType.
func kt(className string) metadata.KotlinType {
	return metadata.KotlinType{ClassName: className}
}

func ktN(className string) metadata.KotlinType {
	return metadata.KotlinType{ClassName: className, Nullable: true}
}

func ktArgs(className string, args ...metadata.KotlinType) metadata.KotlinType {
	return metadata.KotlinType{ClassName: className, TypeArgs: args}
}

func ktArgsN(className string, args ...metadata.KotlinType) metadata.KotlinType {
	return metadata.KotlinType{ClassName: className, TypeArgs: args, Nullable: true}
}

func ktParam(name string) metadata.KotlinType {
	return metadata.KotlinType{IsTypeParam: true, TypeParamName: name}
}

func TestTranslatePrimitives(t *testing.T) {
	cases := []struct {
		input    metadata.KotlinType
		wantStr  string
		wantVoid bool
	}{
		{kt("kotlin.Int"), "int", false},
		{kt("kotlin.Long"), "long", false},
		{kt("kotlin.Short"), "int", false},
		{kt("kotlin.Byte"), "int", false},
		{kt("kotlin.Double"), "double", false},
		{kt("kotlin.Float"), "float", false},
		{kt("kotlin.Boolean"), "bool", false},
		{kt("kotlin.Char"), "int", false},
		{kt("kotlin.String"), "string", false},
		{kt("kotlin.Any"), "any", false},
		{kt("kotlin.Unit"), "", true},
		{kt("kotlin.Nothing"), "", true},
	}
	for _, tc := range cases {
		got, reason := Translate(tc.input)
		if reason != NotRefused {
			t.Errorf("Translate(%q): unexpected refusal %v", tc.input.ClassName, reason)
			continue
		}
		if got.IsVoid != tc.wantVoid {
			t.Errorf("Translate(%q): IsVoid=%v, want %v", tc.input.ClassName, got.IsVoid, tc.wantVoid)
		}
		if !tc.wantVoid && got.String() != tc.wantStr {
			t.Errorf("Translate(%q): got %q, want %q", tc.input.ClassName, got.String(), tc.wantStr)
		}
	}
}

func TestTranslateNullableWrapping(t *testing.T) {
	cases := []struct {
		input   metadata.KotlinType
		wantStr string
	}{
		{ktN("kotlin.String"), "Option<string>"},
		{ktN("kotlin.Int"), "Option<int>"},
		{ktN("kotlin.Boolean"), "Option<bool>"},
		{ktN("kotlin.Long"), "Option<long>"},
		{ktN("kotlin.Double"), "Option<double>"},
		{ktN("java.lang.String"), "Option<string>"},
	}
	for _, tc := range cases {
		got, reason := Translate(tc.input)
		if reason != NotRefused {
			t.Errorf("Translate nullable %q: unexpected refusal %v", tc.input.ClassName, reason)
			continue
		}
		if got.String() != tc.wantStr {
			t.Errorf("Translate nullable %q: got %q, want %q", tc.input.ClassName, got.String(), tc.wantStr)
		}
	}
}

func TestTranslateCollections(t *testing.T) {
	cases := []struct {
		input   metadata.KotlinType
		wantStr string
	}{
		{ktArgs("kotlin.collections.List", kt("kotlin.String")), "List<string>"},
		{ktArgs("kotlin.collections.MutableList", kt("kotlin.Int")), "List<int>"},
		{ktArgs("kotlin.collections.Set", kt("kotlin.String")), "Set<string>"},
		{ktArgs("kotlin.collections.Map", kt("kotlin.String"), kt("kotlin.Int")), "Map<string, int>"},
		{ktArgs("kotlin.collections.MutableMap", kt("kotlin.Long"), kt("kotlin.Boolean")), "Map<long, bool>"},
		{ktArgs("kotlin.Array", kt("kotlin.String")), "List<string>"},
		// New additions
		{ktArgs("kotlin.collections.Collection", kt("kotlin.Int")), "List<int>"},
		{ktArgs("kotlin.collections.Iterable", kt("kotlin.String")), "List<string>"},
		{ktArgs("kotlin.sequences.Sequence", kt("kotlin.Double")), "List<double>"},
		{ktArgs("java.util.List", kt("kotlin.String")), "List<string>"},
		{ktArgs("java.util.Map", kt("kotlin.String"), kt("kotlin.Int")), "Map<string, int>"},
	}
	for _, tc := range cases {
		got, reason := Translate(tc.input)
		if reason != NotRefused {
			t.Errorf("Translate collection %q: unexpected refusal %v", tc.input.ClassName, reason)
			continue
		}
		if got.String() != tc.wantStr {
			t.Errorf("Translate collection %q: got %q, want %q", tc.input.ClassName, got.String(), tc.wantStr)
		}
	}
}

func TestTranslateNestedGenerics(t *testing.T) {
	// List<List<Int>>
	inner := ktArgs("kotlin.collections.List", kt("kotlin.Int"))
	outer := ktArgs("kotlin.collections.List", inner)
	got, reason := Translate(outer)
	if reason != NotRefused {
		t.Fatalf("nested generic: unexpected refusal %v", reason)
	}
	if got.String() != "List<List<int>>" {
		t.Errorf("nested generic: got %q, want %q", got.String(), "List<List<int>>")
	}

	// Map<String, List<Int>>
	listInt := ktArgs("kotlin.collections.List", kt("kotlin.Int"))
	mapType := ktArgs("kotlin.collections.Map", kt("kotlin.String"), listInt)
	got2, reason2 := Translate(mapType)
	if reason2 != NotRefused {
		t.Fatalf("Map<String, List<Int>>: unexpected refusal %v", reason2)
	}
	if got2.String() != "Map<string, List<int>>" {
		t.Errorf("Map<String, List<Int>>: got %q, want %q", got2.String(), "Map<string, List<int>>")
	}
}

func TestTranslateRefusals(t *testing.T) {
	cases := []struct {
		input      metadata.KotlinType
		wantReason RefusalReason
	}{
		{ktParam("T"), RefusalUnresolvedTypeParam},
		{kt("kotlin.coroutines.Continuation"), RefusalRawContinuation},
		{kt("kotlin.reflect.KClass"), RefusalKClassReflection},
		{kt("kotlin.reflect.KFunction"), RefusalKClassReflection},
		{kt("kotlin.reflect.KProperty"), RefusalKClassReflection},
		{kt("kotlin.UInt"), RefusalUnsignedIntJVM17},
		{kt("kotlin.ULong"), RefusalUnsignedIntJVM17},
		{kt("kotlin.UShort"), RefusalUnsignedIntJVM17},
		{kt("kotlin.UByte"), RefusalUnsignedIntJVM17},
		{kt("kotlin.jvm.functions.Function0"), RefusalRawLambda},
		{kt("kotlin.jvm.functions.Function2"), RefusalRawLambda},
		{kt("java.lang.Throwable"), RefusalThrowableReturn},
		{kt("java.lang.Exception"), RefusalThrowableReturn},
		{kt("java.lang.RuntimeException"), RefusalThrowableReturn},
		{kt("kotlin.Exception"), RefusalThrowableReturn},
	}
	for _, tc := range cases {
		_, reason := Translate(tc.input)
		if reason != tc.wantReason {
			t.Errorf("Translate(%q): got refusal %v, want %v", tc.input.ClassName, reason, tc.wantReason)
		}
	}
}

func TestTranslateKotlinResult(t *testing.T) {
	// kotlin.Result<String> -> KotlinResult (extern)
	input := ktArgs("kotlin.Result", kt("kotlin.String"))
	got, reason := Translate(input)
	if reason != NotRefused {
		t.Fatalf("kotlin.Result: unexpected refusal %v", reason)
	}
	if got.Name != "KotlinResult" || !got.IsExtern {
		t.Errorf("kotlin.Result: got %+v, want {Name:KotlinResult IsExtern:true}", got)
	}

	// Nullable kotlin.Result -> Option<KotlinResult>
	inputN := ktArgsN("kotlin.Result", kt("kotlin.String"))
	gotN, reasonN := Translate(inputN)
	if reasonN != NotRefused {
		t.Fatalf("kotlin.Result nullable: unexpected refusal %v", reasonN)
	}
	if gotN.String() != "Option<KotlinResult>" {
		t.Errorf("kotlin.Result nullable: got %q, want %q", gotN.String(), "Option<KotlinResult>")
	}
}

func TestTranslateTriple(t *testing.T) {
	input := ktArgs("kotlin.Triple", kt("kotlin.Int"), kt("kotlin.String"), kt("kotlin.Boolean"))
	got, reason := Translate(input)
	if reason != NotRefused {
		t.Fatalf("kotlin.Triple: unexpected refusal %v", reason)
	}
	if got.String() != "Triple<int, string, bool>" {
		t.Errorf("kotlin.Triple: got %q, want %q", got.String(), "Triple<int, string, bool>")
	}
}

func TestTranslatePair(t *testing.T) {
	input := ktArgs("kotlin.Pair", kt("kotlin.String"), kt("kotlin.Int"))
	got, reason := Translate(input)
	if reason != NotRefused {
		t.Fatalf("kotlin.Pair: unexpected refusal %v", reason)
	}
	if got.String() != "Pair<string, int>" {
		t.Errorf("kotlin.Pair: got %q, want %q", got.String(), "Pair<string, int>")
	}
}

func TestTranslateByteArray(t *testing.T) {
	got, reason := Translate(kt("kotlin.ByteArray"))
	if reason != NotRefused {
		t.Fatalf("ByteArray: unexpected refusal %v", reason)
	}
	if got.Name != "bytes" {
		t.Errorf("ByteArray: got %q, want bytes", got.Name)
	}
}

func TestTranslatePrimitiveArrays(t *testing.T) {
	cases := []struct {
		input   string
		wantStr string
	}{
		{"kotlin.IntArray", "List<int>"},
		{"kotlin.LongArray", "List<long>"},
		{"kotlin.ShortArray", "List<int>"},
		{"kotlin.FloatArray", "List<float>"},
		{"kotlin.DoubleArray", "List<double>"},
		{"kotlin.BooleanArray", "List<bool>"},
	}
	for _, tc := range cases {
		got, reason := Translate(kt(tc.input))
		if reason != NotRefused {
			t.Errorf("Translate(%q): unexpected refusal %v", tc.input, reason)
			continue
		}
		if got.String() != tc.wantStr {
			t.Errorf("Translate(%q): got %q, want %q", tc.input, got.String(), tc.wantStr)
		}
	}
}

func TestTranslateJavaString(t *testing.T) {
	got, reason := Translate(kt("java.lang.String"))
	if reason != NotRefused {
		t.Fatalf("java.lang.String: unexpected refusal %v", reason)
	}
	if got.Name != "string" {
		t.Errorf("java.lang.String: got %q, want string", got.Name)
	}
}

func TestTranslateExternType(t *testing.T) {
	// Unknown class -> opaque extern type
	got, reason := Translate(kt("com.example.MyClass"))
	if reason != NotRefused {
		t.Fatalf("extern type: unexpected refusal %v", reason)
	}
	if !got.IsExtern {
		t.Error("extern type: IsExtern should be true")
	}
	if got.Name != "MyClass" {
		t.Errorf("extern type: got name %q, want MyClass", got.Name)
	}
}

func TestTranslateStarProjection(t *testing.T) {
	star := metadata.KotlinType{IsStarProjection: true}
	input := metadata.KotlinType{
		ClassName: "kotlin.collections.List",
		TypeArgs:  []metadata.KotlinType{star},
	}
	got, reason := Translate(input)
	if reason != NotRefused {
		t.Fatalf("star projection: unexpected refusal %v", reason)
	}
	if got.String() != "List<any>" {
		t.Errorf("star projection: got %q, want List<any>", got.String())
	}
}

// ---------------------------------------------------------------------------
// TranslateFunction tests
// ---------------------------------------------------------------------------

func makeFunction(name string, params []metadata.Param, ret metadata.KotlinType, isSuspend bool) metadata.Function {
	flags := metadata.FunctionFlags{IsSuspend: isSuspend, IsPublic: true}
	return metadata.Function{
		Name:       name,
		JVMName:    name,
		Params:     params,
		ReturnType: ret,
		Flags:      flags,
	}
}

func TestTranslateFunction_Normal(t *testing.T) {
	fn := makeFunction("greetUser", []metadata.Param{
		{Name: "name", Type: kt("kotlin.String")},
		{Name: "age", Type: kt("kotlin.Int")},
	}, kt("kotlin.String"), false)

	mf, reason := TranslateFunction(fn)
	if reason != NotRefused {
		t.Fatalf("TranslateFunction normal: unexpected refusal %v", reason)
	}
	if mf.Name != "greet_user" {
		t.Errorf("Name: got %q, want greet_user", mf.Name)
	}
	if mf.IsSuspend {
		t.Error("IsSuspend should be false")
	}
	if len(mf.Params) != 2 {
		t.Fatalf("Params: got %d, want 2", len(mf.Params))
	}
	if mf.Params[0].Type.Name != "string" {
		t.Errorf("Params[0].Type: got %q, want string", mf.Params[0].Type.Name)
	}
	if mf.Params[1].Type.Name != "int" {
		t.Errorf("Params[1].Type: got %q, want int", mf.Params[1].Type.Name)
	}
	if mf.ReturnType.Name != "string" {
		t.Errorf("ReturnType: got %q, want string", mf.ReturnType.Name)
	}
}

func TestTranslateFunction_Suspend(t *testing.T) {
	fn := makeFunction("fetchData", []metadata.Param{}, kt("kotlin.String"), true)
	mf, reason := TranslateFunction(fn)
	if reason != NotRefused {
		t.Fatalf("suspend: unexpected refusal %v", reason)
	}
	if !mf.IsSuspend {
		t.Error("IsSuspend should be true for suspend function")
	}
}

func TestTranslateFunction_UnresolvedTypeParam(t *testing.T) {
	fn := makeFunction("map", []metadata.Param{
		{Name: "t", Type: ktParam("T")},
	}, kt("kotlin.String"), false)
	_, reason := TranslateFunction(fn)
	if reason != RefusalUnresolvedTypeParam {
		t.Errorf("unresolved type param: got %v, want RefusalUnresolvedTypeParam", reason)
	}
}

func TestTranslateFunction_UnresolvedReturnType(t *testing.T) {
	fn := makeFunction("get", []metadata.Param{}, ktParam("T"), false)
	_, reason := TranslateFunction(fn)
	if reason != RefusalUnresolvedTypeParam {
		t.Errorf("unresolved return type: got %v, want RefusalUnresolvedTypeParam", reason)
	}
}

func TestTranslateFunction_Extension(t *testing.T) {
	recv := kt("kotlin.String")
	fn := metadata.Function{
		Name:       "trimToNull",
		JVMName:    "trimToNull",
		ReturnType: ktN("kotlin.String"),
		Receiver:   &recv,
		Flags:      metadata.FunctionFlags{IsPublic: true},
	}
	mf, reason := TranslateFunction(fn)
	if reason != NotRefused {
		t.Fatalf("extension: unexpected refusal %v", reason)
	}
	if mf.Receiver == nil {
		t.Fatal("extension: Receiver should be non-nil")
	}
	if mf.Receiver.Name != "string" {
		t.Errorf("extension: Receiver.Name got %q, want string", mf.Receiver.Name)
	}
	if mf.ReturnType.String() != "Option<string>" {
		t.Errorf("extension: ReturnType got %q, want Option<string>", mf.ReturnType.String())
	}
}

func TestTranslateFunction_VoidReturn(t *testing.T) {
	fn := makeFunction("doNothing", nil, kt("kotlin.Unit"), false)
	mf, reason := TranslateFunction(fn)
	if reason != NotRefused {
		t.Fatalf("void return: unexpected refusal %v", reason)
	}
	if !mf.ReturnType.IsVoid {
		t.Error("void return: ReturnType.IsVoid should be true")
	}
}

// ---------------------------------------------------------------------------
// KotlinToMochiName tests
// ---------------------------------------------------------------------------

func TestKotlinToMochiName(t *testing.T) {
	cases := []struct {
		input string
		want  string
	}{
		{"fetchUserById", "fetch_user_by_id"},
		{"URL", "url"},
		{"getHTTPSStatus", "get_https_status"},
		{"greet", "greet"},
		{"MyClass", "my_class"},
		{"OkHttpClient", "ok_http_client"},
		{"newCall", "new_call"},
		{"getURL", "get_url"},
		{"parseHTML", "parse_html"},
		{"toJSON", "to_json"},
		{"", ""},
		{"a", "a"},
		{"A", "a"},
	}
	for _, tc := range cases {
		got := KotlinToMochiName(tc.input)
		if got != tc.want {
			t.Errorf("KotlinToMochiName(%q): got %q, want %q", tc.input, got, tc.want)
		}
	}
}

func TestClassToExternName(t *testing.T) {
	cases := []struct {
		input string
		want  string
	}{
		{"com.example.MyClass", "MyClass"},
		{"MyClass", "MyClass"},
		{"com.example.MyClass$Builder", "MyClassBuilder"},
		{"kotlin.collections.List", "List"},
		{"", ""},
	}
	for _, tc := range cases {
		got := ClassToExternName(tc.input)
		if got != tc.want {
			t.Errorf("ClassToExternName(%q): got %q, want %q", tc.input, got, tc.want)
		}
	}
}

func TestShimFnName(t *testing.T) {
	cases := []struct {
		className string
		fnName    string
		want      string
	}{
		{"OkHttpClient", "newCall", "ok_http_client_new_call"},
		{"MyClass", "greet", "my_class_greet"},
		{"", "greet", "greet"},
		{"MyClass", "", "my_class"},
	}
	for _, tc := range cases {
		got := ShimFnName(tc.className, tc.fnName)
		if got != tc.want {
			t.Errorf("ShimFnName(%q, %q): got %q, want %q", tc.className, tc.fnName, got, tc.want)
		}
	}
}

func TestNameRegistryDeduplication(t *testing.T) {
	reg := NewNameRegistry()
	n1 := reg.Allocate("MyClass", "get")
	n2 := reg.Allocate("MyClass", "get")
	n3 := reg.Allocate("MyClass", "get")

	if n1 != "my_class_get" {
		t.Errorf("first allocation: got %q, want my_class_get", n1)
	}
	if n2 != "my_class_get_2" {
		t.Errorf("second allocation: got %q, want my_class_get_2", n2)
	}
	if n3 != "my_class_get_3" {
		t.Errorf("third allocation: got %q, want my_class_get_3", n3)
	}

	// Different name should be unaffected
	n4 := reg.Allocate("MyClass", "set")
	if n4 != "my_class_set" {
		t.Errorf("different name: got %q, want my_class_set", n4)
	}
}
