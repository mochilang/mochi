package typemap

import (
	"testing"

	bridgeerrors "mochi/package3/php/errors"
)

func TestMapPrimitives(t *testing.T) {
	cases := []struct {
		phpType   string
		nullable  bool
		wantMochi string
		wantPrim  bool
	}{
		{"int", false, "int", true},
		{"integer", false, "int", true},
		{"float", false, "float", true},
		{"double", false, "float", true},
		{"string", false, "string", true},
		{"bool", false, "bool", true},
		{"boolean", false, "bool", true},
		{"true", false, "bool", false},
		{"false", false, "bool", false},
		{"null", false, "nil", false},
		{"iterable", false, "list[any]", false},
	}
	for _, tc := range cases {
		m, skip, detail := Map(tc.phpType, tc.nullable, DirectionIn)
		if skip != 0 {
			t.Errorf("Map(%q) unexpected skip %v: %s", tc.phpType, skip, detail)
			continue
		}
		if m.MochiType != tc.wantMochi {
			t.Errorf("Map(%q).MochiType = %q; want %q", tc.phpType, m.MochiType, tc.wantMochi)
		}
		if m.IsPrimitive != tc.wantPrim {
			t.Errorf("Map(%q).IsPrimitive = %v; want %v", tc.phpType, m.IsPrimitive, tc.wantPrim)
		}
	}
}

func TestMapNullable(t *testing.T) {
	m, skip, _ := Map("string", true, DirectionIn)
	if skip != 0 {
		t.Fatalf("unexpected skip for ?string")
	}
	if m.MochiType != "string|nil" {
		t.Errorf("MochiType = %q; want string|nil", m.MochiType)
	}
	if !m.Nullable {
		t.Error("Nullable should be true for ?string")
	}
}

func TestMapVoid(t *testing.T) {
	m, skip, _ := Map("void", false, DirectionOut)
	if skip != 0 {
		t.Fatalf("unexpected skip for void return type")
	}
	if m.MochiType != "unit" {
		t.Errorf("void return -> %q; want unit", m.MochiType)
	}
}

func TestMapSkipReasons(t *testing.T) {
	cases := []struct {
		phpType    string
		wantSkip   bridgeerrors.SkipReason
	}{
		{"mixed", bridgeerrors.SkipMixed},
		{"object", bridgeerrors.SkipObject},
		{"callable", bridgeerrors.SkipCallable},
		{"resource", bridgeerrors.SkipResource},
		{"never", bridgeerrors.SkipNever},
		{"self", bridgeerrors.SkipSelfStatic},
		{"static", bridgeerrors.SkipSelfStatic},
		{"parent", bridgeerrors.SkipSelfStatic},
		{"array", bridgeerrors.SkipUntypedArray},
	}
	for _, tc := range cases {
		_, skip, _ := Map(tc.phpType, false, DirectionIn)
		if skip != tc.wantSkip {
			t.Errorf("Map(%q) skip = %v; want %v", tc.phpType, skip, tc.wantSkip)
		}
	}
}

func TestMapClassHandle(t *testing.T) {
	cases := []struct {
		phpType   string
		wantMochi string
	}{
		{`GuzzleHttp\Client`, "GuzzleHttpClient"},
		{`GuzzleHttp\Psr7\Request`, "GuzzleHttpPsr7Request"},
		{`Monolog\Logger`, "MonologLogger"},
		{`Psr\Log\LoggerInterface`, "PsrLogLoggerInterface"},
		{`\Symfony\Component\Console\Application`, "SymfonyComponentConsoleApplication"},
	}
	for _, tc := range cases {
		m, skip, detail := Map(tc.phpType, false, DirectionIn)
		if skip != 0 {
			t.Errorf("Map(%q) unexpected skip %v: %s", tc.phpType, skip, detail)
			continue
		}
		if m.MochiType != tc.wantMochi {
			t.Errorf("Map(%q).MochiType = %q; want %q", tc.phpType, m.MochiType, tc.wantMochi)
		}
	}
}

func TestMapClassHandleNullable(t *testing.T) {
	m, skip, _ := Map(`GuzzleHttp\Client`, true, DirectionIn)
	if skip != 0 {
		t.Fatalf("unexpected skip")
	}
	if m.MochiType != "GuzzleHttpClient|nil" {
		t.Errorf("MochiType = %q; want GuzzleHttpClient|nil", m.MochiType)
	}
}

func TestMapUnionDegenerate(t *testing.T) {
	// int|null should map to int|nil.
	m, skip, _ := Map("int|null", false, DirectionIn)
	if skip != 0 {
		t.Fatalf("unexpected skip for int|null")
	}
	if m.MochiType != "int|nil" {
		t.Errorf("int|null -> %q; want int|nil", m.MochiType)
	}
	if !m.Nullable {
		t.Error("Nullable should be true for int|null")
	}
}

func TestMapUnionTwoTypes(t *testing.T) {
	// int|string should map to int|string.
	m, skip, _ := Map("int|string", false, DirectionIn)
	if skip != 0 {
		t.Fatalf("unexpected skip for int|string")
	}
	if m.MochiType != "int|string" {
		t.Errorf("int|string -> %q; want int|string", m.MochiType)
	}
}

func TestMapUnionWithNull(t *testing.T) {
	// int|string|null -> int|string|nil.
	m, skip, _ := Map("int|string|null", false, DirectionIn)
	if skip != 0 {
		t.Fatalf("unexpected skip")
	}
	if m.MochiType != "int|string|nil" {
		t.Errorf("int|string|null -> %q; want int|string|nil", m.MochiType)
	}
}

func TestMapUnionContainingSkip(t *testing.T) {
	// int|mixed -> SkipMixed.
	_, skip, _ := Map("int|mixed", false, DirectionIn)
	if skip != bridgeerrors.SkipMixed {
		t.Errorf("int|mixed skip = %v; want SkipMixed", skip)
	}
}

func TestMapIntersection(t *testing.T) {
	_, skip, _ := Map("A&B", false, DirectionIn)
	if skip != bridgeerrors.SkipIntersection {
		t.Errorf("A&B skip = %v; want SkipIntersection", skip)
	}
}

func TestMapTypedArrayList(t *testing.T) {
	m, skip, detail := MapTypedArray("", "string")
	if skip != 0 {
		t.Fatalf("MapTypedArray list unexpected skip: %v: %s", skip, detail)
	}
	if m.MochiType != "list[string]" {
		t.Errorf("MochiType = %q; want list[string]", m.MochiType)
	}
}

func TestMapTypedArrayMap(t *testing.T) {
	m, skip, detail := MapTypedArray("string", "int")
	if skip != 0 {
		t.Fatalf("MapTypedArray map unexpected skip: %v: %s", skip, detail)
	}
	if m.MochiType != "map[string]int" {
		t.Errorf("MochiType = %q; want map[string]int", m.MochiType)
	}
}

func TestMapTypedArraySkipWhenValueSkips(t *testing.T) {
	_, skip, _ := MapTypedArray("", "mixed")
	if skip != bridgeerrors.SkipMixed {
		t.Errorf("array<mixed> skip = %v; want SkipMixed", skip)
	}
}

func TestMapCaseInsensitive(t *testing.T) {
	// PHP type names are case-insensitive; ensure we handle uppercase variants.
	cases := []struct {
		phpType   string
		wantMochi string
	}{
		{"INT", "int"},
		{"STRING", "string"},
		{"BOOL", "bool"},
		{"FLOAT", "float"},
		{"MIXED", ""},
	}
	for _, tc := range cases {
		m, skip, _ := Map(tc.phpType, false, DirectionIn)
		if tc.wantMochi == "" {
			if skip == 0 {
				t.Errorf("Map(%q) should skip; got %q", tc.phpType, m.MochiType)
			}
		} else {
			if skip != 0 {
				t.Errorf("Map(%q) unexpected skip %v", tc.phpType, skip)
				continue
			}
			if m.MochiType != tc.wantMochi {
				t.Errorf("Map(%q).MochiType = %q; want %q", tc.phpType, m.MochiType, tc.wantMochi)
			}
		}
	}
}

func TestClassHandleType(t *testing.T) {
	cases := []struct {
		fqcn string
		want string
	}{
		{`GuzzleHttp\Client`, "GuzzleHttpClient"},
		{`\Foo\Bar\Baz`, "FooBarBaz"},
		{"Logger", "Logger"},
		{"", "OpaqueHandle"},
	}
	for _, tc := range cases {
		got := classHandleType(tc.fqcn)
		if got != tc.want {
			t.Errorf("classHandleType(%q) = %q; want %q", tc.fqcn, got, tc.want)
		}
	}
}

func TestMappingFields(t *testing.T) {
	m, skip, _ := Map("string", false, DirectionIn)
	if skip != 0 {
		t.Fatalf("unexpected skip")
	}
	if m.PHPType != "string" {
		t.Errorf("PHPType = %q; want string", m.PHPType)
	}
	if !m.IsPrimitive {
		t.Error("string should be primitive")
	}
	if m.Nullable {
		t.Error("non-nullable string should not be nullable")
	}
}
