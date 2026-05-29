package externemit

import (
	"strings"
	"testing"

	bridgeerrors "mochi/package3/php/errors"
	"mochi/package3/php/reflect"
)

func surface(classes []reflect.ClassSurface, ifaces []reflect.InterfaceSurface, enums []reflect.EnumSurface, fns []reflect.FunctionSurface) *reflect.ReflectionSurface {
	return &reflect.ReflectionSurface{
		PackageName: "test/pkg",
		PHPVersion:  "8.4.0",
		Classes:     classes,
		Interfaces:  ifaces,
		Enums:       enums,
		Functions:   fns,
	}
}

func TestEmitEmpty(t *testing.T) {
	result, err := Emit(surface(nil, nil, nil, nil))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result.MochiSource != "" {
		t.Errorf("empty surface should produce empty source; got %q", result.MochiSource)
	}
	if len(result.Skips) != 0 {
		t.Errorf("empty surface should produce no skips; got %d", len(result.Skips))
	}
}

func TestEmitClassExternType(t *testing.T) {
	s := surface([]reflect.ClassSurface{
		{FQCN: `GuzzleHttp\Client`, Methods: nil},
	}, nil, nil, nil)
	result, err := Emit(s)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if !strings.Contains(result.MochiSource, "extern type GuzzleHttpClient") {
		t.Errorf("expected extern type GuzzleHttpClient; got:\n%s", result.MochiSource)
	}
}

func TestEmitSimpleMethod(t *testing.T) {
	s := surface([]reflect.ClassSurface{
		{
			FQCN: `Foo\Bar`,
			Methods: []reflect.MethodSurface{
				{
					Name:       "doThing",
					ReturnType: "string",
					Parameters: []reflect.ParameterSurface{
						{Name: "x", Type: "int"},
					},
				},
			},
		},
	}, nil, nil, nil)
	result, err := Emit(s)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	// Expect: extern fn foo_bar_do_thing(self: FooBar, x: int) -> string
	if !strings.Contains(result.MochiSource, "extern fn foo_bar_do_thing(self: FooBar, x: int) -> string") {
		t.Errorf("expected extern fn foo_bar_do_thing; got:\n%s", result.MochiSource)
	}
}

func TestEmitStaticMethod(t *testing.T) {
	s := surface([]reflect.ClassSurface{
		{
			FQCN: `Foo\Bar`,
			Methods: []reflect.MethodSurface{
				{
					Name:       "create",
					Static:     true,
					ReturnType: "string",
					Parameters: nil,
				},
			},
		},
	}, nil, nil, nil)
	result, err := Emit(s)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	// Static: no self parameter
	if !strings.Contains(result.MochiSource, "extern fn foo_bar_create() -> string") {
		t.Errorf("expected extern fn foo_bar_create() -> string; got:\n%s", result.MochiSource)
	}
}

func TestEmitMagicMethodSkipped(t *testing.T) {
	s := surface([]reflect.ClassSurface{
		{
			FQCN: "Foo",
			Methods: []reflect.MethodSurface{
				{Name: "__construct", ReturnType: "void"},
				{Name: "__get", ReturnType: "string", Parameters: []reflect.ParameterSurface{{Name: "name", Type: "string"}}},
			},
		},
	}, nil, nil, nil)
	result, err := Emit(s)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(result.Skips) != 2 {
		t.Errorf("expected 2 skips for magic methods; got %d", len(result.Skips))
	}
	for _, sk := range result.Skips {
		if sk.Reason != bridgeerrors.SkipMagicMethod {
			t.Errorf("expected SkipMagicMethod; got %v for %s", sk.Reason, sk.ItemPath)
		}
	}
}

func TestEmitVariadicParamSkipped(t *testing.T) {
	s := surface([]reflect.ClassSurface{
		{
			FQCN: "Foo",
			Methods: []reflect.MethodSurface{
				{
					Name:       "bar",
					ReturnType: "void",
					Parameters: []reflect.ParameterSurface{
						{Name: "args", Type: "string", Variadic: true},
					},
				},
			},
		},
	}, nil, nil, nil)
	result, err := Emit(s)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(result.Skips) != 1 {
		t.Errorf("expected 1 skip for variadic; got %d", len(result.Skips))
	}
	if result.Skips[0].Reason != bridgeerrors.SkipVararg {
		t.Errorf("expected SkipVararg; got %v", result.Skips[0].Reason)
	}
}

func TestEmitUntypedParamSkipped(t *testing.T) {
	s := surface([]reflect.ClassSurface{
		{
			FQCN: "Foo",
			Methods: []reflect.MethodSurface{
				{
					Name:       "bar",
					ReturnType: "string",
					Parameters: []reflect.ParameterSurface{
						{Name: "x"}, // no type
					},
				},
			},
		},
	}, nil, nil, nil)
	result, err := Emit(s)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(result.Skips) != 1 {
		t.Errorf("expected 1 skip for untyped param; got %d", len(result.Skips))
	}
	if result.Skips[0].Reason != bridgeerrors.SkipMixed {
		t.Errorf("expected SkipMixed for untyped; got %v", result.Skips[0].Reason)
	}
}

func TestEmitMixedReturnSkipped(t *testing.T) {
	s := surface([]reflect.ClassSurface{
		{
			FQCN: "Foo",
			Methods: []reflect.MethodSurface{
				{Name: "bar", ReturnType: "mixed"},
			},
		},
	}, nil, nil, nil)
	result, err := Emit(s)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := false
	for _, sk := range result.Skips {
		if sk.Reason == bridgeerrors.SkipMixed {
			found = true
		}
	}
	if !found {
		t.Errorf("expected SkipMixed for mixed return; got skips: %v", result.Skips)
	}
}

func TestEmitNoReturnAnnotation(t *testing.T) {
	// A method with no return type annotation should map to -> unit.
	s := surface([]reflect.ClassSurface{
		{
			FQCN: "Foo",
			Methods: []reflect.MethodSurface{
				{Name: "run", ReturnType: ""},
			},
		},
	}, nil, nil, nil)
	result, err := Emit(s)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if !strings.Contains(result.MochiSource, "-> unit") {
		t.Errorf("expected -> unit for no return annotation; got:\n%s", result.MochiSource)
	}
}

func TestEmitInterface(t *testing.T) {
	s := surface(nil, []reflect.InterfaceSurface{
		{
			FQCN: `Psr\Log\LoggerInterface`,
			Methods: []reflect.MethodSurface{
				{Name: "log", ReturnType: "void", Parameters: []reflect.ParameterSurface{
					{Name: "level", Type: "string"},
					{Name: "message", Type: "string"},
				}},
			},
		},
	}, nil, nil)
	result, err := Emit(s)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if !strings.Contains(result.MochiSource, "extern type PsrLogLoggerInterface") {
		t.Errorf("expected extern type PsrLogLoggerInterface; got:\n%s", result.MochiSource)
	}
	if !strings.Contains(result.MochiSource, "extern fn psr_log_logger_interface_log(self: PsrLogLoggerInterface, level: string, message: string) -> unit") {
		t.Errorf("expected log fn; got:\n%s", result.MochiSource)
	}
}

func TestEmitPureEnum(t *testing.T) {
	s := surface(nil, nil, []reflect.EnumSurface{
		{FQCN: `Foo\Status`, BackingType: "", Cases: []reflect.EnumCase{
			{Name: "Active"}, {Name: "Inactive"},
		}},
	}, nil)
	result, err := Emit(s)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if !strings.Contains(result.MochiSource, "extern type FooStatus") {
		t.Errorf("expected extern type FooStatus; got:\n%s", result.MochiSource)
	}
	// Pure enum: no fromValue
	if strings.Contains(result.MochiSource, "from_value") {
		t.Errorf("pure enum should not have from_value; got:\n%s", result.MochiSource)
	}
}

func TestEmitBackedEnum(t *testing.T) {
	s := surface(nil, nil, []reflect.EnumSurface{
		{FQCN: `Foo\Color`, BackingType: "string", Cases: []reflect.EnumCase{
			{Name: "Red", Value: "red"},
		}},
	}, nil)
	result, err := Emit(s)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if !strings.Contains(result.MochiSource, "extern fn foo_color_from_value(value: string) -> FooColor") {
		t.Errorf("expected from_value fn; got:\n%s", result.MochiSource)
	}
}

func TestEmitTopLevelFunction(t *testing.T) {
	s := surface(nil, nil, nil, []reflect.FunctionSurface{
		{
			Name:       `MyNs\helper`,
			ReturnType: "int",
			Parameters: []reflect.ParameterSurface{
				{Name: "x", Type: "int"},
				{Name: "y", Type: "int"},
			},
		},
	})
	result, err := Emit(s)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if !strings.Contains(result.MochiSource, "extern fn my_ns_helper(x: int, y: int) -> int") {
		t.Errorf("expected my_ns_helper fn; got:\n%s", result.MochiSource)
	}
}

func TestEmitNullableParam(t *testing.T) {
	s := surface([]reflect.ClassSurface{
		{
			FQCN: "Foo",
			Methods: []reflect.MethodSurface{
				{
					Name:       "bar",
					ReturnType: "void",
					Parameters: []reflect.ParameterSurface{
						{Name: "x", Type: "string", Nullable: true},
					},
				},
			},
		},
	}, nil, nil, nil)
	result, err := Emit(s)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if !strings.Contains(result.MochiSource, "x: string|nil") {
		t.Errorf("expected x: string|nil for nullable param; got:\n%s", result.MochiSource)
	}
}

func TestFormatSKIPPED(t *testing.T) {
	skips := []bridgeerrors.SkipReport{
		{ItemPath: "test/pkg::Foo::bar", Reason: bridgeerrors.SkipMixed, Detail: "mixed type"},
	}
	out := FormatSKIPPED("test/pkg", skips)
	if !strings.Contains(out, "test/pkg::Foo::bar") {
		t.Errorf("SKIPPED.txt missing item path; got:\n%s", out)
	}
	if !strings.Contains(out, "SkipMixed") {
		t.Errorf("SKIPPED.txt missing reason; got:\n%s", out)
	}
}

func TestFormatSKIPPEDEmpty(t *testing.T) {
	out := FormatSKIPPED("test/pkg", nil)
	if out != "" {
		t.Errorf("empty skips should produce empty SKIPPED.txt; got %q", out)
	}
}

func TestToSnakeCase(t *testing.T) {
	cases := []struct{ in, want string }{
		{"GuzzleHttpClient", "guzzle_http_client"},
		{"send", "send"},
		{"doThing", "do_thing"},
		{"XMLParser", "x_m_l_parser"},
		{"", ""},
	}
	for _, tc := range cases {
		got := toSnakeCase(tc.in)
		if got != tc.want {
			t.Errorf("toSnakeCase(%q) = %q; want %q", tc.in, got, tc.want)
		}
	}
}

func TestClassHandle(t *testing.T) {
	cases := []struct{ in, want string }{
		{`GuzzleHttp\Client`, "GuzzleHttpClient"},
		{`Psr\Log\LoggerInterface`, "PsrLogLoggerInterface"},
		{"", "OpaqueHandle"},
		{`\Foo\Bar`, "FooBar"},
	}
	for _, tc := range cases {
		got := classHandle(tc.in)
		if got != tc.want {
			t.Errorf("classHandle(%q) = %q; want %q", tc.in, got, tc.want)
		}
	}
}
