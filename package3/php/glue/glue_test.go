package glue

import (
	"strings"
	"testing"

	"mochi/package3/php/reflect"
)

func surface(classes []reflect.ClassSurface, ifaces []reflect.InterfaceSurface, enums []reflect.EnumSurface) *reflect.ReflectionSurface {
	return &reflect.ReflectionSurface{
		PackageName: "test/pkg",
		PHPVersion:  "8.4.0",
		Classes:     classes,
		Interfaces:  ifaces,
		Enums:       enums,
	}
}

func TestEmitEmpty(t *testing.T) {
	result, err := Emit(surface(nil, nil, nil), "vendor", "pkg")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(result.Files) != 0 {
		t.Errorf("empty surface should produce no files; got %d", len(result.Files))
	}
}

func TestMochiGlueNamespace(t *testing.T) {
	result, err := Emit(surface([]reflect.ClassSurface{
		{FQCN: `Foo\Bar`, Methods: nil},
	}, nil, nil), "guzzlehttp", "guzzle")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result.Namespace != `MochiGlue\Guzzlehttp\Guzzle` {
		t.Errorf("Namespace = %q; want MochiGlue\\Guzzlehttp\\Guzzle", result.Namespace)
	}
}

func TestEmitClassFileCreated(t *testing.T) {
	result, err := Emit(surface([]reflect.ClassSurface{
		{FQCN: `GuzzleHttp\Client`, Methods: nil},
	}, nil, nil), "guzzlehttp", "guzzle")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	src, ok := result.Files["GuzzleHttpClient.php"]
	if !ok {
		t.Fatalf("expected GuzzleHttpClient.php; got files: %v", fileKeys(result.Files))
	}
	if !strings.Contains(src, "class GuzzleHttpClient") {
		t.Errorf("expected class GuzzleHttpClient; got:\n%s", src)
	}
	if !strings.Contains(src, "use GuzzleHttp\\Client as _GuzzleHttpClient") {
		t.Errorf("expected use alias; got:\n%s", src)
	}
	if !strings.Contains(src, "declare(strict_types=1)") {
		t.Errorf("expected strict_types; got:\n%s", src)
	}
}

func TestEmitClassInstanceMethod(t *testing.T) {
	result, err := Emit(surface([]reflect.ClassSurface{
		{
			FQCN: `Foo\Bar`,
			Methods: []reflect.MethodSurface{
				{Name: "doThing", ReturnType: "string", Parameters: []reflect.ParameterSurface{
					{Name: "x", Type: "int"},
				}},
			},
		},
	}, nil, nil), "vendor", "pkg")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	src := result.Files["FooBar.php"]
	if !strings.Contains(src, "public function doThing(int $x): string") {
		t.Errorf("expected doThing method signature; got:\n%s", src)
	}
	if !strings.Contains(src, "return $this->_inner->doThing($x)") {
		t.Errorf("expected delegation to _inner; got:\n%s", src)
	}
}

func TestEmitClassStaticMethod(t *testing.T) {
	result, err := Emit(surface([]reflect.ClassSurface{
		{
			FQCN: `Foo\Bar`,
			Methods: []reflect.MethodSurface{
				{Name: "create", Static: true, ReturnType: "string", Parameters: nil},
			},
		},
	}, nil, nil), "vendor", "pkg")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	src := result.Files["FooBar.php"]
	if !strings.Contains(src, "public static function create(): string") {
		t.Errorf("expected static create method; got:\n%s", src)
	}
	if !strings.Contains(src, "return _FooBar::create()") {
		t.Errorf("expected static delegation; got:\n%s", src)
	}
}

func TestEmitMagicMethodSkipped(t *testing.T) {
	result, err := Emit(surface([]reflect.ClassSurface{
		{
			FQCN: "Foo",
			Methods: []reflect.MethodSurface{
				{Name: "__construct", ReturnType: "void"},
				{Name: "__get", ReturnType: "string", Parameters: []reflect.ParameterSurface{{Name: "name", Type: "string"}}},
				{Name: "normalMethod", ReturnType: "void"},
			},
		},
	}, nil, nil), "vendor", "pkg")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	src := result.Files["Foo.php"]
	// The wrapper always has its own __construct for $_inner; check that the
	// original's magic methods are not forwarded (i.e. no "->__construct" or "->__get").
	if strings.Contains(src, "_inner->__construct") {
		t.Errorf("magic __construct should not be forwarded; got:\n%s", src)
	}
	if strings.Contains(src, "_inner->__get") {
		t.Errorf("magic __get should not be forwarded; got:\n%s", src)
	}
	if !strings.Contains(src, "normalMethod") {
		t.Errorf("normalMethod should be present; got:\n%s", src)
	}
}

func TestEmitVoidReturnNoReturn(t *testing.T) {
	result, err := Emit(surface([]reflect.ClassSurface{
		{
			FQCN: "Foo",
			Methods: []reflect.MethodSurface{
				{Name: "run", ReturnType: "void"},
			},
		},
	}, nil, nil), "vendor", "pkg")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	src := result.Files["Foo.php"]
	// void method should NOT have a return statement
	if strings.Contains(src, "return $this->_inner->run") {
		t.Errorf("void method should not return value; got:\n%s", src)
	}
	if !strings.Contains(src, "$this->_inner->run()") {
		t.Errorf("expected void delegation without return; got:\n%s", src)
	}
}

func TestEmitNullableParam(t *testing.T) {
	result, err := Emit(surface([]reflect.ClassSurface{
		{
			FQCN: "Foo",
			Methods: []reflect.MethodSurface{
				{Name: "bar", ReturnType: "void", Parameters: []reflect.ParameterSurface{
					{Name: "x", Type: "string", Nullable: true},
				}},
			},
		},
	}, nil, nil), "vendor", "pkg")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	src := result.Files["Foo.php"]
	if !strings.Contains(src, "?string $x") {
		t.Errorf("expected ?string $x for nullable; got:\n%s", src)
	}
}

func TestEmitVariadicParam(t *testing.T) {
	result, err := Emit(surface([]reflect.ClassSurface{
		{
			FQCN: "Foo",
			Methods: []reflect.MethodSurface{
				{Name: "bar", ReturnType: "void", Parameters: []reflect.ParameterSurface{
					{Name: "args", Type: "string", Variadic: true},
				}},
			},
		},
	}, nil, nil), "vendor", "pkg")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	src := result.Files["Foo.php"]
	if !strings.Contains(src, "string ...$args") {
		t.Errorf("expected string ...$args for variadic; got:\n%s", src)
	}
	if !strings.Contains(src, "...$args") {
		t.Errorf("expected ...$args in delegation; got:\n%s", src)
	}
}

func TestEmitInterface(t *testing.T) {
	result, err := Emit(surface(nil, []reflect.InterfaceSurface{
		{FQCN: `Psr\Log\LoggerInterface`},
	}, nil), "psr", "log")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	src, ok := result.Files["PsrLogLoggerInterface.php"]
	if !ok {
		t.Fatalf("expected PsrLogLoggerInterface.php; got %v", fileKeys(result.Files))
	}
	if !strings.Contains(src, "use Psr\\Log\\LoggerInterface as _PsrLogLoggerInterface") {
		t.Errorf("expected use alias; got:\n%s", src)
	}
}

func TestEmitPureEnum(t *testing.T) {
	result, err := Emit(surface(nil, nil, []reflect.EnumSurface{
		{FQCN: `Foo\Status`, BackingType: "", Cases: []reflect.EnumCase{
			{Name: "Active"}, {Name: "Inactive"},
		}},
	}), "vendor", "pkg")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	src := result.Files["FooStatus.php"]
	if !strings.Contains(src, "public static function caseActive(): _FooStatus") {
		t.Errorf("expected caseActive accessor; got:\n%s", src)
	}
	if strings.Contains(src, "fromValue") {
		t.Errorf("pure enum should not have fromValue; got:\n%s", src)
	}
}

func TestEmitBackedEnum(t *testing.T) {
	result, err := Emit(surface(nil, nil, []reflect.EnumSurface{
		{FQCN: `Foo\Color`, BackingType: "string", Cases: []reflect.EnumCase{
			{Name: "Red", Value: "red"},
		}},
	}), "vendor", "pkg")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	src := result.Files["FooColor.php"]
	if !strings.Contains(src, "public static function fromValue(mixed $value): _FooColor") {
		t.Errorf("expected fromValue; got:\n%s", src)
	}
	if !strings.Contains(src, "return _FooColor::from($value)") {
		t.Errorf("expected from() delegation; got:\n%s", src)
	}
}

func TestPascalCase(t *testing.T) {
	cases := []struct{ in, want string }{
		{"guzzlehttp", "Guzzlehttp"},
		{"my-package", "MyPackage"},
		{"psr", "Psr"},
		{"symfony", "Symfony"},
		{"some_pkg", "SomePkg"},
	}
	for _, tc := range cases {
		got := pascalCase(tc.in)
		if got != tc.want {
			t.Errorf("pascalCase(%q) = %q; want %q", tc.in, got, tc.want)
		}
	}
}

func TestClassHandle(t *testing.T) {
	cases := []struct{ in, want string }{
		{`GuzzleHttp\Client`, "GuzzleHttpClient"},
		{`Psr\Log\LoggerInterface`, "PsrLogLoggerInterface"},
		{"", "OpaqueHandle"},
	}
	for _, tc := range cases {
		got := classHandle(tc.in)
		if got != tc.want {
			t.Errorf("classHandle(%q) = %q; want %q", tc.in, got, tc.want)
		}
	}
}

func fileKeys(m map[string]string) []string {
	keys := make([]string, 0, len(m))
	for k := range m {
		keys = append(keys, k)
	}
	return keys
}
