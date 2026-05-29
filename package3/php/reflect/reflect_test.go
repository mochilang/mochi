package reflect

import (
	"context"
	"encoding/json"
	"errors"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// phpAvailable returns true when a usable PHP 8.x CLI is on PATH.
func phpAvailable() bool {
	path, err := exec.LookPath("php")
	if err != nil {
		return false
	}
	out, err := exec.Command(path, "--version").Output()
	if err != nil {
		return false
	}
	return strings.HasPrefix(string(out), "PHP 8")
}

func TestParseSurface(t *testing.T) {
	raw := `{
		"package_name": "guzzlehttp/guzzle",
		"php_version": "8.4.0",
		"classes": [
			{
				"fqcn": "GuzzleHttp\\Client",
				"abstract": false,
				"final": false,
				"methods": [
					{
						"name": "send",
						"static": false,
						"abstract": false,
						"final": false,
						"parameters": [
							{"name": "request", "type": "RequestInterface", "optional": false, "variadic": false},
							{"name": "options", "type": "array", "optional": true, "variadic": false, "default_value": "array ()"}
						],
						"return_type": "ResponseInterface"
					}
				],
				"properties": [],
				"interface_fqcns": ["GuzzleHttp\\ClientInterface"]
			}
		],
		"interfaces": [],
		"enums": [],
		"functions": []
	}`
	s, err := ParseSurface([]byte(raw))
	if err != nil {
		t.Fatalf("ParseSurface: %v", err)
	}
	if s.PackageName != "guzzlehttp/guzzle" {
		t.Errorf("PackageName = %q; want guzzlehttp/guzzle", s.PackageName)
	}
	if s.PHPVersion != "8.4.0" {
		t.Errorf("PHPVersion = %q; want 8.4.0", s.PHPVersion)
	}
	if len(s.Classes) != 1 {
		t.Fatalf("expected 1 class; got %d", len(s.Classes))
	}
	cls := s.Classes[0]
	if cls.FQCN != `GuzzleHttp\Client` {
		t.Errorf("FQCN = %q; want GuzzleHttp\\Client", cls.FQCN)
	}
	if len(cls.Methods) != 1 {
		t.Fatalf("expected 1 method; got %d", len(cls.Methods))
	}
	if cls.Methods[0].ReturnType != "ResponseInterface" {
		t.Errorf("ReturnType = %q; want ResponseInterface", cls.Methods[0].ReturnType)
	}
	if len(cls.Methods[0].Parameters) != 2 {
		t.Errorf("expected 2 parameters; got %d", len(cls.Methods[0].Parameters))
	}
}

func TestParseSurfaceBadJSON(t *testing.T) {
	_, err := ParseSurface([]byte("not json"))
	if err == nil {
		t.Error("expected error for bad JSON")
	}
}

func TestRunPHPNotFound(t *testing.T) {
	_, err := Run(context.Background(), "/nonexistent/php-binary", t.TempDir())
	if err == nil {
		t.Fatal("expected error for missing PHP binary")
	}
	if !errors.Is(err, ErrPHPNotFound) {
		t.Errorf("error %v is not ErrPHPNotFound", err)
	}
}

func TestRunFromScriptMinimalPHP(t *testing.T) {
	if !phpAvailable() {
		t.Skip("PHP 8.x not available on PATH")
	}
	// A minimal PHP script that emits a valid ReflectionSurface.
	minimalScript := `<?php
$pkgDir = $argv[1] ?? '';
echo json_encode([
    'package_name' => 'test/pkg',
    'php_version'  => PHP_VERSION,
    'classes'      => [],
    'interfaces'   => [],
    'enums'        => [],
    'functions'    => [],
    'errors'       => [],
]);
`
	s, err := RunFromScript(context.Background(), "php", t.TempDir(), minimalScript)
	if err != nil {
		t.Fatalf("RunFromScript: %v", err)
	}
	if s.PackageName != "test/pkg" {
		t.Errorf("PackageName = %q; want test/pkg", s.PackageName)
	}
	if s.PHPVersion == "" {
		t.Error("PHPVersion should not be empty")
	}
}

func TestRunFromScriptExitNonZero(t *testing.T) {
	if !phpAvailable() {
		t.Skip("PHP 8.x not available on PATH")
	}
	badScript := `<?php exit(1); ?>`
	_, err := RunFromScript(context.Background(), "php", t.TempDir(), badScript)
	if err == nil {
		t.Fatal("expected error for non-zero exit")
	}
	if !errors.Is(err, ErrReflectFailed) {
		t.Errorf("error %v is not ErrReflectFailed", err)
	}
}

func TestRunFromScriptBadJSON(t *testing.T) {
	if !phpAvailable() {
		t.Skip("PHP 8.x not available on PATH")
	}
	badScript := `<?php echo "not valid json"; ?>`
	_, err := RunFromScript(context.Background(), "php", t.TempDir(), badScript)
	if err == nil {
		t.Fatal("expected error for bad JSON output")
	}
	if !strings.Contains(err.Error(), "parse JSON") {
		t.Errorf("error %v should mention parse JSON", err)
	}
}

func TestRunFullReflection(t *testing.T) {
	if !phpAvailable() {
		t.Skip("PHP 8.x not available on PATH")
	}
	// Create a mini PHP package with one class and one interface.
	pkgDir := t.TempDir()
	phpSrc := `<?php
namespace TestPkg;

interface Greeter {
    public function greet(string $name): string;
}

class Hello implements Greeter {
    public string $greeting = 'Hello';
    public function greet(string $name): string {
        return $this->greeting . ', ' . $name . '!';
    }
    public static function world(): string {
        return 'world';
    }
}

function testHelper(int $x, int $y = 0): int {
    return $x + $y;
}
`
	if err := os.WriteFile(filepath.Join(pkgDir, "Hello.php"), []byte(phpSrc), 0o644); err != nil {
		t.Fatalf("write PHP: %v", err)
	}

	s, err := Run(context.Background(), "php", pkgDir)
	if err != nil {
		t.Fatalf("Run: %v", err)
	}

	// Find the Hello class.
	var helloClass *ClassSurface
	for i := range s.Classes {
		if strings.HasSuffix(s.Classes[i].FQCN, "Hello") {
			helloClass = &s.Classes[i]
			break
		}
	}
	if helloClass == nil {
		// Dump the surface for debugging.
		raw, _ := json.MarshalIndent(s, "", "  ")
		t.Fatalf("Hello class not found\n%s", raw)
	}

	// Verify greet method.
	var greetMethod *MethodSurface
	for i := range helloClass.Methods {
		if helloClass.Methods[i].Name == "greet" {
			greetMethod = &helloClass.Methods[i]
			break
		}
	}
	if greetMethod == nil {
		t.Fatalf("greet method not found in Hello class")
	}
	if greetMethod.ReturnType != "string" {
		t.Errorf("greet ReturnType = %q; want string", greetMethod.ReturnType)
	}
	if len(greetMethod.Parameters) != 1 {
		t.Errorf("greet expected 1 param; got %d", len(greetMethod.Parameters))
	}

	// Verify world static method.
	var worldMethod *MethodSurface
	for i := range helloClass.Methods {
		if helloClass.Methods[i].Name == "world" {
			worldMethod = &helloClass.Methods[i]
			break
		}
	}
	if worldMethod == nil {
		t.Fatalf("world method not found")
	}
	if !worldMethod.Static {
		t.Error("world should be static")
	}
}

func TestReflectSurfaceTypes(t *testing.T) {
	// Verify the surface type structs can be marshalled/unmarshalled round-trip.
	s := ReflectionSurface{
		PackageName: "test/pkg",
		PHPVersion:  "8.4.0",
		Classes: []ClassSurface{
			{
				FQCN:     `Foo\Bar`,
				Abstract: false,
				Methods: []MethodSurface{
					{Name: "foo", ReturnType: "string",
						Parameters: []ParameterSurface{
							{Name: "x", Type: "int", Optional: false},
						}},
				},
				Properties: []PropertySurface{{Name: "count", Type: "int"}},
			},
		},
		Interfaces: []InterfaceSurface{{FQCN: `Foo\IFoo`}},
		Enums: []EnumSurface{
			{FQCN: `Foo\Color`, BackingType: "string",
				Cases: []EnumCase{{Name: "Red", Value: "red"}}},
		},
		Functions: []FunctionSurface{{Name: "helper", ReturnType: "void"}},
	}
	raw, err := json.Marshal(s)
	if err != nil {
		t.Fatalf("marshal: %v", err)
	}
	var s2 ReflectionSurface
	if err := json.Unmarshal(raw, &s2); err != nil {
		t.Fatalf("unmarshal: %v", err)
	}
	if s2.PackageName != s.PackageName {
		t.Errorf("round-trip PackageName = %q; want %q", s2.PackageName, s.PackageName)
	}
	if len(s2.Enums) != 1 || s2.Enums[0].BackingType != "string" {
		t.Errorf("round-trip enum backing type mismatch")
	}
}
