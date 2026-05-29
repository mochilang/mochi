package library

import (
	"encoding/json"
	"strings"
	"testing"
)

func defaultConfig() Config {
	return Config{
		ComposerName:  "acme/my-lib",
		Description:   "Test library",
		Version:       "1.0.0",
		License:       "MIT",
		PSR4Namespace: `Acme\MyLib`,
		PHPRequire:    "^8.4",
	}
}

func TestEmitRequiredFields(t *testing.T) {
	_, err := Emit(Config{}, nil)
	if err == nil {
		t.Error("expected error for empty ComposerName")
	}
	_, err = Emit(Config{ComposerName: "a/b"}, nil)
	if err == nil {
		t.Error("expected error for empty PSR4Namespace")
	}
}

func TestEmitProducesExpectedFiles(t *testing.T) {
	result, err := Emit(defaultConfig(), nil)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	for _, expect := range []string{"composer.json", "README.md", "LICENSE"} {
		if _, ok := result.Files[expect]; !ok {
			t.Errorf("expected file %q; got %v", expect, fileKeys(result.Files))
		}
	}
}

func TestEmitComposerJSON(t *testing.T) {
	result, err := Emit(defaultConfig(), nil)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	src := result.Files["composer.json"]
	// Must parse as valid JSON.
	var m map[string]any
	if err := json.Unmarshal([]byte(src), &m); err != nil {
		t.Fatalf("composer.json is not valid JSON: %v\n%s", err, src)
	}
	if m["name"] != "acme/my-lib" {
		t.Errorf("name = %v; want acme/my-lib", m["name"])
	}
	if m["type"] != "library" {
		t.Errorf("type = %v; want library", m["type"])
	}
	if m["minimum-stability"] != "stable" {
		t.Errorf("minimum-stability = %v; want stable", m["minimum-stability"])
	}
}

func TestEmitComposerJSONPSR4(t *testing.T) {
	result, err := Emit(defaultConfig(), nil)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	src := result.Files["composer.json"]
	// PSR-4 namespace must have trailing backslash in JSON.
	if !strings.Contains(src, `"Acme\\MyLib\\"`) {
		t.Errorf("expected PSR-4 namespace with trailing backslash; got:\n%s", src)
	}
	if !strings.Contains(src, `"src/"`) {
		t.Errorf("expected src/ directory; got:\n%s", src)
	}
}

func TestEmitComposerJSONRequire(t *testing.T) {
	cfg := defaultConfig()
	cfg.Require = map[string]string{"monolog/monolog": "^3.0"}
	result, err := Emit(cfg, nil)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	src := result.Files["composer.json"]
	if !strings.Contains(src, `"php"`) {
		t.Errorf("expected php in require; got:\n%s", src)
	}
	if !strings.Contains(src, `"monolog/monolog"`) {
		t.Errorf("expected monolog/monolog in require; got:\n%s", src)
	}
}

func TestEmitComposerJSONAuthors(t *testing.T) {
	cfg := defaultConfig()
	cfg.Authors = []Author{{Name: "Alice", Email: "alice@example.com"}}
	result, err := Emit(cfg, nil)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	src := result.Files["composer.json"]
	if !strings.Contains(src, "Alice") {
		t.Errorf("expected author Alice; got:\n%s", src)
	}
}

func TestEmitREADME(t *testing.T) {
	result, err := Emit(defaultConfig(), nil)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	src := result.Files["README.md"]
	if !strings.Contains(src, "acme/my-lib") {
		t.Errorf("expected package name in README; got:\n%s", src)
	}
	if !strings.Contains(src, "composer require") {
		t.Errorf("expected installation instructions; got:\n%s", src)
	}
}

func TestEmitLicense(t *testing.T) {
	cfg := defaultConfig()
	cfg.Authors = []Author{{Name: "Acme Corp"}}
	result, err := Emit(cfg, nil)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	src := result.Files["LICENSE"]
	if !strings.Contains(src, "MIT License") {
		t.Errorf("expected MIT License; got:\n%s", src)
	}
	if !strings.Contains(src, "Acme Corp") {
		t.Errorf("expected author in LICENSE; got:\n%s", src)
	}
}

func TestEmitClassFile(t *testing.T) {
	cfg := defaultConfig()
	classes := []ClassFile{
		{
			FQCN:   `Acme\MyLib\Client`,
			Source: "<?php\ndeclare(strict_types=1);\nnamespace Acme\\MyLib;\nclass Client {}\n",
		},
	}
	result, err := Emit(cfg, classes)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	path := "src/Acme/MyLib/Client.php"
	src, ok := result.Files[path]
	if !ok {
		t.Fatalf("expected %q; got files: %v", path, fileKeys(result.Files))
	}
	if !strings.Contains(src, "class Client") {
		t.Errorf("expected class Client in file; got:\n%s", src)
	}
}

func TestEmitMultipleClasses(t *testing.T) {
	cfg := defaultConfig()
	classes := []ClassFile{
		{FQCN: `Acme\MyLib\Client`, Source: "<?php class Client {}"},
		{FQCN: `Acme\MyLib\Response`, Source: "<?php class Response {}"},
		{FQCN: `Acme\MyLib\Http\Request`, Source: "<?php class Request {}"},
	}
	result, err := Emit(cfg, classes)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	expected := []string{
		"src/Acme/MyLib/Client.php",
		"src/Acme/MyLib/Response.php",
		"src/Acme/MyLib/Http/Request.php",
	}
	for _, path := range expected {
		if _, ok := result.Files[path]; !ok {
			t.Errorf("expected file %q; got: %v", path, fileKeys(result.Files))
		}
	}
}

func TestClassFilePath(t *testing.T) {
	cases := []struct {
		nsRoot, fqcn, want string
	}{
		{`Acme\MyLib`, `Acme\MyLib\Client`, "Acme/MyLib/Client.php"},
		{`Acme\MyLib`, `Acme\MyLib\Http\Request`, "Acme/MyLib/Http/Request.php"},
		{`Foo`, `Foo\Bar`, "Foo/Bar.php"},
		{`Foo`, `\Foo\Bar`, "Foo/Bar.php"},
	}
	for _, tc := range cases {
		got := classFilePath(tc.nsRoot, tc.fqcn)
		if got != tc.want {
			t.Errorf("classFilePath(%q, %q) = %q; want %q", tc.nsRoot, tc.fqcn, got, tc.want)
		}
	}
}

func TestDefaultPHPRequire(t *testing.T) {
	cfg := Config{
		ComposerName:  "x/y",
		PSR4Namespace: `X\Y`,
		// PHPRequire intentionally empty
	}
	result, err := Emit(cfg, nil)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	src := result.Files["composer.json"]
	if !strings.Contains(src, "^8.4") {
		t.Errorf("expected default ^8.4 PHP require; got:\n%s", src)
	}
}

func fileKeys(m map[string]string) []string {
	keys := make([]string, 0, len(m))
	for k := range m {
		keys = append(keys, k)
	}
	return keys
}
