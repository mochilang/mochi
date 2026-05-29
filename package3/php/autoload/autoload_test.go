package autoload

import (
	"strings"
	"testing"
)

func TestGenerateEmpty(t *testing.T) {
	src := Generate(Config{})
	if !strings.Contains(src, "<?php") {
		t.Errorf("expected PHP header; got:\n%s", src)
	}
	if strings.Contains(src, "spl_autoload_register") {
		t.Errorf("empty config should not emit autoloader; got:\n%s", src)
	}
}

func TestGeneratePSR4(t *testing.T) {
	cfg := Config{
		PSR4: []PSR4Entry{
			{Prefix: `GuzzleHttp\`, Dir: "guzzlehttp/guzzle/src"},
		},
	}
	src := Generate(cfg)
	if !strings.Contains(src, "spl_autoload_register") {
		t.Errorf("expected spl_autoload_register; got:\n%s", src)
	}
	if !strings.Contains(src, `"GuzzleHttp\\"`) {
		t.Errorf("expected escaped prefix in output; got:\n%s", src)
	}
	if !strings.Contains(src, "guzzlehttp/guzzle/src") {
		t.Errorf("expected source dir; got:\n%s", src)
	}
	if !strings.Contains(src, "str_replace") {
		t.Errorf("expected namespace-to-path conversion; got:\n%s", src)
	}
}

func TestGenerateMultiplePSR4Sorted(t *testing.T) {
	cfg := Config{
		PSR4: []PSR4Entry{
			{Prefix: `Symfony\`, Dir: "symfony/console/src"},
			{Prefix: `GuzzleHttp\`, Dir: "guzzlehttp/guzzle/src"},
			{Prefix: `Psr\`, Dir: "psr/log/src"},
		},
	}
	src := Generate(cfg)
	// Check sorted order: GuzzleHttp, Psr, Symfony
	gPos := strings.Index(src, "GuzzleHttp")
	pPos := strings.Index(src, "Psr")
	sPos := strings.Index(src, "Symfony")
	if gPos < 0 || pPos < 0 || sPos < 0 {
		t.Fatalf("all prefixes should be present")
	}
	if !(gPos < pPos && pPos < sPos) {
		t.Errorf("PSR-4 entries should be sorted alphabetically; got positions G=%d P=%d S=%d", gPos, pPos, sPos)
	}
}

func TestGenerateClassmap(t *testing.T) {
	cfg := Config{
		Classmap: []ClassmapEntry{
			{FQCN: `GuzzleHttp\Client`, File: "guzzlehttp/guzzle/src/Client.php"},
		},
	}
	src := Generate(cfg)
	if !strings.Contains(src, `"GuzzleHttp\\Client"`) {
		t.Errorf("expected escaped FQCN in classmap; got:\n%s", src)
	}
	if !strings.Contains(src, "Client.php") {
		t.Errorf("expected file path in classmap; got:\n%s", src)
	}
	if !strings.Contains(src, "isset($classmap[$class])") {
		t.Errorf("expected classmap lookup; got:\n%s", src)
	}
}

func TestGenerateFiles(t *testing.T) {
	cfg := Config{
		Files: []FilesEntry{
			{File: "vlucas/phpdotenv/src/functions.php"},
		},
	}
	src := Generate(cfg)
	if !strings.Contains(src, "require_once") {
		t.Errorf("expected require_once for files; got:\n%s", src)
	}
	if !strings.Contains(src, "functions.php") {
		t.Errorf("expected file path; got:\n%s", src)
	}
}

func TestGenerateCombined(t *testing.T) {
	cfg := Config{
		PSR4: []PSR4Entry{
			{Prefix: `GuzzleHttp\`, Dir: "guzzlehttp/guzzle/src"},
		},
		Classmap: []ClassmapEntry{
			{FQCN: `Monolog\Logger`, File: "monolog/monolog/src/Monolog/Logger.php"},
		},
		Files: []FilesEntry{
			{File: "helpers.php"},
		},
	}
	src := Generate(cfg)
	if !strings.Contains(src, "GuzzleHttp") {
		t.Errorf("expected PSR-4 entry; got:\n%s", src)
	}
	if !strings.Contains(src, "Monolog") {
		t.Errorf("expected classmap entry; got:\n%s", src)
	}
	if !strings.Contains(src, "helpers.php") {
		t.Errorf("expected files entry; got:\n%s", src)
	}
}

func TestBuildConfig(t *testing.T) {
	pkgs := []PackageAutoload{
		{
			VendorDir: "guzzlehttp/guzzle",
			PSR4:      map[string]string{`GuzzleHttp\`: "src/"},
		},
		{
			VendorDir: "psr/log",
			PSR4:      map[string]string{`Psr\Log\`: "src/"},
		},
	}
	cfg := BuildConfig(pkgs)
	if len(cfg.PSR4) != 2 {
		t.Errorf("expected 2 PSR4 entries; got %d", len(cfg.PSR4))
	}
	// Verify dir is prefixed with VendorDir.
	found := false
	for _, e := range cfg.PSR4 {
		if strings.HasPrefix(e.Dir, "guzzlehttp/guzzle/") {
			found = true
		}
	}
	if !found {
		t.Errorf("expected guzzlehttp/guzzle prefix in Dir; got %v", cfg.PSR4)
	}
}

func TestBuildConfigClassmap(t *testing.T) {
	pkgs := []PackageAutoload{
		{
			VendorDir: "acme/lib",
			Classmap:  map[string]string{`Acme\Util`: "src/Util.php"},
		},
	}
	cfg := BuildConfig(pkgs)
	if len(cfg.Classmap) != 1 {
		t.Fatalf("expected 1 classmap entry; got %d", len(cfg.Classmap))
	}
	if cfg.Classmap[0].FQCN != `Acme\Util` {
		t.Errorf("FQCN = %q; want Acme\\Util", cfg.Classmap[0].FQCN)
	}
	if !strings.HasPrefix(cfg.Classmap[0].File, "acme/lib/") {
		t.Errorf("File should have VendorDir prefix; got %q", cfg.Classmap[0].File)
	}
}

func TestBuildConfigFiles(t *testing.T) {
	pkgs := []PackageAutoload{
		{
			VendorDir: "vlucas/phpdotenv",
			Files:     []string{"src/bootstrap.php"},
		},
	}
	cfg := BuildConfig(pkgs)
	if len(cfg.Files) != 1 {
		t.Fatalf("expected 1 files entry; got %d", len(cfg.Files))
	}
	if !strings.HasPrefix(cfg.Files[0].File, "vlucas/phpdotenv/") {
		t.Errorf("File should have VendorDir prefix; got %q", cfg.Files[0].File)
	}
}

func TestNormalisePrefix(t *testing.T) {
	cases := []struct{ in, want string }{
		{`GuzzleHttp\`, `GuzzleHttp\`},
		{`Psr`, `Psr\`},
		{`Foo\\`, `Foo\\`},
	}
	for _, tc := range cases {
		got := normalisePrefix(tc.in)
		if got != tc.want {
			t.Errorf("normalisePrefix(%q) = %q; want %q", tc.in, got, tc.want)
		}
	}
}

func TestGenerateHeader(t *testing.T) {
	src := Generate(Config{PSR4: []PSR4Entry{{Prefix: `A\`, Dir: "a/src"}}})
	if !strings.Contains(src, "Mochi PHP bridge") {
		t.Errorf("expected generated file header comment; got:\n%s", src)
	}
	if !strings.Contains(src, "do not edit manually") {
		t.Errorf("expected do not edit comment; got:\n%s", src)
	}
}
