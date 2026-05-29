package pharemit

import (
	"strings"
	"testing"
)

func defaultConfig() Config {
	return Config{
		PharName:      "acme-mylib",
		PSR4Namespace: `Acme\MyLib`,
	}
}

func TestBuildRequiredPharName(t *testing.T) {
	_, err := Build(Config{PSR4Namespace: `A\B`})
	if err == nil {
		t.Error("expected error for empty PharName")
	}
}

func TestBuildRequiredPSR4Namespace(t *testing.T) {
	_, err := Build(Config{PharName: "foo"})
	if err == nil {
		t.Error("expected error for empty PSR4Namespace")
	}
}

func TestBuildProducesExpectedFiles(t *testing.T) {
	result, err := Build(defaultConfig())
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	for _, want := range []string{"stub.php", "build.php"} {
		if _, ok := result.Files[want]; !ok {
			t.Errorf("expected file %q in result", want)
		}
	}
}

func TestBuildPharFileName(t *testing.T) {
	result, err := Build(defaultConfig())
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result.PharFileName != "acme-mylib.phar" {
		t.Errorf("PharFileName = %q; want acme-mylib.phar", result.PharFileName)
	}
}

func TestStubContainsPharMapPhar(t *testing.T) {
	result, err := Build(defaultConfig())
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	stub := result.Files["stub.php"]
	if !strings.Contains(stub, "Phar::mapPhar") {
		t.Errorf("expected Phar::mapPhar in stub; got:\n%s", stub)
	}
	if !strings.Contains(stub, "__HALT_COMPILER") {
		t.Errorf("expected __HALT_COMPILER in stub; got:\n%s", stub)
	}
}

func TestStubContainsPSR4Autoloader(t *testing.T) {
	result, err := Build(defaultConfig())
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	stub := result.Files["stub.php"]
	if !strings.Contains(stub, "spl_autoload_register") {
		t.Errorf("expected spl_autoload_register in stub; got:\n%s", stub)
	}
	if !strings.Contains(stub, "Acme\\MyLib") {
		t.Errorf("expected namespace in stub; got:\n%s", stub)
	}
}

func TestStubContainsPharName(t *testing.T) {
	result, err := Build(defaultConfig())
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	stub := result.Files["stub.php"]
	if !strings.Contains(stub, "acme-mylib.phar") {
		t.Errorf("expected phar file name in stub; got:\n%s", stub)
	}
}

func TestBuildScriptContainsRecursiveIterator(t *testing.T) {
	result, err := Build(defaultConfig())
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	build := result.Files["build.php"]
	if !strings.Contains(build, "RecursiveIteratorIterator") {
		t.Errorf("expected RecursiveIteratorIterator in build.php; got:\n%s", build)
	}
	if !strings.Contains(build, "addFile") {
		t.Errorf("expected addFile in build.php; got:\n%s", build)
	}
	if !strings.Contains(build, "setStub") {
		t.Errorf("expected setStub in build.php; got:\n%s", build)
	}
}

func TestBuildScriptDefaultSrcDir(t *testing.T) {
	result, err := Build(defaultConfig())
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	build := result.Files["build.php"]
	if !strings.Contains(build, `"src"`) {
		t.Errorf("expected default src/ dir in build.php; got:\n%s", build)
	}
}

func TestBuildScriptCustomSrcDir(t *testing.T) {
	cfg := defaultConfig()
	cfg.SrcDir = "lib"
	result, err := Build(cfg)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	build := result.Files["build.php"]
	if !strings.Contains(build, `"lib"`) {
		t.Errorf("expected custom lib/ dir in build.php; got:\n%s", build)
	}
}

func TestBuildCompressionGZ(t *testing.T) {
	cfg := defaultConfig()
	cfg.Compression = CompressGZ
	result, err := Build(cfg)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	build := result.Files["build.php"]
	if !strings.Contains(build, "Phar::GZ") {
		t.Errorf("expected Phar::GZ compression; got:\n%s", build)
	}
}

func TestBuildCompressionBZ2(t *testing.T) {
	cfg := defaultConfig()
	cfg.Compression = CompressBZ2
	result, err := Build(cfg)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	build := result.Files["build.php"]
	if !strings.Contains(build, "Phar::BZ2") {
		t.Errorf("expected Phar::BZ2 compression; got:\n%s", build)
	}
}

func TestBuildCompressionNoneNoCall(t *testing.T) {
	result, err := Build(defaultConfig())
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	build := result.Files["build.php"]
	if strings.Contains(build, "compressFiles") {
		t.Errorf("expected no compressFiles for CompressNone; got:\n%s", build)
	}
}

func TestStubPreamble(t *testing.T) {
	cfg := defaultConfig()
	cfg.StubPreamble = "Auto-generated. Do not edit."
	result, err := Build(cfg)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	stub := result.Files["stub.php"]
	if !strings.Contains(stub, "Auto-generated. Do not edit.") {
		t.Errorf("expected preamble in stub; got:\n%s", stub)
	}
}
