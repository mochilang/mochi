package python_test

import (
	"os/exec"
	"testing"

	ffiinfo "mochi/runtime/ffi/infer"
	"mochi/runtime/ffi/python"
)

func TestRealInfer(t *testing.T) {
	if _, err := exec.LookPath("python3"); err != nil {
		t.Skip("python3 not installed")
	}

	info, err := python.Infer("textwrap")
	if err != nil {
		t.Fatalf("infer failed: %v", err)
	}
	if info.Path != "textwrap" {
		t.Fatalf("unexpected module path %s", info.Path)
	}
	if info.Doc == "" {
		t.Fatalf("expected module doc")
	}

	var wrap *ffiinfo.FuncInfo
	for i := range info.Functions {
		if info.Functions[i].Name == "wrap" {
			wrap = &info.Functions[i]
			break
		}
	}
	if wrap == nil {
		t.Fatalf("wrap function not found")
	}
	if len(wrap.Params) == 0 {
		t.Fatalf("wrap params missing")
	}

	var tw *ffiinfo.TypeInfo
	for i := range info.Types {
		if info.Types[i].Name == "TextWrapper" {
			tw = &info.Types[i]
			break
		}
	}
	if tw == nil {
		t.Fatalf("TextWrapper type not found")
	}
	if tw.Kind != "class" {
		t.Fatalf("TextWrapper kind should be class, got %s", tw.Kind)
	}
	if len(tw.Methods) == 0 {
		t.Fatalf("TextWrapper methods missing")
	}
}
