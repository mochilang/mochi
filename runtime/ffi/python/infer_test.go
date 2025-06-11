package python_test

import (
	"testing"

	"mochi/runtime/ffi/python"
)

func TestInfer(t *testing.T) {
	info, err := python.Infer("re")
	if err != nil {
		t.Fatalf("infer failed: %v", err)
	}

	foundCompile := false
	for _, f := range info.Functions {
		if f.Name == "re.compile" {
			foundCompile = true
			break
		}
	}
	if !foundCompile {
		t.Fatalf("expected compile function in inference results")
	}

	foundFlag := false
	for _, c := range info.Consts {
		if c.Name == "re.IGNORECASE" {
			foundFlag = true
			break
		}
	}
	if !foundFlag {
		t.Fatalf("expected IGNORECASE constant")
	}

	foundPattern := false
	for _, tinfo := range info.Types {
		if tinfo.Name == "re.Pattern" {
			if tinfo.Kind != "class" {
				t.Fatalf("Pattern kind should be class, got %s", tinfo.Kind)
			}
			foundPattern = true
			break
		}
	}
	if !foundPattern {
		t.Fatalf("expected Pattern type")
	}
}
