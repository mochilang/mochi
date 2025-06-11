package python_test

import (
	"testing"

	"mochi/runtime/ffi/python"
)

func TestInfer(t *testing.T) {
	info, err := python.Infer("testpkg")
	if err != nil {
		t.Fatalf("infer failed: %v", err)
	}

	foundAdd := false
	for _, f := range info.Functions {
		if f.Name == "Add" {
			foundAdd = true
			break
		}
	}
	if !foundAdd {
		t.Fatalf("expected Add function in inference results")
	}

	foundPi := false
	for _, c := range info.Consts {
		if c.Name == "PI" {
			foundPi = true
			break
		}
	}
	if !foundPi {
		t.Fatalf("expected PI constant")
	}

	foundAnswer := false
	for _, v := range info.Vars {
		if v.Name == "Answer" {
			foundAnswer = true
			break
		}
	}
	if !foundAnswer {
		t.Fatalf("expected Answer variable")
	}

	foundPoint := false
	for _, tinfo := range info.Types {
		if tinfo.Name == "Point" {
			if tinfo.Kind != "class" {
				t.Fatalf("Point kind should be class, got %s", tinfo.Kind)
			}
			foundPoint = true
			break
		}
	}
	if !foundPoint {
		t.Fatalf("expected Point type")
	}
}
