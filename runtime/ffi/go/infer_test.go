package goffi_test

import (
	"testing"

	goffi "mochi/runtime/ffi/go"
)

func TestInfer(t *testing.T) {
	info, err := goffi.Infer("mochi/runtime/ffi/go/testpkg")
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
		if c.Name == "Pi" {
			foundPi = true
			break
		}
	}
	if !foundPi {
		t.Fatalf("expected Pi constant")
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
			if tinfo.Kind != "struct" {
				t.Fatalf("Point kind should be struct, got %s", tinfo.Kind)
			}
			foundPoint = true
			break
		}
	}
	if !foundPoint {
		t.Fatalf("expected Point type")
	}
}
