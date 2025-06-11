package goffi_test

import (
	"testing"

	goffi "mochi/runtime/ffi/go"
	ffiinfo "mochi/runtime/ffi/infer"
)

func TestInfer(t *testing.T) {
	info, err := goffi.Infer("mochi/runtime/ffi/go/testpkg")
	if err != nil {
		t.Fatalf("infer failed: %v", err)
	}

	var add ffiinfo.FuncInfo
	foundAdd := false
	for _, f := range info.Functions {
		if f.Name == "Add" {
			add = f
			foundAdd = true
			break
		}
	}
	if !foundAdd {
		t.Fatalf("expected Add function in inference results")
	}
	if len(add.Params) != 2 || add.Params[0].Type != "int" || add.Params[1].Type != "int" {
		t.Fatalf("Add params incorrect: %+v", add.Params)
	}
	if len(add.Results) != 1 || add.Results[0].Type != "int" {
		t.Fatalf("Add results incorrect: %+v", add.Results)
	}
	if add.Doc == "" {
		t.Fatalf("Add doc missing")
	}
	if len(add.Examples) == 0 {
		t.Fatalf("Add examples missing")
	}

	foundPi := false
	for _, c := range info.Consts {
		if c.Name == "Pi" {
			if c.Doc == "" || c.Value != "3.14" {
				t.Fatalf("unexpected Pi details: %+v", c)
			}
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
			if v.Doc == "" {
				t.Fatalf("expected docs for Answer")
			}
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
			if len(tinfo.Fields) != 2 || tinfo.Fields[0].Name != "X" {
				t.Fatalf("unexpected Point fields: %+v", tinfo.Fields)
			}
			foundPoint = true
			break
		}
	}
	if !foundPoint {
		t.Fatalf("expected Point type")
	}
}
