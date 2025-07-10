package goffi_test

import (
	"strings"
	"testing"

	goffi "mochi/runtime/ffi/go"
	ffiinfo "mochi/runtime/ffi/infer"
)

func TestInfer(t *testing.T) {
	info, err := goffi.Infer("mochi/runtime/ffi/go/testpkg")
	if err != nil {
		t.Fatalf("infer failed: %v", err)
	}

	if info.Path != "mochi/runtime/ffi/go/testpkg" {
		t.Fatalf("unexpected module path %s", info.Path)
	}

	if len(info.Functions) < 3 {
		t.Fatalf("expected at least 3 functions, got %d", len(info.Functions))
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
	if !strings.Contains(add.Examples[0].Code, "Add(2, 3)") {
		t.Fatalf("Add example code incorrect: %s", add.Examples[0].Code)
	}
	if strings.TrimSpace(add.Examples[0].Output) != "5" {
		t.Fatalf("Add example output incorrect: %q", add.Examples[0].Output)
	}

	foundMD5 := false
	for _, f := range info.Functions {
		if f.Name == "MD5Hex" {
			foundMD5 = true
			if len(f.Params) != 1 || f.Params[0].Type != "string" {
				t.Fatalf("MD5Hex params incorrect: %+v", f.Params)
			}
			if len(f.Results) != 1 || f.Results[0].Type != "string" {
				t.Fatalf("MD5Hex results incorrect: %+v", f.Results)
			}
			break
		}
	}
	if !foundMD5 {
		t.Fatalf("expected MD5Hex function in inference results")
	}

	foundFail := false
	for _, f := range info.Functions {
		if f.Name == "Fail" {
			foundFail = true
			if len(f.Params) != 0 {
				t.Fatalf("Fail should have no params")
			}
			if len(f.Results) != 1 || f.Results[0].Type != "error" {
				t.Fatalf("Fail results incorrect: %+v", f.Results)
			}
			break
		}
	}
	if !foundFail {
		t.Fatalf("expected Fail function in inference results")
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
