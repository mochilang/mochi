package python_test

import (
	"os"
	"testing"

	ffiinfo "mochi/runtime/ffi/infer"
	"mochi/runtime/ffi/python"
)

func TestInfer(t *testing.T) {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatalf("getwd failed: %v", err)
	}
	os.Setenv("PYTHONPATH", dir)
	defer os.Unsetenv("PYTHONPATH")

	info, err := python.Infer("testmod")
	if err != nil {
		t.Fatalf("infer failed: %v", err)
	}

	if info.Path != "testmod" {
		t.Fatalf("expected path testmod, got %s", info.Path)
	}
	if info.Doc == "" {
		t.Fatalf("expected module doc")
	}

	// check function add
	var add *ffiinfo.FuncInfo
	for i := range info.Functions {
		if info.Functions[i].Name == "add" {
			add = &info.Functions[i]
			break
		}
	}
	if add == nil {
		t.Fatalf("add function not found")
	}
	if add.Doc == "" {
		t.Fatalf("add doc missing")
	}
	if len(add.Params) != 2 || add.Params[0].Type != "int" || add.Params[1].Type != "int" {
		t.Fatalf("add params incorrect: %+v", add.Params)
	}
	if len(add.Results) != 1 || add.Results[0].Type != "int" {
		t.Fatalf("add results incorrect: %+v", add.Results)
	}

	// check constant PI
	foundPi := false
	for _, c := range info.Consts {
		if c.Name == "PI" {
			if c.Type != "float" || c.Value != "3.14" {
				t.Fatalf("PI constant details wrong: %+v", c)
			}
			foundPi = true
			break
		}
	}
	if !foundPi {
		t.Fatalf("PI constant not found")
	}

	// check variable answer
	foundAnswer := false
	for _, v := range info.Vars {
		if v.Name == "answer" {
			if v.Type != "int" {
				t.Fatalf("answer type wrong: %+v", v)
			}
			foundAnswer = true
			break
		}
	}
	if !foundAnswer {
		t.Fatalf("answer variable not found")
	}

	// check class Point
	var point *ffiinfo.TypeInfo
	for i := range info.Types {
		if info.Types[i].Name == "Point" {
			point = &info.Types[i]
			break
		}
	}
	if point == nil {
		t.Fatalf("Point type not found")
	}
	if point.Kind != "class" {
		t.Fatalf("Point kind should be class, got %s", point.Kind)
	}
	if point.Doc == "" {
		t.Fatalf("Point doc missing")
	}
	if len(point.Fields) != 2 {
		t.Fatalf("unexpected Point fields: %+v", point.Fields)
	}
	// expecting x,y fields
	if point.Fields[0].Name != "x" || point.Fields[1].Name != "y" {
		t.Fatalf("unexpected Point fields order or names: %+v", point.Fields)
	}
	if point.Fields[0].Type != "int" || point.Fields[1].Type != "int" {
		t.Fatalf("unexpected Point field types: %+v", point.Fields)
	}

	foundMag := false
	for _, m := range point.Methods {
		if m.Name == "magnitude" {
			if m.Doc == "" {
				t.Fatalf("magnitude doc missing")
			}
			if len(m.Results) != 1 || m.Results[0].Type != "float" {
				t.Fatalf("magnitude return wrong: %+v", m.Results)
			}
			foundMag = true
			break
		}
	}
	if !foundMag {
		t.Fatalf("magnitude method not found")
	}
}
