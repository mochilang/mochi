package deno_test

import (
	"os/exec"
	"testing"

	deno "mochi/runtime/ffi/deno"
	ffiinfo "mochi/runtime/ffi/infer"
)

func TestInfer(t *testing.T) {
	if _, err := exec.LookPath("deno"); err != nil {
		t.Skip("deno not installed")
	}

	info, err := deno.Infer("testpkg.ts")
	if err != nil {
		t.Fatalf("infer failed: %v", err)
	}

	var add ffiinfo.FuncInfo
	foundAdd := false
	var failFunc ffiinfo.FuncInfo
	foundFail := false
	for _, f := range info.Functions {
		switch f.Name {
		case "add":
			add = f
			foundAdd = true
		case "fail":
			failFunc = f
			foundFail = true
		}
	}
	if !foundAdd {
		t.Fatalf("expected add function in inference results")
	}
	if len(add.Params) != 2 || add.Params[0].Type != "number" || add.Params[1].Type != "number" {
		t.Fatalf("add params incorrect: %+v", add.Params)
	}
	if len(add.Results) != 1 || add.Results[0].Type != "number" {
		t.Fatalf("add results incorrect: %+v", add.Results)
	}
	if add.Doc == "" {
		t.Fatalf("add doc missing")
	}
	if !foundFail {
		t.Fatalf("expected fail function")
	}
	if len(failFunc.Results) != 1 || failFunc.Results[0].Type != "never" {
		t.Fatalf("fail results incorrect: %+v", failFunc.Results)
	}

	foundPi := false
	for _, c := range info.Consts {
		if c.Name == "PI" {
			if c.Doc == "" || c.Type == "" {
				t.Fatalf("unexpected PI details: %+v", c)
			}
			foundPi = true
			break
		}
	}
	if !foundPi {
		t.Fatalf("expected PI constant")
	}

	foundAnswer := false
	for _, v := range info.Vars {
		if v.Name == "answer" {
			if v.Type != "number" {
				t.Fatalf("unexpected answer type: %s", v.Type)
			}
			foundAnswer = true
			break
		}
	}
	if !foundAnswer {
		t.Fatalf("expected answer variable")
	}

	foundPoint := false
	for _, tinfo := range info.Types {
		if tinfo.Name == "Point" {
			if tinfo.Kind != "interface" {
				t.Fatalf("Point kind should be interface, got %s", tinfo.Kind)
			}
			foundPoint = true
			break
		}
	}
	if !foundPoint {
		t.Fatalf("expected Point type")
	}
}
