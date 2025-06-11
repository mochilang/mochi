package deno_test

import (
	"testing"

	deno "mochi/runtime/ffi/deno"
)

func TestInfer(t *testing.T) {
	info, err := deno.Infer("./testpkg/mod.ts")
	if err != nil {
		t.Fatalf("infer failed: %v", err)
	}

	checkFunc := func(name string) bool {
		for _, f := range info.Functions {
			if f.Name == name {
				return true
			}
		}
		return false
	}
	if !checkFunc("add") {
		t.Fatalf("expected add function")
	}
	if !checkFunc("square") {
		t.Fatalf("expected square function")
	}

	foundAnswer := false
	for _, c := range info.Consts {
		if c.Name == "answer" {
			foundAnswer = true
			break
		}
	}
	if !foundAnswer {
		t.Fatalf("expected answer constant")
	}

	foundCounter := false
	for _, v := range info.Vars {
		if v.Name == "counter" {
			foundCounter = true
			break
		}
	}
	if !foundCounter {
		t.Fatalf("expected counter variable")
	}

	foundPoint := false
	foundColor := false
	for _, tinfo := range info.Types {
		if tinfo.Name == "Point" && tinfo.Kind == "class" {
			foundPoint = true
		}
		if tinfo.Name == "Color" && tinfo.Kind == "enum" {
			foundColor = true
		}
	}
	if !foundPoint {
		t.Fatalf("expected Point class")
	}
	if !foundColor {
		t.Fatalf("expected Color enum")
	}
}
