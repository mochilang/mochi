package emit_test

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	swifterrors "mochi/package3/swift/errors"
	"mochi/package3/swift/emit"
	"mochi/package3/swift/typemap"
)

func TestEmitExterns(t *testing.T) {
	dir := t.TempDir()
	outPath := filepath.Join(dir, "externs.mochi")

	fns := []emit.MappedFn{
		{
			SwiftName: "greet",
			MochiName: "greet",
			Params: []emit.ParamMapping{
				{Name: "name", Mapping: typemap.Map("String")},
			},
			Return:  typemap.Map("String"),
			IsAsync: false,
		},
		{
			SwiftName: "add",
			MochiName: "add",
			Params: []emit.ParamMapping{
				{Name: "a", Mapping: typemap.Map("Int")},
				{Name: "b", Mapping: typemap.Map("Int")},
			},
			Return:  typemap.Map("Int"),
			IsAsync: false,
		},
		{
			SwiftName: "fetchData",
			MochiName: "fetchData",
			Params: []emit.ParamMapping{
				{Name: "url", Mapping: typemap.Map("String")},
			},
			Return:  typemap.Map("String"),
			IsAsync: true,
		},
	}

	if err := emit.EmitExterns("MyModule", fns, outPath); err != nil {
		t.Fatalf("EmitExterns error: %v", err)
	}

	data, err := os.ReadFile(outPath)
	if err != nil {
		t.Fatalf("ReadFile error: %v", err)
	}

	content := string(data)
	if !strings.Contains(content, "extern fn greet(name: str) -> str") {
		t.Errorf("missing greet extern, got:\n%s", content)
	}
	if !strings.Contains(content, "extern fn add(a: i64, b: i64) -> i64") {
		t.Errorf("missing add extern, got:\n%s", content)
	}
	if !strings.Contains(content, "extern async fn fetchData(url: str) -> str") {
		t.Errorf("missing fetchData async extern, got:\n%s", content)
	}
	if !strings.Contains(content, "Module: MyModule") {
		t.Errorf("missing module comment, got:\n%s", content)
	}
}

func TestEmitExternsDeterministic(t *testing.T) {
	dir := t.TempDir()
	fns := []emit.MappedFn{
		{MochiName: "zzz", Params: nil, Return: typemap.Map("Void")},
		{MochiName: "aaa", Params: nil, Return: typemap.Map("Int")},
	}

	out1 := filepath.Join(dir, "out1.mochi")
	out2 := filepath.Join(dir, "out2.mochi")

	if err := emit.EmitExterns("M", fns, out1); err != nil {
		t.Fatal(err)
	}
	if err := emit.EmitExterns("M", fns, out2); err != nil {
		t.Fatal(err)
	}

	d1, _ := os.ReadFile(out1)
	d2, _ := os.ReadFile(out2)
	if string(d1) != string(d2) {
		t.Error("EmitExterns output is not deterministic")
	}
	// aaa should appear before zzz (sorted)
	idx1 := strings.Index(string(d1), "aaa")
	idx2 := strings.Index(string(d1), "zzz")
	if idx1 > idx2 {
		t.Errorf("output not sorted: aaa at %d, zzz at %d", idx1, idx2)
	}
}

func TestEmitSkipped(t *testing.T) {
	dir := t.TempDir()
	outPath := filepath.Join(dir, "SKIPPED.txt")

	skipped := []emit.SkipReport{
		{ItemPath: "MyModule.closureFunc", Reason: swifterrors.SkipClosure, Detail: "param 0 is closure"},
		{ItemPath: "MyModule.genericFunc", Reason: swifterrors.SkipGeneric, Detail: "type param T"},
	}

	if err := emit.EmitSkipped(skipped, outPath); err != nil {
		t.Fatalf("EmitSkipped error: %v", err)
	}

	data, err := os.ReadFile(outPath)
	if err != nil {
		t.Fatalf("ReadFile error: %v", err)
	}

	content := string(data)
	if !strings.Contains(content, "MyModule.closureFunc") {
		t.Errorf("missing closureFunc in SKIPPED.txt")
	}
	if !strings.Contains(content, "SkipClosure") {
		t.Errorf("missing SkipClosure reason")
	}
	if !strings.Contains(content, "MyModule.genericFunc") {
		t.Errorf("missing genericFunc in SKIPPED.txt")
	}
}
