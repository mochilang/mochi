package cdeclwrapper_test

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/package3/swift/cdeclwrapper"
	"mochi/package3/swift/emit"
	"mochi/package3/swift/typemap"
)

func TestEmitBridgeFile(t *testing.T) {
	dir := t.TempDir()
	outPath := filepath.Join(dir, "MochiBridge.swift")

	spec := cdeclwrapper.WrapperSpec{
		Module: "MyModule",
		Functions: []emit.MappedFn{
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
					{Name: "a", Mapping: typemap.Map("Int32")},
					{Name: "b", Mapping: typemap.Map("Int32")},
				},
				Return:  typemap.Map("Int32"),
				IsAsync: false,
			},
		},
	}

	if err := cdeclwrapper.EmitBridgeFile(spec, outPath); err != nil {
		t.Fatalf("EmitBridgeFile error: %v", err)
	}

	data, err := os.ReadFile(outPath)
	if err != nil {
		t.Fatalf("ReadFile error: %v", err)
	}

	content := string(data)
	if !strings.Contains(content, `@_cdecl`) {
		t.Errorf("missing @_cdecl annotation, got:\n%s", content)
	}
	if !strings.Contains(content, "UnsafeMutablePointer<CChar>") {
		t.Errorf("String return should use UnsafeMutablePointer<CChar>, got:\n%s", content)
	}
	if !strings.Contains(content, "UnsafeMutablePointer<CChar>") {
		t.Errorf("String param should use pointer type, got:\n%s", content)
	}
	if !strings.Contains(content, "Int32") {
		t.Errorf("Int32 params should appear directly, got:\n%s", content)
	}
	if !strings.Contains(content, "import Foundation") {
		t.Errorf("missing Foundation import, got:\n%s", content)
	}
}

func TestEmitBridgeFileAsyncSkipped(t *testing.T) {
	dir := t.TempDir()
	outPath := filepath.Join(dir, "MochiBridge.swift")

	spec := cdeclwrapper.WrapperSpec{
		Module: "M",
		Functions: []emit.MappedFn{
			{
				SwiftName: "fetchAsync",
				MochiName: "fetchAsync",
				Params:    nil,
				Return:    typemap.Map("String"),
				IsAsync:   true,
			},
		},
	}

	if err := cdeclwrapper.EmitBridgeFile(spec, outPath); err != nil {
		t.Fatalf("EmitBridgeFile error: %v", err)
	}

	data, _ := os.ReadFile(outPath)
	content := string(data)
	if !strings.Contains(content, "SKIPPED") {
		t.Errorf("async function should produce SKIPPED comment, got:\n%s", content)
	}
}
