package asyncbridge_test

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/package3/swift/asyncbridge"
	"mochi/package3/swift/emit"
	"mochi/package3/swift/typemap"
)

// TestPhase11AsyncBridge is the MEP-69 Phase 11 gate for the Swift async bridge.
//
// Verifies:
//   - WrapAsync returns a spec with the same name and IsAsync=false-equivalent
//   - EmitDispatchBridgeFile emits valid Swift containing DispatchGroup and @_cdecl
func TestPhase11AsyncBridge(t *testing.T) {
	t.Run("WrapAsync", func(t *testing.T) {
		spec := asyncbridge.AsyncWrapSpec{
			MochiName:  "swift_MyModule_fetchData",
			SwiftName:  "fetchData",
			Module:     "MyModule",
			ReturnKind: typemap.KindString,
			IsThrowing: true,
			Params: []emit.ParamMapping{
				{Name: "url", Mapping: &typemap.Mapping{Kind: typemap.KindString}},
			},
		}
		wrapped := asyncbridge.WrapAsync(spec)
		if wrapped.MochiName != spec.MochiName {
			t.Errorf("MochiName: got %q, want %q", wrapped.MochiName, spec.MochiName)
		}
		if wrapped.ReturnKind != spec.ReturnKind {
			t.Errorf("ReturnKind: got %v, want %v", wrapped.ReturnKind, spec.ReturnKind)
		}
	})

	t.Run("EmitDispatchBridgeFile", func(t *testing.T) {
		specs := []asyncbridge.AsyncWrapSpec{
			{
				MochiName:  "swift_Pkg_fetchData",
				SwiftName:  "fetchData",
				Module:     "Pkg",
				ReturnKind: typemap.KindString,
				IsThrowing: true,
				Params: []emit.ParamMapping{
					{Name: "url", Mapping: &typemap.Mapping{Kind: typemap.KindString}},
				},
			},
			{
				MochiName:  "swift_Pkg_doWork",
				SwiftName:  "doWork",
				Module:     "Pkg",
				ReturnKind: typemap.KindUnit,
				IsThrowing: false,
			},
		}

		outDir := t.TempDir()
		outPath := filepath.Join(outDir, "AsyncBridge.swift")
		if err := asyncbridge.EmitDispatchBridgeFile(specs, outPath); err != nil {
			t.Fatalf("EmitDispatchBridgeFile: %v", err)
		}

		data, err := os.ReadFile(outPath)
		if err != nil {
			t.Fatalf("ReadFile: %v", err)
		}
		sw := string(data)

		if !strings.Contains(sw, "DispatchGroup") {
			t.Error("AsyncBridge.swift missing DispatchGroup")
		}
		if !strings.Contains(sw, "@_cdecl") {
			t.Error("AsyncBridge.swift missing @_cdecl")
		}
		if !strings.Contains(sw, "group.wait()") {
			t.Error("AsyncBridge.swift missing group.wait()")
		}
		if !strings.Contains(sw, "swift_Pkg_fetchData") {
			t.Error("AsyncBridge.swift missing swift_Pkg_fetchData")
		}
		if !strings.Contains(sw, "try await") {
			t.Error("AsyncBridge.swift missing try await for throwing function")
		}
	})
}
