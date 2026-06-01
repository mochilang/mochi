package concurrency_test

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/package3/swift/concurrency"
	"mochi/package3/swift/typemap"
)

// TestPhase14Concurrency is the MEP-69 Phase 14 gate for Swift structured concurrency bridge.
func TestPhase14Concurrency(t *testing.T) {
	specs := []concurrency.SequenceSpec{
		{
			MochiName:         "swift_Pkg_intStream",
			SwiftSequenceName: "Pkg.IntStream",
			Module:            "Pkg",
			ElementKind:       typemap.KindInt,
		},
		{
			MochiName:         "swift_Pkg_stringStream",
			SwiftSequenceName: "Pkg.StringStream",
			Module:            "Pkg",
			ElementKind:       typemap.KindString,
		},
	}

	outDir := t.TempDir()
	outPath := filepath.Join(outDir, "ConcurrencyBridge.swift")
	if err := concurrency.EmitAsyncSequenceBridge(specs, outPath); err != nil {
		t.Fatalf("EmitAsyncSequenceBridge: %v", err)
	}

	data, err := os.ReadFile(outPath)
	if err != nil {
		t.Fatalf("ReadFile: %v", err)
	}
	sw := string(data)

	if !strings.Contains(sw, "DispatchGroup") {
		t.Error("missing DispatchGroup")
	}
	if !strings.Contains(sw, "@_cdecl") {
		t.Error("missing @_cdecl")
	}
	if !strings.Contains(sw, "for await element") {
		t.Error("missing for await element")
	}
	if !strings.Contains(sw, "swift_Pkg_intStream") {
		t.Error("missing swift_Pkg_intStream")
	}
	if !strings.Contains(sw, "swift_Pkg_stringStream") {
		t.Error("missing swift_Pkg_stringStream")
	}
}
