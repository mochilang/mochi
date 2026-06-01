package promise_test

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/package3/typescript/promise"
)

// TestPhase14Promise is the MEP-72 Phase 14 gate for the promise package.
func TestPhase14Promise(t *testing.T) {
	// ClassifyReturn.
	cases := []struct {
		ts   string
		want promise.ReturnKind
	}{
		{"string", promise.ReturnSync},
		{"Promise<string>", promise.ReturnPromise},
		{"AsyncIterable<number>", promise.ReturnAsyncIterable},
		{"void", promise.ReturnSync},
	}
	for _, c := range cases {
		got := promise.ClassifyReturn(c.ts)
		if got != c.want {
			t.Errorf("ClassifyReturn(%q) = %v, want %v", c.ts, got, c.want)
		}
	}

	// ExtractInner.
	if got := promise.ExtractInner("Promise<string>"); got != "string" {
		t.Errorf("ExtractInner Promise<string> = %q", got)
	}
	if got := promise.ExtractInner("AsyncIterable<number>"); got != "number" {
		t.Errorf("ExtractInner AsyncIterable<number> = %q", got)
	}
	if got := promise.ExtractInner("string"); got != "string" {
		t.Errorf("ExtractInner string = %q", got)
	}

	// EmitPromiseBridge.
	specs := []promise.AsyncSpec{
		{
			MochiName:   "ts_my_pkg_fetchData",
			JSName:      "fetchData",
			Module:      "my-pkg",
			ReturnKind:  promise.ReturnPromise,
			ElementType: "string",
			Params:      []promise.AsyncParam{{Name: "url", Type: "string"}},
		},
		{
			MochiName:   "ts_my_pkg_streamLines",
			JSName:      "streamLines",
			Module:      "my-pkg",
			ReturnKind:  promise.ReturnAsyncIterable,
			ElementType: "string",
			Params:      []promise.AsyncParam{{Name: "path", Type: "string"}},
		},
	}
	outDir := t.TempDir()
	outPath := filepath.Join(outDir, "promise_bridge.mjs")
	if err := promise.EmitPromiseBridge(specs, outPath); err != nil {
		t.Fatalf("EmitPromiseBridge: %v", err)
	}
	data, err := os.ReadFile(outPath)
	if err != nil {
		t.Fatalf("read bridge: %v", err)
	}
	content := string(data)
	if !strings.Contains(content, "ts_my_pkg_fetchData") {
		t.Error("bridge missing fetchData wrapper")
	}
	if !strings.Contains(content, "for await") {
		t.Error("bridge missing for-await-of for AsyncIterable")
	}
}
