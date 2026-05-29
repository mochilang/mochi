package asyncbridge

import (
	"strings"
	"testing"
)

// TestPhase12AsyncBridge is the Phase 12 umbrella sentinel. It builds
// two complete shim modules (one per loop mode) covering the surface
// every importer sees and asserts the renderer:
//
//  1. Emits a future-import + asyncio import preamble.
//  2. Re-exports the source import line so the shim can call back into
//     the async fns.
//  3. Embeds the cross-loop hazard helper + MochiAsyncReentryError class.
//  4. Drives every shim through asyncio.run in PerCall mode and through
//     _mochi_get_loop().run_until_complete in Persistent mode (and only
//     declares _MOCHI_LOOP in the Persistent variant).
//  5. Round-trips deterministically (re-rendering yields byte-identical
//     output) and rejects invalid AsyncFn descriptors.
func TestPhase12AsyncBridge(t *testing.T) {
	fns := []AsyncFn{
		{
			Name:       "fetch",
			SyncName:   "fetch_sync",
			ParamNames: []string{"url", "timeout"},
			ParamTypes: []string{"str", "float"},
			Return:     "bytes",
		},
		{
			Name:       "compute",
			SyncName:   "compute_sync",
			ParamNames: []string{"n"},
			ParamTypes: []string{"int"},
			Return:     "int",
		},
	}

	perCall := Module{
		SourceImport: "from mochi_async_pkg import fetch, compute",
		Mode:         PerCall,
		Fns:          fns,
	}
	persistent := Module{
		SourceImport: "from mochi_async_pkg import fetch, compute",
		Mode:         Persistent,
		Fns:          fns,
	}

	pcSrc, err := perCall.Render()
	if err != nil {
		t.Fatalf("PerCall Render: %v", err)
	}
	psSrc, err := persistent.Render()
	if err != nil {
		t.Fatalf("Persistent Render: %v", err)
	}

	// Preamble + helper + source import are present in both.
	for _, src := range []string{pcSrc, psSrc} {
		for _, want := range []string{
			"from __future__ import annotations",
			"import asyncio",
			"from mochi_async_pkg import fetch, compute",
			"class MochiAsyncReentryError(RuntimeError)",
			"def _mochi_check_no_running_loop(fn_name)",
			"def fetch_sync(url: str, timeout: float) -> bytes:",
			"def compute_sync(n: int) -> int:",
			`_mochi_check_no_running_loop("fetch_sync")`,
			`_mochi_check_no_running_loop("compute_sync")`,
		} {
			if !strings.Contains(src, want) {
				t.Errorf("module missing %q\n%s", want, src)
			}
		}
	}

	// PerCall uses asyncio.run; Persistent uses the cached loop.
	if !strings.Contains(pcSrc, "asyncio.run(fetch(url, timeout))") {
		t.Errorf("per-call missing asyncio.run for fetch:\n%s", pcSrc)
	}
	if !strings.Contains(pcSrc, "asyncio.run(compute(n))") {
		t.Errorf("per-call missing asyncio.run for compute:\n%s", pcSrc)
	}
	if strings.Contains(pcSrc, "_MOCHI_LOOP") {
		t.Errorf("per-call must not declare _MOCHI_LOOP:\n%s", pcSrc)
	}

	if !strings.Contains(psSrc, "_MOCHI_LOOP = None") {
		t.Errorf("persistent missing _MOCHI_LOOP cache:\n%s", psSrc)
	}
	if !strings.Contains(psSrc, "_mochi_get_loop().run_until_complete(fetch(url, timeout))") {
		t.Errorf("persistent missing run_until_complete for fetch:\n%s", psSrc)
	}
	if !strings.Contains(psSrc, "_mochi_get_loop().run_until_complete(compute(n))") {
		t.Errorf("persistent missing run_until_complete for compute:\n%s", psSrc)
	}
	if strings.Contains(psSrc, "asyncio.run(") {
		t.Errorf("persistent must not call asyncio.run:\n%s", psSrc)
	}

	// Determinism.
	rerender, _ := perCall.Render()
	if rerender != pcSrc {
		t.Fatal("PerCall Render is not deterministic")
	}

	// Mode round-trip via the public parser.
	for _, m := range []Mode{PerCall, Persistent} {
		got, err := ParseMode(m.String())
		if err != nil || got != m {
			t.Errorf("ParseMode round-trip broken for %v: got %v (%v)", m, got, err)
		}
	}

	// Invalid descriptor surfaces from Module.Render.
	bad := Module{Mode: PerCall, Fns: []AsyncFn{{Name: "x"}}}
	if _, err := bad.Render(); err == nil {
		t.Fatal("expected Render to reject invalid AsyncFn")
	}
}
