package asyncbridge

import (
	"strings"
	"testing"
)

func TestRenderShimPerCall(t *testing.T) {
	out := RenderShim(AsyncFn{
		Name:       "fetch",
		SyncName:   "fetch_sync",
		ParamNames: []string{"url", "timeout"},
		ParamTypes: []string{"str", "float"},
		Return:     "bytes",
	}, PerCall)
	wantSubs := []string{
		"def fetch_sync(url: str, timeout: float) -> bytes:",
		`_mochi_check_no_running_loop("fetch_sync")`,
		"return asyncio.run(fetch(url, timeout))",
	}
	for _, w := range wantSubs {
		if !strings.Contains(out, w) {
			t.Errorf("missing %q in:\n%s", w, out)
		}
	}
	if strings.Contains(out, "_mochi_get_loop") {
		t.Errorf("per-call shim must not call _mochi_get_loop:\n%s", out)
	}
}

func TestRenderShimPersistent(t *testing.T) {
	out := RenderShim(AsyncFn{
		Name:       "tick",
		SyncName:   "tick_sync",
		ParamNames: []string{"n"},
		ParamTypes: []string{"int"},
		Return:     "int",
	}, Persistent)
	if !strings.Contains(out, "_mochi_get_loop().run_until_complete(tick(n))") {
		t.Errorf("persistent shim missing run_until_complete call:\n%s", out)
	}
	if strings.Contains(out, "asyncio.run(") {
		t.Errorf("persistent shim must not call asyncio.run:\n%s", out)
	}
}

func TestRenderShimZeroParams(t *testing.T) {
	out := RenderShim(AsyncFn{
		Name:     "ping",
		SyncName: "ping_sync",
		Return:   "None",
	}, PerCall)
	if !strings.Contains(out, "def ping_sync() -> None:") {
		t.Errorf("zero-param signature wrong:\n%s", out)
	}
	if !strings.Contains(out, "asyncio.run(ping())") {
		t.Errorf("zero-param call wrong:\n%s", out)
	}
}

func TestRenderShimPanicsOnInvalid(t *testing.T) {
	defer func() {
		if r := recover(); r == nil {
			t.Fatal("RenderShim must panic on invalid AsyncFn")
		}
	}()
	RenderShim(AsyncFn{}, PerCall)
}

func TestRenderModulePerCall(t *testing.T) {
	m := Module{
		SourceImport: "from mochi_async_pkg import fetch, tick",
		Mode:         PerCall,
		Fns: []AsyncFn{
			{Name: "fetch", SyncName: "fetch_sync", ParamNames: []string{"url"}, ParamTypes: []string{"str"}, Return: "bytes"},
			{Name: "tick", SyncName: "tick_sync", ParamNames: []string{"n"}, ParamTypes: []string{"int"}, Return: "int"},
		},
	}
	src, err := m.Render()
	if err != nil {
		t.Fatalf("Render: %v", err)
	}
	wantSubs := []string{
		"from __future__ import annotations",
		"import asyncio",
		"from mochi_async_pkg import fetch, tick",
		"class MochiAsyncReentryError(RuntimeError)",
		"def _mochi_check_no_running_loop(fn_name)",
		"def fetch_sync(url: str) -> bytes:",
		"def tick_sync(n: int) -> int:",
		"asyncio.run(fetch(url))",
		"asyncio.run(tick(n))",
	}
	for _, w := range wantSubs {
		if !strings.Contains(src, w) {
			t.Errorf("module missing %q\n--module--\n%s", w, src)
		}
	}
	if strings.Contains(src, "_MOCHI_LOOP") {
		t.Errorf("per-call module must not declare _MOCHI_LOOP:\n%s", src)
	}
}

func TestRenderModulePersistent(t *testing.T) {
	m := Module{
		SourceImport: "from mochi_async_pkg import tick",
		Mode:         Persistent,
		Fns: []AsyncFn{
			{Name: "tick", SyncName: "tick_sync", ParamNames: []string{"n"}, ParamTypes: []string{"int"}, Return: "int"},
		},
	}
	src, err := m.Render()
	if err != nil {
		t.Fatalf("Render: %v", err)
	}
	wantSubs := []string{
		"_MOCHI_LOOP = None",
		"def _mochi_get_loop():",
		"_mochi_get_loop().run_until_complete(tick(n))",
	}
	for _, w := range wantSubs {
		if !strings.Contains(src, w) {
			t.Errorf("persistent module missing %q\n%s", w, src)
		}
	}
}

func TestRenderModuleEmpty(t *testing.T) {
	m := Module{Mode: PerCall}
	src, err := m.Render()
	if err != nil {
		t.Fatalf("Render: %v", err)
	}
	if !strings.Contains(src, "import asyncio") {
		t.Errorf("empty module missing asyncio import:\n%s", src)
	}
	if !strings.Contains(src, "MochiAsyncReentryError") {
		t.Errorf("empty module missing cross-loop helper:\n%s", src)
	}
}

func TestRenderModulePropagatesValidateError(t *testing.T) {
	m := Module{
		Mode: PerCall,
		Fns:  []AsyncFn{{Name: "fetch"}},
	}
	if _, err := m.Render(); err == nil {
		t.Fatal("expected Render to fail on invalid AsyncFn")
	}
}

func TestRenderModuleDeterministic(t *testing.T) {
	m := Module{
		SourceImport: "from m import fetch",
		Mode:         PerCall,
		Fns: []AsyncFn{
			{Name: "fetch", SyncName: "fetch_sync", ParamNames: []string{"url"}, ParamTypes: []string{"str"}, Return: "bytes"},
		},
	}
	a, _ := m.Render()
	b, _ := m.Render()
	if a != b {
		t.Fatal("Render should be deterministic")
	}
}

func TestRenderModuleSourceImportTrailingNewline(t *testing.T) {
	withNL := Module{SourceImport: "from m import f\n", Mode: PerCall}
	withoutNL := Module{SourceImport: "from m import f", Mode: PerCall}
	a, _ := withNL.Render()
	b, _ := withoutNL.Render()
	if a != b {
		t.Fatalf("trailing-newline normalisation broken:\nwith=%q\nwithout=%q", a, b)
	}
}
