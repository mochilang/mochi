package subproc

import (
	"strings"
	"testing"
)

func TestWorkerOptionsValidate(t *testing.T) {
	cases := []struct {
		name    string
		opts    WorkerOptions
		wantErr bool
	}{
		{"ok", WorkerOptions{ModuleImport: "from m import f", Methods: []string{"f"}}, false},
		{"empty import", WorkerOptions{Methods: []string{"f"}}, true},
		{"empty methods", WorkerOptions{ModuleImport: "from m import f"}, true},
		{"bad identifier", WorkerOptions{ModuleImport: "from m import f", Methods: []string{"1bad"}}, true},
		{"hyphen in identifier", WorkerOptions{ModuleImport: "from m import f", Methods: []string{"bad-name"}}, true},
	}
	for _, c := range cases {
		err := c.opts.Validate()
		if c.wantErr && err == nil {
			t.Errorf("%s: expected error", c.name)
		}
		if !c.wantErr && err != nil {
			t.Errorf("%s: unexpected error: %v", c.name, err)
		}
	}
}

func TestRenderWorkerSync(t *testing.T) {
	src, err := RenderWorker(WorkerOptions{
		ModuleImport: "from mochi_pkg import fetch, compute",
		Methods:      []string{"fetch", "compute"},
	})
	if err != nil {
		t.Fatalf("RenderWorker: %v", err)
	}
	wants := []string{
		"import json",
		"import sys",
		"from mochi_pkg import fetch, compute",
		`"fetch": fetch,`,
		`"compute": compute,`,
		"_PARSE_ERROR = -32700",
		"_METHOD_NOT_FOUND = -32601",
		"def _dispatch(req):",
		"result = _METHODS[method](*params)",
		"if __name__ == \"__main__\":",
		"    _main()",
	}
	for _, w := range wants {
		if !strings.Contains(src, w) {
			t.Errorf("worker missing %q\n%s", w, src)
		}
	}
	if strings.Contains(src, "asyncio") {
		t.Errorf("sync worker must not import asyncio:\n%s", src)
	}
}

func TestRenderWorkerAsync(t *testing.T) {
	src, err := RenderWorker(WorkerOptions{
		ModuleImport: "from mochi_pkg import fetch",
		Methods:      []string{"fetch"},
		Async:        true,
	})
	if err != nil {
		t.Fatalf("RenderWorker: %v", err)
	}
	wants := []string{
		"import asyncio",
		"async def _dispatch(req):",
		"result = await _METHODS[method](*params)",
		"async def _main():",
		"asyncio.run(_main())",
	}
	for _, w := range wants {
		if !strings.Contains(src, w) {
			t.Errorf("async worker missing %q\n%s", w, src)
		}
	}
}

func TestRenderWorkerPropagatesValidateError(t *testing.T) {
	if _, err := RenderWorker(WorkerOptions{}); err == nil {
		t.Fatal("expected validate error")
	}
}

func TestRenderWorkerNormalisesImportNewline(t *testing.T) {
	withNL, _ := RenderWorker(WorkerOptions{ModuleImport: "from m import f\n", Methods: []string{"f"}})
	withoutNL, _ := RenderWorker(WorkerOptions{ModuleImport: "from m import f", Methods: []string{"f"}})
	if withNL != withoutNL {
		t.Fatalf("trailing newline not normalised")
	}
}
