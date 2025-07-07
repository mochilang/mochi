//go:build archived && slow

package wasm_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"

	wasm "mochi/archived/x/wasm"
	"mochi/parser"
	"mochi/types"
)

// runWasm compiles prog using the given toolchain and executes the resulting
// WebAssembly module with node and wasm_exec.js. It returns the stdout output.
func runWasm(t *testing.T, tc wasm.Toolchain) string {
	t.Helper()

	prog, err := parser.ParseString("print(\"hello wasm\")")
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}

	// compile
	compiler := wasm.New(env, wasm.WithToolchain(tc))
	wasmBytes, err := compiler.Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	tmp := t.TempDir()
	wasmPath := filepath.Join(tmp, "prog.wasm")
	if err := os.WriteFile(wasmPath, wasmBytes, 0644); err != nil {
		t.Fatalf("write wasm: %v", err)
	}

	src := filepath.Join(runtime.GOROOT(), "lib", "wasm", "wasm_exec.js")
	data, err := os.ReadFile(src)
	if err != nil {
		t.Fatalf("read wasm_exec.js: %v", err)
	}
	if err := os.WriteFile(filepath.Join(tmp, "wasm_exec.js"), data, 0644); err != nil {
		t.Fatalf("write wasm_exec.js: %v", err)
	}

	runJS := filepath.Join(tmp, "run.js")
	script := `const fs = require('fs');
require('./wasm_exec.js');
const go = new globalThis.Go();
WebAssembly.instantiate(fs.readFileSync('prog.wasm'), go.importObject).then((res) => {
  go.run(res.instance);
}).catch(err => { console.error(err); process.exit(1); });`
	if err := os.WriteFile(runJS, []byte(script), 0644); err != nil {
		t.Fatalf("write run.js: %v", err)
	}

	cmd := exec.Command("node", "run.js")
	cmd.Dir = tmp
	var buf bytes.Buffer
	cmd.Stdout = &buf
	cmd.Stderr = &buf
	if err := cmd.Run(); err != nil {
		t.Fatalf("node run failed: %v\n%s", err, buf.String())
	}

	return strings.TrimSpace(buf.String())
}

func TestWasmCompiler(t *testing.T) {
	if _, err := exec.LookPath("node"); err != nil {
		t.Skip("node not installed")
	}

	// Always test the Go toolchain
	if out := runWasm(t, wasm.ToolchainGo); out != "hello wasm" {
		t.Fatalf("unexpected output: %q", out)
	}

	// Test TinyGo toolchain if tinygo is available
	if _, err := exec.LookPath("tinygo"); err == nil {
		if out := runWasm(t, wasm.ToolchainTinyGo); out != "hello wasm" {
			t.Fatalf("unexpected output (tinygo): %q", out)
		}
	}
}
