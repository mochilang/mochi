//go:build slow

package main

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

func TestPlaygroundWasm(t *testing.T) {
	if _, err := exec.LookPath("tinygo"); err != nil {
		t.Skip("tinygo not installed")
	}
	if _, err := exec.LookPath("node"); err != nil {
		t.Skip("node not installed")
	}

	tmp := t.TempDir()

	wasmPath := filepath.Join(tmp, "mochi.wasm")
	buildCmd := exec.Command("tinygo", "build", "-o", wasmPath, "-target", "wasm", "./")
	buildCmd.Dir = filepath.Join("tools", "playground")
	if out, err := buildCmd.CombinedOutput(); err != nil {
		t.Fatalf("tinygo build failed: %v\n%s", err, out)
	}

	src := filepath.Join(runtime.GOROOT(), "lib", "wasm", "wasm_exec.js")
	data, err := os.ReadFile(src)
	if err != nil {
		t.Fatalf("read wasm_exec.js: %v", err)
	}
	wasmExec := filepath.Join(tmp, "wasm_exec.js")
	if err := os.WriteFile(wasmExec, data, 0644); err != nil {
		t.Fatalf("write wasm_exec.js: %v", err)
	}

	runJS := filepath.Join(tmp, "run.js")
	script := `const fs = require('fs');
require('./wasm_exec.js');
const go = new globalThis.Go();
WebAssembly.instantiate(fs.readFileSync('mochi.wasm'), go.importObject).then((res) => {
  go.run(res.instance);
  const out = globalThis.runMochi('print("hello playground")');
  console.log(out);
}).catch(err => { console.error(err); process.exit(1); });`
	if err := os.WriteFile(runJS, []byte(script), 0644); err != nil {
		t.Fatalf("write run.js: %v", err)
	}

	nodeCmd := exec.Command("node", "run.js")
	nodeCmd.Dir = tmp
	var buf bytes.Buffer
	nodeCmd.Stdout = &buf
	nodeCmd.Stderr = &buf
	if err := nodeCmd.Run(); err != nil {
		t.Fatalf("node run failed: %v\n%s", err, buf.String())
	}

	got := strings.TrimSpace(buf.String())
	if got != "hello playground" {
		t.Fatalf("unexpected output: %q", got)
	}
}
