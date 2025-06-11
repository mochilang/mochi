package main

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"

	goexec "mochi/runtime/go"
)

func TestWasmInterpreter(t *testing.T) {
	tmp := t.TempDir()

	// build wasm
	wasmPath := filepath.Join(tmp, "mochi.wasm")
	cmd := goexec.Command("build", "-o", wasmPath, ".")
	cmd.Env = append(os.Environ(), "GOOS=js", "GOARCH=wasm")
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("go build failed: %v\n%s", err, out)
	}

	// copy wasm_exec.js from Go installation
	src := filepath.Join(runtime.GOROOT(), "lib", "wasm", "wasm_exec.js")
	data, err := os.ReadFile(src)
	if err != nil {
		t.Fatalf("read wasm_exec.js: %v", err)
	}
	wasmExecPath := filepath.Join(tmp, "wasm_exec.js")
	if err := os.WriteFile(wasmExecPath, data, 0644); err != nil {
		t.Fatalf("write wasm_exec.js: %v", err)
	}

	// create node runner
	runJS := filepath.Join(tmp, "run.js")
	script := `const fs = require('fs');
require('./wasm_exec.js');
const go = new globalThis.Go();
WebAssembly.instantiate(fs.readFileSync('mochi.wasm'), go.importObject).then((result) => {
  go.run(result.instance);
  const out = globalThis.runMochi('print("hello world")');
  console.log(out);
  process.exit(0);
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

	if strings.TrimSpace(buf.String()) != "hello world" {
		t.Fatalf("unexpected output: %q", buf.String())
	}
}
