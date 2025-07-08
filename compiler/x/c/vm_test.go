//go:build slow

package ccode_test

import (
    "bytes"
    "fmt"
    "os"
    "os/exec"
    "path/filepath"
    "strings"
    "testing"

    ccode "mochi/compiler/x/c"
    "mochi/parser"
    "mochi/types"
)

func TestCompilePrograms(t *testing.T) {
    cc, err := ccode.EnsureCC()
    if err != nil {
        t.Skipf("C compiler not installed: %v", err)
    }
    root := findRepoRoot(t)
    pattern := filepath.Join(root, "tests", "vm", "valid", "*.mochi")
    files, err := filepath.Glob(pattern)
    if err != nil {
        t.Fatalf("glob error: %v", err)
    }
    outDir := filepath.Join(root, "tests", "machine", "x", "c")
    os.MkdirAll(outDir, 0o755)
    for _, src := range files {
        name := strings.TrimSuffix(filepath.Base(src), ".mochi")
        t.Run(name, func(t *testing.T) { compileAndRun(t, cc, src, outDir, name) })
    }
}

func compileAndRun(t *testing.T, cc string, src, outDir, name string) {
    data, err := os.ReadFile(src)
    if err != nil {
        t.Fatalf("read error: %v", err)
    }
    errPath := filepath.Join(outDir, name+".error")
    os.Remove(errPath)
    prog, err := parser.ParseString(string(data))
    if err != nil {
        writeError(t, outDir, name, string(data), err)
        return
    }
    env := types.NewEnv(nil)
    if errs := types.Check(prog, env); len(errs) > 0 {
        writeError(t, outDir, name, string(data), fmt.Errorf("type error: %v", errs[0]))
        return
    }
    code, err := ccode.New(env).Compile(prog)
    if err != nil {
        writeError(t, outDir, name, string(data), err)
        return
    }
    codePath := filepath.Join(outDir, name+".c")
    os.WriteFile(codePath, code, 0o644)

    binPath := filepath.Join(outDir, name)
    cmd := exec.Command(cc, codePath, "-o", binPath)
    var buf bytes.Buffer
    cmd.Stdout = &buf
    cmd.Stderr = &buf
    if err := cmd.Run(); err != nil {
        writeError(t, outDir, name, string(data), fmt.Errorf("cc: %v\n%s", err, buf.String()))
        return
    }
    defer os.Remove(binPath)

    cmd = exec.Command(binPath)
    buf.Reset()
    cmd.Stdout = &buf
    cmd.Stderr = &buf
    if err := cmd.Run(); err != nil {
        writeError(t, outDir, name, string(data), fmt.Errorf("run: %v\n%s", err, buf.String()))
        return
    }
    outPath := filepath.Join(outDir, name+".out")
    os.WriteFile(outPath, buf.Bytes(), 0o644)
    os.Remove(errPath)
}

func writeError(t *testing.T, dir, name, src string, err error) {
    lines := strings.Split(src, "\n")
    msg := err.Error()
    ln := 0
    if idx := strings.Index(msg, "line "); idx != -1 {
        fmt.Sscanf(msg[idx:], "line %d", &ln)
    }
    var ctx string
    if ln > 0 {
        start := ln - 2
        if start < 0 { start = 0 }
        end := ln + 1
        if end > len(lines) { end = len(lines) }
        for i := start; i < end; i++ { ctx += lines[i] + "\n" }
    }
    errPath := filepath.Join(dir, name+".error")
    os.WriteFile(errPath, []byte(msg+"\n"+ctx), 0o644)
}

func findRepoRoot(t *testing.T) string {
    dir, err := os.Getwd()
    if err != nil { t.Fatal("cannot determine working directory") }
    for i := 0; i < 10; i++ {
        if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil { return dir }
        parent := filepath.Dir(dir)
        if parent == dir { break }
        dir = parent
    }
    t.Fatal("go.mod not found (not in Go module)")
    return ""
}
