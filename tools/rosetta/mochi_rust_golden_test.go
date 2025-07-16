//go:build slow

package rosetta

import (
    "bytes"
    "fmt"
    "os"
    "os/exec"
    "path/filepath"
    "strings"
    "testing"

    rustcode "mochi/compiler/x/rust"
    "mochi/parser"
    "mochi/types"
)

func TestMochiRustGolden(t *testing.T) {
    if _, err := exec.LookPath("rustc"); err != nil {
        t.Skip("rustc not installed")
    }

    root := findRepoRoot(t)
    srcDir := filepath.Join(root, "tests/rosetta/x/Mochi")
    outDir := filepath.Join(root, "tests/rosetta/out/Rust")

    if err := os.MkdirAll(outDir, 0o755); err != nil {
        t.Fatalf("mkout: %v", err)
    }

    outs, err := filepath.Glob(filepath.Join(srcDir, "*.out"))
    if err != nil {
        t.Fatalf("glob: %v", err)
    }
    if len(outs) == 0 {
        t.Fatal("no Mochi Rosetta tests found")
    }

    for _, outPath := range outs {
        name := strings.TrimSuffix(filepath.Base(outPath), ".out")
        srcPath := filepath.Join(srcDir, name+".mochi")
        rsPath := filepath.Join(outDir, name+".rs")
        if _, err := os.Stat(srcPath); err != nil {
            t.Fatalf("missing source for %s", name)
        }

        t.Run(name, func(t *testing.T) {
            prog, err := parser.Parse(srcPath)
            if err != nil {
                writeRustError(outDir, name, fmt.Errorf("parse error: %w", err))
                t.Skip("parse error")
                return
            }
            env := types.NewEnv(nil)
            if errs := types.Check(prog, env); len(errs) > 0 {
                writeRustError(outDir, name, fmt.Errorf("type error: %v", errs[0]))
                t.Skip("type error")
                return
            }
            code, err := rustcode.New(env).Compile(prog)
            if err != nil {
                writeRustError(outDir, name, fmt.Errorf("compile error: %w", err))
                t.Skip("compile error")
                return
            }
            gotCode := bytes.TrimSpace(code)

            if shouldUpdate() {
                if err := os.WriteFile(rsPath, append(gotCode, '\n'), 0o644); err != nil {
                    t.Fatalf("write rs: %v", err)
                }
                t.Logf("updated: %s", rsPath)
            } else {
                wantData, err := os.ReadFile(rsPath)
                if err != nil {
                    t.Fatalf("read rs golden: %v", err)
                }
                wantCode := bytes.TrimSpace(wantData)
                if !bytes.Equal(gotCode, wantCode) {
                    t.Errorf("%s Rust\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name, gotCode, wantCode)
                }
            }

            tmp := t.TempDir()
            file := filepath.Join(tmp, "prog.rs")
            if err := os.WriteFile(file, code, 0o644); err != nil {
                t.Fatalf("write temp rs: %v", err)
            }
            exe := filepath.Join(tmp, "prog")
            if out, err := exec.Command("rustc", file, "-O", "-o", exe).CombinedOutput(); err != nil {
                writeRustError(outDir, name, fmt.Errorf("rustc error: %v\n%s", err, out))
                t.Skip("rustc error")
                return
            }
            outBytes, err := exec.Command(exe).CombinedOutput()
            if err != nil {
                writeRustError(outDir, name, fmt.Errorf("run error: %v\n%s", err, outBytes))
                t.Skip("run error")
                return
            }
            gotOut := bytes.TrimSpace(outBytes)
            if shouldUpdate() {
                if err := os.WriteFile(outPath, append(gotOut, '\n'), 0o644); err != nil {
                    t.Fatalf("write out: %v", err)
                }
                t.Logf("updated: %s", outPath)
            } else {
                want, err := os.ReadFile(outPath)
                if err != nil {
                    t.Fatalf("read golden: %v", err)
                }
                want = bytes.TrimSpace(want)
                if !bytes.Equal(gotOut, want) {
                    t.Errorf("%s output\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name, gotOut, want)
                }
            }
            _ = os.Remove(filepath.Join(outDir, name+".error"))
        })
    }
}

func writeRustError(dir, name string, err error) {
    _ = os.WriteFile(filepath.Join(dir, name+".error"), []byte(err.Error()), 0o644)
}
