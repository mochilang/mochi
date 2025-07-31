//go:build slow

package kotlin_test

import (
    "encoding/json"
    "flag"
    "os"
    "os/exec"
    "path/filepath"
    "sort"
    "strings"
    "testing"

    kotlin "mochi/aster/x/kotlin"
)

func shouldUpdate() bool {
    f := flag.Lookup("update")
    return f != nil && f.Value.String() == "true"
}

func ensureKotlinc(t *testing.T) {
    if _, err := exec.LookPath("kotlinc"); err != nil {
        t.Skip("kotlinc not installed")
    }
}

func TestPrint_Golden(t *testing.T) {
    ensureKotlinc(t)
    root := repoRoot(t)
    srcDir := filepath.Join(root, "tests", "transpiler", "x", "kt")
    outDir := filepath.Join(root, "tests", "aster", "x", "kotlin")
    os.MkdirAll(outDir, 0o755)

    files, err := filepath.Glob(filepath.Join(srcDir, "*.kt"))
    if err != nil {
        t.Fatal(err)
    }
    sort.Strings(files)
    var selected []string
    for _, f := range files {
        if filepath.Base(f) == "two-sum.kt" {
            selected = append(selected, f)
        }
    }
    files = selected

    for _, src := range files {
        name := strings.TrimSuffix(filepath.Base(src), ".kt")
        t.Run(name, func(t *testing.T) {
            data, err := os.ReadFile(src)
            if err != nil {
                t.Fatalf("read src: %v", err)
            }
            prog, err := kotlin.Inspect(string(data))
            if err != nil {
                t.Fatalf("inspect: %v", err)
            }
            astJSON, err := json.MarshalIndent(prog, "", "  ")
            if err != nil {
                t.Fatalf("marshal: %v", err)
            }
            astJSON = append(astJSON, '\n')
            jsonPath := filepath.Join(outDir, name+".kt.json")
            if shouldUpdate() {
                if err := os.WriteFile(jsonPath, astJSON, 0644); err != nil {
                    t.Fatalf("write json: %v", err)
                }
            }
            wantJSON, err := os.ReadFile(jsonPath)
            if err != nil {
                t.Skip("missing golden")
                return
            }
            if string(astJSON) != string(wantJSON) {
                t.Fatalf("json mismatch\n--- got ---\n%s\n--- want ---\n%s", astJSON, wantJSON)
            }
            out, err := kotlin.Print(prog)
            if err != nil {
                t.Fatalf("print: %v", err)
            }
            outPath := filepath.Join(outDir, name+".kt")
            if shouldUpdate() {
                if err := os.WriteFile(outPath, []byte(out), 0644); err != nil {
                    t.Fatalf("write out: %v", err)
                }
            }
            jar := filepath.Join(outDir, name+".jar")
            if outb, err := exec.Command("kotlinc", outPath, "-include-runtime", "-d", jar).CombinedOutput(); err != nil {
                t.Fatalf("kotlinc error: %v\n%s", err, outb)
            }
            got, err := exec.Command("java", "-jar", jar).CombinedOutput()
            if err != nil {
                t.Fatalf("run printed: %v\n%s", err, got)
            }
            wantJar := filepath.Join(outDir, name+"_ref.jar")
            if outb, err := exec.Command("kotlinc", src, "-include-runtime", "-d", wantJar).CombinedOutput(); err != nil {
                t.Fatalf("kotlinc ref error: %v\n%s", err, outb)
            }
            want, err := exec.Command("java", "-jar", wantJar).CombinedOutput()
            if err != nil {
                t.Fatalf("run original: %v\n%s", err, want)
            }
            outFile := filepath.Join(outDir, name+".out")
            if shouldUpdate() {
                if err := os.WriteFile(outFile, got, 0644); err != nil {
                    t.Fatalf("write out file: %v", err)
                }
            }
            if string(got) != string(want) {
                t.Fatalf("output mismatch\n--- got ---\n%s\n--- want ---\n%s", got, want)
            }
        })
    }
}

