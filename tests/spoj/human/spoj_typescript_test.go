//go:build slow

package human

import (
    "bytes"
    "fmt"
    "os"
    "os/exec"
    "strings"
    "testing"

    "mochi/golden"
)

func TestTypeScriptSolutions(t *testing.T) {
    if _, err := exec.LookPath("node"); err != nil {
        t.Skip("node not installed")
    }
    golden.Run(t, "tests/spoj/human/x/typescript", ".ts", ".out", func(src string) ([]byte, error) {
        inPath := strings.TrimSuffix(src, ".ts") + ".in"
        cmd := exec.Command("npx", "--yes", "ts-node", src)
        if data, err := os.ReadFile(inPath); err == nil {
            cmd.Stdin = bytes.NewReader(data)
        }
        var stderr bytes.Buffer
        cmd.Stderr = &stderr
        out, err := cmd.Output()
        if err != nil {
            return nil, fmt.Errorf("%w: %s", err, stderr.String())
        }
        return bytes.TrimSpace(out), nil
    })
}
