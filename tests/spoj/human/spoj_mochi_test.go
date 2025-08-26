//go:build slow

package human

import (
    "bytes"
    "os"
    "os/exec"
    "strings"
    "testing"

    "mochi/golden"
)

func TestMochiSolutions(t *testing.T) {
    if _, err := exec.LookPath("mochi"); err != nil {
        t.Skip("mochi not installed")
    }
    golden.Run(t, "tests/spoj/human/x/mochi", ".mochi", ".out", func(src string) ([]byte, error) {
        inPath := strings.TrimSuffix(src, ".mochi") + ".in"
        cmd := exec.Command("mochi", "run", src)
        if data, err := os.ReadFile(inPath); err == nil {
            cmd.Stdin = bytes.NewReader(data)
        }
        out, err := cmd.CombinedOutput()
        if err != nil {
            return out, err
        }
        return bytes.TrimSpace(out), nil
    })
}
