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

func TestHaskellSolutions(t *testing.T) {
    if _, err := exec.LookPath("runghc"); err != nil {
        t.Skip("runghc not installed")
    }
    golden.Run(t, "tests/spoj/human/x/haskell", ".hs", ".out", func(src string) ([]byte, error) {
        inPath := strings.TrimSuffix(src, ".hs") + ".in"
        cmd := exec.Command("runghc", src)
        if data, err := os.ReadFile(inPath); err == nil {
            cmd.Stdin = bytes.NewReader(data)
        }
        out, err := cmd.CombinedOutput()
        if err != nil {
            return nil, err
        }
        return bytes.TrimSpace(out), nil
    })
}

