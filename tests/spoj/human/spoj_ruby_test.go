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

func TestRubySolutions(t *testing.T) {
    if _, err := exec.LookPath("ruby"); err != nil {
        t.Skip("ruby not installed")
    }
    golden.Run(t, "tests/spoj/human/x/ruby", ".rb", ".out", func(src string) ([]byte, error) {
        inPath := strings.TrimSuffix(src, ".rb") + ".in"
        cmd := exec.Command("ruby", src)
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
