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

func TestErlangSolutions(t *testing.T) {
    if _, err := exec.LookPath("escript"); err != nil {
        t.Skip("escript not installed")
    }
    golden.Run(t, "tests/spoj/human/x/erlang", ".erl", ".out", func(src string) ([]byte, error) {
        inPath := strings.TrimSuffix(src, ".erl") + ".in"
        cmd := exec.Command("escript", src)
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
