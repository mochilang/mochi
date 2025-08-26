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

func TestClojureSolutions(t *testing.T) {
        if _, err := exec.LookPath("bb"); err != nil {
                t.Skip("bb toolchain not installed")
        }
        golden.Run(t, "tests/spoj/human/x/clojure", ".clj", ".out", func(src string) ([]byte, error) {
                inPath := strings.TrimSuffix(src, ".clj") + ".in"
                cmd := exec.Command("bb", src)
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

