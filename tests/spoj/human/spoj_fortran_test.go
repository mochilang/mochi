//go:build slow

package human

import (
    "bytes"
    "os"
    "os/exec"
    "path/filepath"
    "strings"
    "testing"

    "mochi/golden"
)

func TestFortranSolutions(t *testing.T) {
    if _, err := exec.LookPath("gfortran"); err != nil {
        t.Skip("gfortran not installed")
    }
    golden.Run(t, "tests/spoj/human/x/fortran", ".f90", ".out", func(src string) ([]byte, error) {
        inPath := strings.TrimSuffix(src, ".f90") + ".in"
        tmpDir, err := os.MkdirTemp("", "spojfortran")
        if err != nil {
            return nil, err
        }
        bin := filepath.Join(tmpDir, "a.out")
        if out, err := exec.Command("gfortran", src, "-O2", "-o", bin).CombinedOutput(); err != nil {
            return nil, err
        } else {
            _ = out
        }
        cmd := exec.Command(bin)
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
