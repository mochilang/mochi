//go:build cosmo

package cosmo

import (
	"io/ioutil"
	"os"
	"os/exec"
)

// CompileAndRun compiles the provided C snippet with cosmocc and executes the resulting binary.
// The program's stdout is returned.
func CompileAndRun(code string) (string, error) {
	src, err := ioutil.TempFile("", "cosmo-*.c")
	if err != nil {
		return "", err
	}
	defer os.Remove(src.Name())
	if _, err := src.WriteString(code); err != nil {
		src.Close()
		return "", err
	}
	src.Close()
	out := src.Name() + ".bin"
	cmd := exec.Command("cosmocc", "-static", "-o", out, src.Name())
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return "", err
	}
	defer os.Remove(out)
	runCmd := exec.Command(out)
	outBytes, err := runCmd.CombinedOutput()
	return string(outBytes), err
}

// CompileToFile compiles the C source code and writes the resulting binary to the given output path.
func CompileToFile(code, output string) error {
	tmp, err := ioutil.TempFile("", "cosmo-*.c")
	if err != nil {
		return err
	}
	defer os.Remove(tmp.Name())
	if _, err := tmp.WriteString(code); err != nil {
		tmp.Close()
		return err
	}
	tmp.Close()
	cmd := exec.Command("cosmocc", "-static", "-o", output, tmp.Name())
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}
