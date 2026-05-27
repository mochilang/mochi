package emit

import (
	"fmt"
	"os/exec"
	"strings"
)

// Compile invokes javac to compile javaFiles to classDir.
// flags: e.g. []string{"--release", "21", "-Xlint:all", "-Werror"}
func Compile(javacPath string, javaFiles []string, classDir string, flags []string) error {
	args := append([]string{"-d", classDir}, flags...)
	args = append(args, javaFiles...)

	out, err := exec.Command(javacPath, args...).CombinedOutput()
	if err != nil {
		return fmt.Errorf("javac: %w\n%s", err, strings.TrimSpace(string(out)))
	}
	return nil
}
