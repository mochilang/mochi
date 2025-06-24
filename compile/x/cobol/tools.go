package cobolcode

import "fmt"

// EnsureCOBOL ensures the cobc compiler is installed.
func EnsureCOBOL() error {
	return fmt.Errorf("cobc not installed")
}
