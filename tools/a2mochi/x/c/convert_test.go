package c_test

import (
	"os/exec"
	"testing"

	c "mochi/tools/a2mochi/x/c"
)

func TestConvertSimple(t *testing.T) {
	if _, err := exec.LookPath("clang"); err != nil {
		t.Skip("clang not installed")
	}
	src := "int main(){printf(\"hi\n\");}"
	code, err := c.ConvertSource(src)
	if err != nil {
		t.Fatalf("convert: %v", err)
	}
	if code == "" {
		t.Fatalf("empty output")
	}
}
