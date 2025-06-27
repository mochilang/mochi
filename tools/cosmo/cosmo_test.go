//go:build cosmo

package cosmo

import "testing"

func TestCompileAndRun(t *testing.T) {
	out, err := CompileAndRun(`#include <stdio.h>
int main(){printf("%d", 5*5);return 0;}`)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	if out != "25" {
		t.Fatalf("unexpected output: %s", out)
	}
}
