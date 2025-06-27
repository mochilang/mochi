//go:build cosmo

package cosmo

import "testing"

func TestCompileAndRun(t *testing.T) {
	if err := EnsureCosmo(); err != nil {
		t.Skipf("cosmocc not available: %v", err)
	}
	out, err := CompileAndRun(`#include <stdio.h>
int main(){printf("%d",25);}`)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	if out != "25" {
		t.Fatalf("unexpected output: %s", out)
	}
}
