package build

import (
	"runtime"
	"testing"
)

func TestPhase2NanInf(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships host-cc discovery only on POSIX; Windows lands in Phase 11")
	}
	runFixtureSuite(t, "nan-inf")
}
