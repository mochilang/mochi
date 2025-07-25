package vm_test

import (
	"os"
	"path/filepath"
	"strings"
)

func writeErr(src string, err error) {
	base := strings.TrimSuffix(src, filepath.Ext(src))
	_ = os.WriteFile(base+".error", []byte(err.Error()), 0644)
}

func removeErr(src string) {
	base := strings.TrimSuffix(src, filepath.Ext(src))
	os.Remove(base + ".error")
	os.Remove(base + ".mochi.error")
}
