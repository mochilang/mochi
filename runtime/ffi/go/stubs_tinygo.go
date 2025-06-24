//go:build tinygo

package goffi

import (
	"fmt"
	ffiinfo "mochi/runtime/ffi/infer"
)

// AttrAuto is not supported when compiled with TinyGo.
func AttrAuto(pkg, name string, args ...any) (any, error) {
	return nil, fmt.Errorf("goffi: AttrAuto not supported on tinygo")
}

// CallAuto is not supported on TinyGo.
func CallAuto(name string, args ...any) (any, error) {
	return nil, fmt.Errorf("goffi: CallAuto not supported on tinygo")
}

// Export is not available on TinyGo builds.
func Export(path, dir string) error {
	return fmt.Errorf("goffi: Export not supported on tinygo")
}

// ExportAll is not available on TinyGo builds.
func ExportAll(dir string) error {
	return fmt.Errorf("goffi: ExportAll not supported on tinygo")
}

// PackageInfo describes a Go package.
type PackageInfo struct {
	Path string
	Doc  string
}

// Packages is not supported on TinyGo builds.
func Packages() ([]PackageInfo, error) {
	return nil, fmt.Errorf("goffi: Packages not supported on tinygo")
}

// Infer is unavailable on TinyGo builds.
func Infer(path string) (*ffiinfo.ModuleInfo, error) {
	return nil, fmt.Errorf("goffi: Infer not supported on tinygo")
}
