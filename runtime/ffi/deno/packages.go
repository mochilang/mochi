package deno

import (
	"path/filepath"
	"runtime"

	ffiinfo "mochi/runtime/ffi/infer"
)

var pkgDir string

func init() {
	_, file, _, _ := runtime.Caller(0)
	pkgDir = filepath.Dir(file)
}

// Packages scans the Deno FFI directory for .ts modules and returns
// ModuleInfo details for each.
func Packages() (map[string]*ffiinfo.ModuleInfo, error) {
	files, err := filepath.Glob(filepath.Join(pkgDir, "*.ts"))
	if err != nil {
		return nil, err
	}
	out := make(map[string]*ffiinfo.ModuleInfo, len(files))
	for _, f := range files {
		info, err := Infer(f)
		if err != nil {
			return nil, err
		}
		out[filepath.Base(f)] = info
	}
	return out, nil
}
