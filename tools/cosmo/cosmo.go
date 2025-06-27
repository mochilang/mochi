//go:build cosmo && libcosmo

package cosmo

/*
#cgo LDFLAGS: -lcosmo
#include <stdlib.h>

// These helpers are provided by the cosmocc static library.
int cosmo_compile_and_run(const char* src, char** out);
int cosmo_compile_to_file(const char* src, const char* out);
*/
import "C"

import (
	"errors"
	"unsafe"
)

// CompileAndRun compiles the given C code using the embedded Cosmopolitan
// compiler and returns its standard output.
func CompileAndRun(code string) (string, error) {
	csrc := C.CString(code)
	defer C.free(unsafe.Pointer(csrc))
	var out *C.char
	if C.cosmo_compile_and_run(csrc, &out) != 0 {
		return "", errors.New("compile error")
	}
	defer C.free(unsafe.Pointer(out))
	return C.GoString(out), nil
}

// CompileToFile compiles the source code to the given output path using the
// embedded Cosmopolitan compiler.
func CompileToFile(code, out string) error {
	csrc := C.CString(code)
	defer C.free(unsafe.Pointer(csrc))
	cout := C.CString(out)
	defer C.free(unsafe.Pointer(cout))
	if C.cosmo_compile_to_file(csrc, cout) != 0 {
		return errors.New("compile error")
	}
	return nil
}
