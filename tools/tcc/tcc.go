//go:build tcc && libtcc

package tcc

/*
#cgo LDFLAGS: -ltcc -lm -ldl
#include <stdlib.h>
#include <libtcc.h>

static int compile_and_run(const char* code) {
    TCCState *s = tcc_new();
    if (!s) return -1;
    tcc_set_output_type(s, TCC_OUTPUT_MEMORY);
    if (tcc_compile_string(s, code) == -1) return -1;
    if (tcc_relocate(s, TCC_RELOCATE_AUTO) < 0) return -1;
    int (*fn)(int) = tcc_get_symbol(s, "square");
    int res = fn(5);
    tcc_delete(s);
    return res;
}
*/
import "C"
import (
	"errors"
	"unsafe"
)

// CompileAndRun compiles the provided C snippet using TinyCC and executes
// the function `square` with argument 5. It returns the resulting value or
// an error if compilation fails.
func CompileAndRun(code string) (int, error) {
	csrc := C.CString(code)
	defer C.free(unsafe.Pointer(csrc))
	n := C.compile_and_run(csrc)
	if n < 0 {
		return 0, errors.New("compile or run error")
	}
	return int(n), nil
}
