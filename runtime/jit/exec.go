package jit

/*
#include <stdint.h>

typedef int64_t (*fnptr)();
static int64_t call(fnptr f) { return f(); }
*/
import "C"
import "unsafe"

func exec(code unsafe.Pointer) int64 {
	return int64(C.call((C.fnptr)(code)))
}
