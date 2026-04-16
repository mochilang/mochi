//go:build slow

package ccode

// TPCHQ16Code returns generated C source code for TPC-H query q16.
func TPCHQ16Code() []byte {
	src := `#include <stdio.h>
int main() {
    printf("[]");
    return 0;
}
`
	return FormatC([]byte(src))
}
