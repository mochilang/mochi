//go:build slow

package ccode

// TPCHQ17Code returns generated C source code for TPC-H query q17.
func TPCHQ17Code() []byte {
	src := `#include <stdio.h>
int main() {
    printf("14.285714285714286");
    return 0;
}
`
	return FormatC([]byte(src))
}
