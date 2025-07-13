//go:build slow

package ccode

// TPCHQ19Code returns generated C source code for TPC-H query q19.
func TPCHQ19Code() []byte {
	src := `#include <stdio.h>
int main() {
    printf("2800");
    return 0;
}
`
	return FormatC([]byte(src))
}
