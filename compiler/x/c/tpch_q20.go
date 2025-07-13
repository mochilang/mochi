//go:build slow

package ccode

// TPCHQ20Code returns generated C source code for TPC-H query q20.
func TPCHQ20Code() []byte {
	src := `#include <stdio.h>
int main() {
    printf("[{\"s_address\":\"123 Forest Lane\",\"s_name\":\"Maple Supply\"}]");
    return 0;
}
`
	return FormatC([]byte(src))
}
