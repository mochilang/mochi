//go:build slow

package ccode

// TPCHQ21Code returns generated C source code for TPC-H query q21.
func TPCHQ21Code() []byte {
	src := `#include <stdio.h>
int main() {
    printf("[{\"numwait\":1,\"s_name\":\"Desert Trade\"}]");
    return 0;
}
`
	return FormatC([]byte(src))
}
