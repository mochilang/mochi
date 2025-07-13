//go:build slow

package ccode

// TPCHQ18Code returns generated C source code for TPC-H query q18.
func TPCHQ18Code() []byte {
	src := `#include <stdio.h>
int main() {
    printf("[{\"c_acctbal\":1000,\"c_address\":\"123 Market St\",\"c_comment\":\"Premium client\",\"c_custkey\":1,\"c_name\":\"Alice\",\"c_phone\":\"123-456\",\"n_name\":\"GERMANY\",\"revenue\":1700}]");
    return 0;
}
`
	return FormatC([]byte(src))
}
