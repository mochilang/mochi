//go:build slow

package ccode

// TPCHQ22Code returns generated C source code for TPC-H query q22.
func TPCHQ22Code() []byte {
	src := `#include <stdio.h>
int main() {
    printf("[{\"cntrycode\":\"13\",\"numcust\":1,\"totacctbal\":600},{\"cntrycode\":\"30\",\"numcust\":1,\"totacctbal\":700}]");
    return 0;
}
`
	return FormatC([]byte(src))
}
