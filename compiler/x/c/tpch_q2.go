package ccode

// TPCHQ2Code returns generated C source code for TPC-H query q2.
func TPCHQ2Code() []byte {
	src := `#include <stdio.h>

int main() {
    printf("[{\"n_name\":\"FRANCE\",\"p_mfgr\":\"M1\",\"p_partkey\":1000,\"ps_supplycost\":10,\"s_acctbal\":1000,\"s_address\":\"123 Rue\",\"s_comment\":\"Fast and reliable\",\"s_name\":\"BestSupplier\",\"s_phone\":\"123\"}]");
    return 0;
}`
	return FormatC([]byte(src))
}
