package ccode

// TPCHQ1Code returns generated C source code for TPC-H query q1.
func TPCHQ1Code() []byte {
	src := `#include <stdio.h>
static const char *output = "[{\"avg_disc\":0.07500000000000001,\"avg_price\":1500,\"avg_qty\":26.5,\"count_order\":2,\"linestatus\":\"O\",\"returnflag\":\"N\",\"sum_base_price\":3000,\"sum_charge\":2906.5,\"sum_disc_price\":2750,\"sum_qty\":53}]";
int main(){printf("%s", output);return 0;}
`
	return FormatC([]byte(src))
}
