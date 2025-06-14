package datalog

import goffi "mochi/runtime/ffi/go"

func ffiRegister() {
	goffi.Register("datalog.fact", Fact)
	goffi.Register("datalog.rule", func(rule string) error { return RuleStr(rule) })
	goffi.Register("datalog.query", QueryStr)
}
