package wrapper_test

import (
	"strings"
	"testing"

	"mochi/package3/java/typemap"
	"mochi/package3/java/wrapper"
)

func TestEmitFutureWrapper_ContainsMethod(t *testing.T) {
	stringMapping, _, _ := typemap.Map("java.lang.String")
	m := wrapper.FutureWrapMethod{
		ClassName:   "com.example.MyService",
		OrigName:    "getData",
		WrapName:    "MochiWrap_MyService_getData_future",
		ElemMapping: stringMapping,
	}
	src := wrapper.EmitFutureWrapper(m)
	for _, want := range []string{
		"public static void MochiWrap_MyService_getData_future",
		"long __handle",
		"long __cbHandle",
		"thenAccept",
		"MochiRuntime.callback",
	} {
		if !strings.Contains(src, want) {
			t.Errorf("EmitFutureWrapper output missing %q:\n%s", want, src)
		}
	}
}

func TestEmitFutureWrapper_NilMapping(t *testing.T) {
	m := wrapper.FutureWrapMethod{
		ClassName:   "com.example.Svc",
		OrigName:    "doWork",
		WrapName:    "MochiWrap_Svc_doWork_future",
		ElemMapping: nil,
	}
	src := wrapper.EmitFutureWrapper(m)
	if src == "" {
		t.Error("EmitFutureWrapper returned empty string for nil elem mapping")
	}
}
