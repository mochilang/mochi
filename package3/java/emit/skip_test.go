package emit

import (
	"strings"
	"testing"

	javaerrors "mochi/package3/java/errors"
)

func TestEmitSkipReport(t *testing.T) {
	skips := []javaerrors.SkipReport{
		{ItemPath: "com.example.Foo.bar", Reason: javaerrors.SkipVarargs, Detail: "varargs method"},
		{ItemPath: "com.example.Foo.baz", Reason: javaerrors.SkipNative, Detail: "native method"},
	}
	report := EmitSkipReport(skips)
	if !strings.Contains(report, "Total: 2") {
		t.Errorf("missing total count in report: %s", report)
	}
	if !strings.Contains(report, "com.example.Foo.bar") {
		t.Errorf("missing first item in report: %s", report)
	}
	if !strings.Contains(report, "SkipVarargs") {
		t.Errorf("missing reason in report: %s", report)
	}
}

func TestEmitSkipReportEmpty(t *testing.T) {
	report := EmitSkipReport(nil)
	if !strings.Contains(report, "No skipped") {
		t.Errorf("empty report should say no skipped items: %s", report)
	}
}
