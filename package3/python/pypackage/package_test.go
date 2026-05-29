package pypackage

import (
	"strings"
	"testing"

	"mochi/package3/python/typemap"
)

func TestPackageValidateRejectsEmptyDistribution(t *testing.T) {
	p := Package{Version: "0.1.0", Summary: "x", License: "Apache-2.0"}
	if err := p.Validate(); err == nil {
		t.Fatal("expected error for empty Distribution")
	} else if !strings.Contains(err.Error(), "Distribution") {
		t.Fatalf("expected Distribution error, got %v", err)
	}
}

func TestPackageValidateRejectsBadDistribution(t *testing.T) {
	cases := []string{"Foo", "_foo", "foo_bar", "foo.bar", "-foo", "foo-"}
	for _, n := range cases {
		p := Package{Distribution: n, Version: "0.1.0", Summary: "x", License: "Apache-2.0"}
		if err := p.Validate(); err == nil {
			t.Fatalf("expected error for distribution %q", n)
		}
	}
}

func TestPackageValidateAcceptsPEP503Name(t *testing.T) {
	p := Package{Distribution: "mochi-httpx-wrap", Version: "0.1.0", Summary: "x", License: "Apache-2.0"}
	if err := p.Validate(); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
}

func TestPackageValidateRequiresVersionSummaryLicense(t *testing.T) {
	p := Package{Distribution: "x"}
	if err := p.Validate(); err == nil || !strings.Contains(err.Error(), "Version") {
		t.Fatalf("expected Version error, got %v", err)
	}
	p.Version = "0.1.0"
	if err := p.Validate(); err == nil || !strings.Contains(err.Error(), "Summary") {
		t.Fatalf("expected Summary error, got %v", err)
	}
	p.Summary = "y"
	if err := p.Validate(); err == nil || !strings.Contains(err.Error(), "License") {
		t.Fatalf("expected License error, got %v", err)
	}
}

func TestPackageValidateRejectsBadExportName(t *testing.T) {
	p := Package{
		Distribution: "x", Version: "0.1.0", Summary: "y", License: "Apache-2.0",
		Exports: []Export{{Name: "1bad", Kind: ExportConstant, Type: typemap.MochiType{Kind: typemap.KindScalar, Name: "int"}}},
	}
	if err := p.Validate(); err == nil || !strings.Contains(err.Error(), "Python identifier") {
		t.Fatalf("expected identifier error, got %v", err)
	}
}

func TestPackageValidateRejectsEmptyExportName(t *testing.T) {
	p := Package{
		Distribution: "x", Version: "0.1.0", Summary: "y", License: "Apache-2.0",
		Exports: []Export{{Name: "", Kind: ExportConstant}},
	}
	if err := p.Validate(); err == nil || !strings.Contains(err.Error(), "empty Name") {
		t.Fatalf("expected empty Name error, got %v", err)
	}
}

func TestPackageModuleNameDefault(t *testing.T) {
	p := Package{Distribution: "mochi-httpx"}
	if got, want := p.ModuleName(), "mochi_httpx"; got != want {
		t.Fatalf("ModuleName = %q, want %q", got, want)
	}
	p.Module = "foo"
	if got, want := p.ModuleName(), "foo"; got != want {
		t.Fatalf("ModuleName override = %q, want %q", got, want)
	}
}

func TestExportKindString(t *testing.T) {
	pairs := map[ExportKind]string{
		ExportFunc:      "fun",
		ExportRecord:    "record",
		ExportInterface: "interface",
		ExportConstant:  "const",
		ExportKind(99):  "unknown",
	}
	for k, want := range pairs {
		if got := k.String(); got != want {
			t.Fatalf("ExportKind(%d).String() = %q, want %q", k, got, want)
		}
	}
}
