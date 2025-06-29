package lsp

import "testing"

func TestAnalyzeValid(t *testing.T) {
	src := "print(\"hi\")"
	diags := Analyze("test.mochi", src)
	if len(diags) != 0 {
		t.Fatalf("expected no diagnostics, got %v", diags)
	}
}

func TestAnalyzeParseError(t *testing.T) {
	src := "let x ="
	diags := Analyze("test.mochi", src)
	if len(diags) == 0 {
		t.Fatal("expected parse diagnostic")
	}
}

func TestAnalyzeTypeError(t *testing.T) {
	src := "let x: int = \"hi\""
	diags := Analyze("test.mochi", src)
	if len(diags) == 0 {
		t.Fatal("expected type diagnostic")
	}
}
