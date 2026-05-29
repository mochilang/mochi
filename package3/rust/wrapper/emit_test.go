package wrapper

import (
	"strings"
	"testing"
)

func TestEmitLibRSContainsPrologue(t *testing.T) {
	c := Synth("hex", "0.4.3", hexLikeSurface())
	out := EmitLibRS(c)
	for _, want := range []string{
		"#[repr(C)]",
		"pub struct MochiString {",
		"pub struct MochiSlice {",
		"pub struct MochiOption {",
		"pub struct MochiResult {",
		"pub struct MochiHandle {",
		"unsafe fn mochi_string_to_str",
		"fn mochi_string_from_owned",
		"pub unsafe extern \"C\" fn mochi_string_free",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("lib.rs missing %q", want)
		}
	}
}

func TestEmitLibRSScalarBodyIsConcrete(t *testing.T) {
	c := Synth("hex", "0.4.3", hexLikeSurface())
	out := EmitLibRS(c)
	// to_upper_hex is fully scalar; its body should be a direct call,
	// not a todo!().
	if !strings.Contains(out, "mochi_hex_to_upper_hex(value: uint8_t) -> uint32_t") {
		t.Errorf("to_upper_hex signature missing: %s", grepWindow(out, "to_upper_hex"))
	}
	if !strings.Contains(out, "    hex::to_upper_hex(value)") {
		t.Errorf("to_upper_hex body missing direct call: %s", grepWindow(out, "to_upper_hex"))
	}
}

func TestEmitLibRSStringInputReturnsConcrete(t *testing.T) {
	// encode: &[u8] -> bytes (compound input -> todo!), return String -> string.
	c := Synth("hex", "0.4.3", hexLikeSurface())
	out := EmitLibRS(c)
	// encode has a bytes input so the body falls back to todo!.
	if !strings.Contains(out, "mochi_hex_encode(data: MochiSlice) -> MochiString") {
		t.Errorf("encode signature missing")
	}
	if !strings.Contains(out, "// TODO: marshal compound input/output") {
		t.Errorf("expected encode body to be a todo!() (bytes input is compound)")
	}
}

func TestEmitLibRSDecodeReturnsTodo(t *testing.T) {
	// decode returns Result<Vec<u8>, FromHexError> which is compound.
	c := Synth("hex", "0.4.3", hexLikeSurface())
	out := EmitLibRS(c)
	if !strings.Contains(out, "mochi_hex_decode(input: MochiString) -> MochiResult") {
		t.Errorf("decode signature missing")
	}
}

func TestEmitCargoTOML(t *testing.T) {
	c := Synth("hex", "0.4.3", hexLikeSurface())
	got := EmitCargoTOML(c)
	for _, want := range []string{
		`name = "mochi_wrap_hex"`,
		`version = "0.1.0"`,
		`edition = "2021"`,
		`publish = false`,
		`crate-type = ["cdylib", "rlib"]`,
		`hex = "=0.4.3"`,
	} {
		if !strings.Contains(got, want) {
			t.Errorf("Cargo.toml missing %q\nfull:\n%s", want, got)
		}
	}
}

func TestEmitSkippedTXTGroups(t *testing.T) {
	c := Synth("hex", "0.4.3", hexLikeSurface())
	got := EmitSkippedTXT(c)
	if !strings.Contains(got, "# SkipGeneric") {
		t.Errorf("SKIPPED.txt missing SkipGeneric group:\n%s", got)
	}
	if !strings.Contains(got, "SKIPPED: hex::decode_to_slice") {
		t.Errorf("SKIPPED.txt missing decode_to_slice entry:\n%s", got)
	}
	if !strings.Contains(got, "Reason: SkipGeneric") {
		t.Errorf("SKIPPED.txt missing Reason line:\n%s", got)
	}
}

func TestEmitSkippedTXTEmpty(t *testing.T) {
	c := &Crate{Name: "demo", Upstream: "demo"}
	got := EmitSkippedTXT(c)
	if got != "# No skipped items.\n" {
		t.Errorf("empty SKIPPED.txt = %q", got)
	}
}

func TestEmitSkippedTXTDeterministic(t *testing.T) {
	c := Synth("hex", "0.4.3", hexLikeSurface())
	first := EmitSkippedTXT(c)
	for i := 0; i < 5; i++ {
		if EmitSkippedTXT(c) != first {
			t.Errorf("EmitSkippedTXT non-deterministic at iter %d", i)
		}
	}
}

func TestEmitLibRSDeterministic(t *testing.T) {
	c := Synth("hex", "0.4.3", hexLikeSurface())
	first := EmitLibRS(c)
	for i := 0; i < 5; i++ {
		if EmitLibRS(c) != first {
			t.Errorf("EmitLibRS non-deterministic at iter %d", i)
		}
	}
}

func grepWindow(s, needle string) string {
	idx := strings.Index(s, needle)
	if idx < 0 {
		return "(no match for " + needle + ")"
	}
	start := idx - 40
	if start < 0 {
		start = 0
	}
	end := idx + len(needle) + 80
	if end > len(s) {
		end = len(s)
	}
	return s[start:end]
}
