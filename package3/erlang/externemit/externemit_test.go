package externemit

import (
	"strings"
	"testing"

	breakers "mochi/package3/erlang/errors"
	"mochi/package3/erlang/typemap"
)

func TestEmitShimMochi_Header(t *testing.T) {
	out := EmitShimMochi("cowboy", nil, nil)
	if !strings.Contains(out, `"cowboy"`) {
		t.Error("missing package name in header comment")
	}
	if !strings.Contains(out, "Auto-generated") {
		t.Error("missing auto-generated comment")
	}
}

func TestEmitShimMochi_OTPExternTypes(t *testing.T) {
	out := EmitShimMochi("ranch", nil, nil)
	if !strings.Contains(out, "extern type erlang.Pid") {
		t.Error("missing extern type erlang.Pid")
	}
	if !strings.Contains(out, "extern type erlang.Port") {
		t.Error("missing extern type erlang.Port")
	}
	if !strings.Contains(out, "extern type erlang.Reference") {
		t.Error("missing extern type erlang.Reference")
	}
}

func TestEmitShimMochi_ExternFn(t *testing.T) {
	fns := []FnDecl{
		{
			ShimModule: "cowboy_mochi_shim",
			Function:   "start_clear",
			Args: []ArgDecl{
				{Name: "name", Type: typemap.MochiString},
				{Name: "port", Type: typemap.MochiInt},
			},
			Return: "result<unit, string>",
		},
	}
	out := EmitShimMochi("cowboy", fns, nil)
	if !strings.Contains(out, "extern fn cowboy_mochi_shim::start_clear(") {
		t.Errorf("missing extern fn declaration:\n%s", out)
	}
	if !strings.Contains(out, "name: string") {
		t.Error("missing argument name:string")
	}
	if !strings.Contains(out, "port: int") {
		t.Error("missing argument port:int")
	}
	if !strings.Contains(out, ") result<unit, string>") {
		t.Error("missing return type")
	}
}

func TestEmitShimMochi_MultipleFns(t *testing.T) {
	fns := []FnDecl{
		{ShimModule: "hackney_mochi_shim", Function: "get", Return: "result<bytes, string>"},
		{ShimModule: "hackney_mochi_shim", Function: "post", Return: "result<bytes, string>"},
	}
	out := EmitShimMochi("hackney", fns, nil)
	if !strings.Contains(out, "extern fn hackney_mochi_shim::get(") {
		t.Error("missing get extern fn")
	}
	if !strings.Contains(out, "extern fn hackney_mochi_shim::post(") {
		t.Error("missing post extern fn")
	}
}

func TestEmitShimMochi_SkippedBlock(t *testing.T) {
	skipped := []breakers.SkipReport{
		{
			Module:   "cowboy",
			Function: "stream_body",
			Arity:    3,
			Position: "arg1",
			Reason:   breakers.SkipIodata,
			Detail:   "iodata() argument",
		},
	}
	out := EmitShimMochi("cowboy", nil, skipped)
	if !strings.Contains(out, "SKIPPED") {
		t.Error("missing SKIPPED section")
	}
	if !strings.Contains(out, "stream_body") {
		t.Error("missing function name in SKIPPED section")
	}
	if !strings.Contains(out, "SkipIodata") {
		t.Error("missing SkipIodata reason")
	}
}

func TestEmitShimMochi_NoSkippedBlock(t *testing.T) {
	out := EmitShimMochi("jsx", nil, nil)
	if strings.Contains(out, "SKIPPED") {
		t.Error("should not have SKIPPED section when no skips")
	}
}

func TestEmitSkippedTxt_Empty(t *testing.T) {
	out := EmitSkippedTxt(nil)
	if !strings.Contains(out, "No skipped") {
		t.Errorf("empty skip report should say 'No skipped', got: %q", out)
	}
}

func TestEmitSkippedTxt_WithEntries(t *testing.T) {
	skipped := []breakers.SkipReport{
		{Module: "lager", Function: "log", Arity: 3, Position: "arg2", Reason: breakers.SkipAnyTerm, Detail: "term()"},
		{Module: "lager", Reason: breakers.SkipNoTypeinfo, Detail: "no abstract code"},
	}
	out := EmitSkippedTxt(skipped)
	if !strings.Contains(out, "Total skipped: 2") {
		t.Errorf("expected 'Total skipped: 2', got:\n%s", out)
	}
	if !strings.Contains(out, "lager:log/3") {
		t.Error("missing lager:log/3 location")
	}
	if !strings.Contains(out, "SkipAnyTerm") {
		t.Error("missing SkipAnyTerm")
	}
	if !strings.Contains(out, "SkipNoTypeinfo") {
		t.Error("missing SkipNoTypeinfo")
	}
}

func TestEmitShimMochi_NoArgsFunction(t *testing.T) {
	fns := []FnDecl{
		{ShimModule: "poolboy_mochi_shim", Function: "status", Args: nil, Return: "string"},
	}
	out := EmitShimMochi("poolboy", fns, nil)
	if !strings.Contains(out, "extern fn poolboy_mochi_shim::status() string") {
		t.Errorf("missing no-arg extern fn:\n%s", out)
	}
}
