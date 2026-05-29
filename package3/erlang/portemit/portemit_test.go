package portemit

import (
	"strings"
	"testing"

	"mochi/package3/erlang/typemap"
)

func TestEmitShimErl_ModuleDeclaration(t *testing.T) {
	out := EmitShimErl("cowboy", nil)
	if !strings.Contains(out, "-module(cowboy_mochi_shim).") {
		t.Errorf("missing -module declaration, got:\n%s", out)
	}
	if !strings.Contains(out, "-behaviour(gen_server).") {
		t.Error("missing -behaviour(gen_server)")
	}
}

func TestEmitShimErl_StartStop(t *testing.T) {
	out := EmitShimErl("ranch", nil)
	if !strings.Contains(out, "start_link() ->") {
		t.Error("missing start_link/0")
	}
	if !strings.Contains(out, "stop() ->") {
		t.Error("missing stop/0")
	}
}

func TestEmitShimErl_ExportsNoFns(t *testing.T) {
	out := EmitShimErl("hackney", nil)
	if !strings.Contains(out, "-export([start_link/0, stop/0]).") {
		t.Error("missing start_link/stop export")
	}
	// No per-fn export block when fns is nil.
	if strings.Count(out, "-export([") > 2 {
		t.Error("too many export blocks for empty fn list")
	}
}

func TestEmitShimErl_FunctionWrapper(t *testing.T) {
	fns := []FnSpec{
		{
			Module:   "cowboy",
			Function: "start_clear",
			Arity:    3,
			Args:     []typemap.MochiType{typemap.MochiString, typemap.MochiInt, typemap.MochiBytes},
			Return:   typemap.MochiString,
		},
	}
	out := EmitShimErl("cowboy", fns)

	// Export declaration for start_clear/3.
	if !strings.Contains(out, "start_clear/3") {
		t.Errorf("missing start_clear/3 export, got:\n%s", out)
	}
	// The function body calls gen_server:call.
	if !strings.Contains(out, "gen_server:call(?SERVER, {call,") {
		t.Error("missing gen_server:call in function wrapper")
	}
	// Module:function key.
	if !strings.Contains(out, `"cowboy:start_clear"`) {
		t.Error("missing dispatch key cowboy:start_clear")
	}
	// 3 args: Arg1, Arg2, Arg3.
	if !strings.Contains(out, "Arg1, Arg2, Arg3") {
		t.Errorf("expected Arg1, Arg2, Arg3 in wrapper, got:\n%s", out)
	}
}

func TestEmitShimErl_MultipleFunctions(t *testing.T) {
	fns := []FnSpec{
		{Module: "hackney", Function: "get", Arity: 2},
		{Module: "hackney", Function: "post", Arity: 3},
	}
	out := EmitShimErl("hackney", fns)
	if !strings.Contains(out, "get/2") {
		t.Error("missing get/2 export")
	}
	if !strings.Contains(out, "post/3") {
		t.Error("missing post/3 export")
	}
}

func TestEmitShimErl_GenServerCallbacks(t *testing.T) {
	out := EmitShimErl("jsx", nil)
	callbacks := []string{
		"init([]) ->",
		"handle_call(",
		"handle_cast(",
		"handle_info(",
		"terminate(",
	}
	for _, cb := range callbacks {
		if !strings.Contains(out, cb) {
			t.Errorf("missing gen_server callback %q", cb)
		}
	}
}

func TestEmitShimErl_PortProtocol(t *testing.T) {
	out := EmitShimErl("poolboy", nil)
	if !strings.Contains(out, "{packet, 4}") {
		t.Error("missing {packet, 4} in port options")
	}
	if !strings.Contains(out, "term_to_binary") {
		t.Error("missing term_to_binary encoding")
	}
	if !strings.Contains(out, "binary_to_term") {
		t.Error("missing binary_to_term decoding")
	}
	if !strings.Contains(out, "mochi_binary") {
		t.Error("missing mochi_binary reference")
	}
}

func TestEmitShimErl_TimeoutHandling(t *testing.T) {
	out := EmitShimErl("recon", nil)
	if !strings.Contains(out, "after 30000") {
		t.Error("missing 30-second timeout")
	}
	if !strings.Contains(out, "timeout") {
		t.Error("missing timeout error atom")
	}
}

func TestEmitShimErl_ExitStatusHandling(t *testing.T) {
	out := EmitShimErl("lager", nil)
	if !strings.Contains(out, "exit_status") {
		t.Error("missing exit_status handling")
	}
	if !strings.Contains(out, "mochi_binary_exit") {
		t.Error("missing mochi_binary_exit stop reason")
	}
}

func TestMakeArgList(t *testing.T) {
	args := makeArgList(3)
	if len(args) != 3 {
		t.Fatalf("len = %d, want 3", len(args))
	}
	if args[0] != "Arg1" || args[1] != "Arg2" || args[2] != "Arg3" {
		t.Errorf("unexpected args: %v", args)
	}
}

func TestMakeArgList_Zero(t *testing.T) {
	args := makeArgList(0)
	if len(args) != 0 {
		t.Errorf("expected empty, got %v", args)
	}
}
