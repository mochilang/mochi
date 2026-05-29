// Package portemit generates the shim.erl Erlang gen_server that wraps
// function calls via the OTP Port {packet,4} bridge protocol.
//
// For each package, a single shim.erl file is emitted. It contains:
//
//  1. A gen_server that opens a Port to the Mochi binary (priv/mochi_binary).
//  2. One exported function per translated spec: calls dispatch via the Port.
//  3. A "call" helper that encodes {call, Fun, Args} as ETF and waits for
//     {reply, Result} or {error, Reason}.
//
// The generated file is placed at:
//
//	<workdir>/erlang_shims/<appname>/src/<appname>_mochi_shim.erl
//
// The shim communicates with the Mochi binary using {packet,4} framing:
// each message is prefixed with a 4-byte big-endian length, then the
// ETF-encoded payload.
//
// Message protocol (from research note 08 §3):
//
//	Mochi → Erlang: {call, SeqID, Fun, Args}
//	Erlang → Mochi: {reply, SeqID, Result} | {error, SeqID, Reason}
package portemit

import (
	"fmt"
	"strings"

	"mochi/package3/erlang/typemap"
)

// FnSpec describes a single function to be wrapped in the shim.
type FnSpec struct {
	// Module is the Erlang module name.
	Module string
	// Function is the Erlang function name.
	Function string
	// Arity is the function arity.
	Arity int
	// Args is the list of argument Mochi types (for documentation).
	Args []typemap.MochiType
	// Return is the return Mochi type.
	Return typemap.MochiType
}

// EmitShimErl generates the complete shim.erl source for a single OTP application.
// appName is the OTP application name (e.g. "cowboy"). fns is the list of
// translated functions to expose.
func EmitShimErl(appName string, fns []FnSpec) string {
	var sb strings.Builder
	module := appName + "_mochi_shim"

	sb.WriteString(fmt.Sprintf("-module(%s).\n", module))
	sb.WriteString("-behaviour(gen_server).\n\n")

	// Exports
	sb.WriteString("%% Public API\n")
	sb.WriteString("-export([start_link/0, stop/0]).\n")
	// Per-function exports.
	if len(fns) > 0 {
		sb.WriteString("-export([\n")
		for i, fn := range fns {
			sb.WriteString(fmt.Sprintf("  %s/%d", fn.Function, fn.Arity))
			if i < len(fns)-1 {
				sb.WriteString(",\n")
			} else {
				sb.WriteString("\n")
			}
		}
		sb.WriteString("]).\n")
	}
	sb.WriteString("\n%% gen_server callbacks\n")
	sb.WriteString("-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).\n\n")

	sb.WriteString("-define(SERVER, ?MODULE).\n\n")
	sb.WriteString("-record(state, {port :: port()}).\n\n")

	// start_link
	sb.WriteString("start_link() ->\n")
	sb.WriteString("  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).\n\n")

	// stop
	sb.WriteString("stop() ->\n")
	sb.WriteString("  gen_server:stop(?SERVER).\n\n")

	// Per-function wrappers.
	for _, fn := range fns {
		args := makeArgList(fn.Arity)
		sb.WriteString(fmt.Sprintf("%s(%s) ->\n", fn.Function, strings.Join(args, ", ")))
		sb.WriteString(fmt.Sprintf("  gen_server:call(?SERVER, {call, %q, [%s]}).\n\n",
			fn.Module+":"+fn.Function, strings.Join(args, ", ")))
	}

	// gen_server init
	sb.WriteString("init([]) ->\n")
	sb.WriteString("  PrivDir = code:priv_dir(?MODULE),\n")
	sb.WriteString("  Binary = filename:join(PrivDir, \"mochi_binary\"),\n")
	sb.WriteString("  Port = open_port({spawn_executable, Binary}, [{packet, 4}, binary, exit_status]),\n")
	sb.WriteString("  {ok, #state{port = Port}}.\n\n")

	// handle_call dispatch
	sb.WriteString("handle_call({call, Fun, Args}, _From, #state{port = Port} = State) ->\n")
	sb.WriteString("  Msg = term_to_binary({call, Fun, Args}),\n")
	sb.WriteString("  Port ! {self(), {command, Msg}},\n")
	sb.WriteString("  receive\n")
	sb.WriteString("    {Port, {data, Data}} ->\n")
	sb.WriteString("      case binary_to_term(Data) of\n")
	sb.WriteString("        {reply, Result} -> {reply, {ok, Result}, State};\n")
	sb.WriteString("        {error, Reason} -> {reply, {error, Reason}, State}\n")
	sb.WriteString("      end\n")
	sb.WriteString("  after 30000 ->\n")
	sb.WriteString("    {reply, {error, timeout}, State}\n")
	sb.WriteString("  end;\n")
	sb.WriteString("handle_call(_Req, _From, State) ->\n")
	sb.WriteString("  {reply, {error, unknown_call}, State}.\n\n")

	// handle_cast
	sb.WriteString("handle_cast(_Msg, State) -> {noreply, State}.\n\n")

	// handle_info
	sb.WriteString("handle_info({Port, {exit_status, Code}}, #state{port = Port} = State) ->\n")
	sb.WriteString("  error_logger:error_msg(\"mochi binary exited with code ~p~n\", [Code]),\n")
	sb.WriteString("  {stop, {mochi_binary_exit, Code}, State};\n")
	sb.WriteString("handle_info(_Info, State) -> {noreply, State}.\n\n")

	// terminate
	sb.WriteString("terminate(_Reason, #state{port = Port}) ->\n")
	sb.WriteString("  Port ! {self(), close},\n")
	sb.WriteString("  ok.\n")

	return sb.String()
}

// makeArgList returns a list of Erlang variable names [Arg1, Arg2, ...] of
// length n.
func makeArgList(n int) []string {
	args := make([]string, n)
	for i := range args {
		args[i] = fmt.Sprintf("Arg%d", i+1)
	}
	return args
}
