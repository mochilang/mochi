// rebar3.go adds rebar3 compile integration and shim materialisation to the
// build driver. These capabilities are separated from driver.go so phase 0
// (skeleton) compiles without an exec dependency in tests.
package build

import (
	"context"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"time"
)

const defaultRebar3Timeout = 5 * time.Minute

// Rebar3Options controls RunRebar3Compile behaviour.
type Rebar3Options struct {
	// Rebar3Bin is the path to the rebar3 executable.
	// Default: "rebar3" (PATH lookup).
	Rebar3Bin string
	// Timeout caps the compile step. Default: 5 minutes.
	Timeout time.Duration
	// Verbose pipes rebar3 stdout/stderr to os.Stderr when true.
	Verbose bool
}

// RunRebar3Compile runs `rebar3 compile` inside workspaceDir. It expects
// rebar.config (and optionally rebar3.lock) to already exist in that
// directory. Returns ErrRebar3NotFound when the rebar3 binary cannot be
// located so callers can provide a useful diagnostic.
func (d *Driver) RunRebar3Compile(workspaceDir string, ropts Rebar3Options) error {
	rebar3 := ropts.Rebar3Bin
	if rebar3 == "" {
		rebar3 = "rebar3"
	}
	timeout := ropts.Timeout
	if timeout <= 0 {
		timeout = defaultRebar3Timeout
	}

	if _, err := exec.LookPath(rebar3); err != nil {
		return &ErrRebar3NotFound{Bin: rebar3}
	}

	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()

	cmd := exec.CommandContext(ctx, rebar3, "compile")
	cmd.Dir = workspaceDir
	if d.opts.Deterministic {
		cmd.Env = append(cmd.Environ(), "SOURCE_DATE_EPOCH=0")
	}

	if ropts.Verbose || d.opts.Verbose {
		cmd.Stdout = os.Stderr
		cmd.Stderr = os.Stderr
	}

	if err := cmd.Run(); err != nil {
		if ctx.Err() != nil {
			return fmt.Errorf("build: rebar3 compile timed out after %s", timeout)
		}
		return fmt.Errorf("build: rebar3 compile in %s: %w", workspaceDir, err)
	}
	return nil
}

// ErrRebar3NotFound is returned when the rebar3 binary cannot be found.
type ErrRebar3NotFound struct{ Bin string }

func (e *ErrRebar3NotFound) Error() string {
	return fmt.Sprintf("build: rebar3 not found (%q); install rebar3 and ensure it is on PATH", e.Bin)
}

// MaterializeShimErl writes the generated shim.erl source to the canonical
// location inside the erlang_shims workspace:
//
//	<shimsDir>/<appName>/src/<appName>_mochi_shim.erl
//
// shimsDir is typically obtained via d.ErlangShimsDir(). The directory tree
// is created as needed.
func (d *Driver) MaterializeShimErl(shimsDir, appName, content string) (string, error) {
	appDir := filepath.Join(shimsDir, appName, "src")
	if err := os.MkdirAll(appDir, 0o755); err != nil {
		return "", fmt.Errorf("build: create shim src dir %s: %w", appDir, err)
	}
	path := filepath.Join(appDir, appName+"_mochi_shim.erl")
	if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
		return "", fmt.Errorf("build: write shim.erl %s: %w", path, err)
	}
	return path, nil
}

// MaterializeRunnerErl writes the mochi_port_runner.erl source into shimsDir.
// This module is the Erlang-side stdin/stdout dispatch loop that the Go Port
// manager spawns when Mochi calls into Erlang (Direction 1).
// The file is placed at <shimsDir>/mochi_port_runner/src/mochi_port_runner.erl.
func (d *Driver) MaterializeRunnerErl(shimsDir string) (string, error) {
	appDir := filepath.Join(shimsDir, "mochi_port_runner", "src")
	if err := os.MkdirAll(appDir, 0o755); err != nil {
		return "", fmt.Errorf("build: create runner src dir: %w", err)
	}
	path := filepath.Join(appDir, "mochi_port_runner.erl")
	if err := os.WriteFile(path, []byte(runnerErlSource), 0o644); err != nil {
		return "", fmt.Errorf("build: write mochi_port_runner.erl: %w", err)
	}
	return path, nil
}

// runnerErlSource is the Erlang stdin/stdout dispatch loop that the Go Port
// manager spawns for Direction-1 calls (Mochi → Erlang). It reads
// {packet,4}-framed ETF from stdin and dispatches {call, SeqID, Fun, Args}
// to the named function, returning {reply, SeqID, Result} or
// {error, SeqID, {Class, Reason}} on exceptions.
//
// Fun is an atom of the form "Module:Function" (e.g. 'cowboy:start_http').
// Args is an Erlang list of ETF-decoded values.
//
// The runner exits cleanly on the `stop` atom or on stdin EOF.
const runnerErlSource = `-module(mochi_port_runner).
-export([main/0]).

main() ->
    Port = erlang:open_port({fd, 0, 1}, [{packet, 4}, binary]),
    loop(Port).

loop(Port) ->
    receive
        {Port, {data, Data}} ->
            case catch binary_to_term(Data) of
                {call, SeqID, Fun, Args} ->
                    Reply = dispatch(SeqID, Fun, Args),
                    Encoded = term_to_binary(Reply),
                    port_command(Port, Encoded),
                    loop(Port);
                _ ->
                    loop(Port)
            end;
        {Port, closed} ->
            erlang:halt(0);
        {'EXIT', Port, _} ->
            erlang:halt(0);
        _ ->
            loop(Port)
    end.

dispatch(SeqID, Fun, Args) ->
    [ModStr, FunStr] = string:split(atom_to_list(Fun), ":"),
    Mod = list_to_atom(ModStr),
    F   = list_to_atom(FunStr),
    try
        Result = apply(Mod, F, Args),
        {reply, SeqID, Result}
    catch
        Class:Reason ->
            {error, SeqID, {Class, Reason}}
    end.
`
