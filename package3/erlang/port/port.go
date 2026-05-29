// Package port manages the Go-side OTP Port process connection for the
// MEP-66 Erlang bridge (Direction 1: Mochi calls Erlang).
//
// When a Mochi program contains `import erlang "..."` declarations the
// compiled binary must be able to dispatch function calls into the Erlang
// ecosystem at runtime. This package handles that path:
//
//  1. Start() spawns `erl -noshell` with the pre-compiled BEAM shims on
//     the code path and a mochi_port_runner main loop on stdin/stdout.
//  2. Call() encodes {call, SeqID, Fun, Args} as an ETF message with
//     {packet,4} framing and waits for a matching {reply, SeqID, Result}
//     or {error, SeqID, Reason} reply.
//  3. Stop() sends a `stop` atom and waits for the Erlang OS process to exit.
//
// {packet,4} framing: each message is a 4-byte big-endian payload length
// followed by the ETF-encoded payload (including the 0x83 version magic).
//
// The Erlang-side runner module (mochi_port_runner.erl) is emitted by the
// build driver (package3/erlang/build) and compiled with rebar3 before
// the Port manager is started.
package port

import (
	"encoding/binary"
	"fmt"
	"io"
	"os/exec"
	"sync"
	"sync/atomic"
	"time"

	"mochi/package3/erlang/etf"
)

const (
	defaultTimeout  = 30 * time.Second
	maxMessageBytes = 64 * 1024 * 1024 // 64 MiB sanity cap
)

// Manager holds a live connection to an Erlang Port process.
// All exported methods are safe for concurrent use.
type Manager struct {
	cmd   *exec.Cmd
	stdin io.WriteCloser

	writeMu sync.Mutex // serialises writes to stdin
	pendMu  sync.Mutex // protects pending map
	pending map[uint32]chan callResult
	nextSeq atomic.Uint32

	timeout   time.Duration
	done      chan struct{}
	closeOnce sync.Once
}

type callResult struct {
	val interface{}
	err error
}

// Options configures a Manager.
type Options struct {
	// ErlBin is the path to the `erl` binary. Default: "erl" (PATH lookup).
	ErlBin string
	// BeamDirs are added to the Erlang code path via -pa (before -run).
	BeamDirs []string
	// RunnerModule is the Erlang module invoked via -run … main.
	// Default: "mochi_port_runner".
	RunnerModule string
	// Cookie sets -setcookie (optional; omit for single-node deployments).
	Cookie string
	// Timeout is the per-call deadline. Default: 30s.
	Timeout time.Duration
	// ExtraEnv contains additional KEY=VALUE pairs for the child process.
	ExtraEnv []string
}

// Start launches the Erlang OS process and begins the background read loop.
// The caller must call Stop() when done.
func Start(opts Options) (*Manager, error) {
	erlBin := opts.ErlBin
	if erlBin == "" {
		erlBin = "erl"
	}
	runnerMod := opts.RunnerModule
	if runnerMod == "" {
		runnerMod = "mochi_port_runner"
	}
	timeout := opts.Timeout
	if timeout <= 0 {
		timeout = defaultTimeout
	}

	var args []string
	args = append(args, "-noshell")
	for _, dir := range opts.BeamDirs {
		args = append(args, "-pa", dir)
	}
	if opts.Cookie != "" {
		args = append(args, "-setcookie", opts.Cookie)
	}
	args = append(args, "-run", runnerMod, "main")

	cmd := exec.Command(erlBin, args...)
	if len(opts.ExtraEnv) > 0 {
		cmd.Env = append(cmd.Environ(), opts.ExtraEnv...)
	}

	stdin, err := cmd.StdinPipe()
	if err != nil {
		return nil, fmt.Errorf("port: stdin pipe: %w", err)
	}
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		return nil, fmt.Errorf("port: stdout pipe: %w", err)
	}

	if err := cmd.Start(); err != nil {
		return nil, fmt.Errorf("port: start %q: %w", erlBin, err)
	}

	m := &Manager{
		cmd:     cmd,
		stdin:   stdin,
		pending: make(map[uint32]chan callResult),
		timeout: timeout,
		done:    make(chan struct{}),
	}

	go m.readLoop(stdout)
	return m, nil
}

// StartWithPipes creates a Manager that communicates over caller-supplied
// pipes instead of spawning a subprocess. Used by tests to inject a mock
// Erlang side without requiring a real `erl` binary.
func StartWithPipes(r io.Reader, w io.WriteCloser, timeout time.Duration) *Manager {
	if timeout <= 0 {
		timeout = defaultTimeout
	}
	m := &Manager{
		stdin:   w,
		pending: make(map[uint32]chan callResult),
		timeout: timeout,
		done:    make(chan struct{}),
	}
	go m.readLoop(r)
	return m
}

// Call dispatches a function call to the connected Erlang node and waits
// for the reply. fun is the "Module:Function" atom string. args is the
// list of ETF-encodable arguments.
func (m *Manager) Call(fun string, args []interface{}) (interface{}, error) {
	seq := m.nextSeq.Add(1)
	ch := make(chan callResult, 1)

	m.pendMu.Lock()
	m.pending[seq] = ch
	m.pendMu.Unlock()

	// Build: {call, SeqID, Fun, [Args...]}
	msg := etf.Tuple{
		etf.Atom("call"),
		int64(seq),
		etf.Atom(fun),
		etf.List(args),
	}
	if err := m.writePacket(msg); err != nil {
		m.pendMu.Lock()
		delete(m.pending, seq)
		m.pendMu.Unlock()
		return nil, fmt.Errorf("port: send call %q: %w", fun, err)
	}

	timer := time.NewTimer(m.timeout)
	defer timer.Stop()

	select {
	case r := <-ch:
		return r.val, r.err
	case <-timer.C:
		m.pendMu.Lock()
		delete(m.pending, seq)
		m.pendMu.Unlock()
		return nil, fmt.Errorf("port: call %q timed out after %s", fun, m.timeout)
	case <-m.done:
		return nil, fmt.Errorf("port: manager stopped")
	}
}

// Stop closes the stdin pipe (which signals EOF to the Erlang runner) and
// waits for the OS process to exit. Safe to call multiple times.
func (m *Manager) Stop() error {
	var retErr error
	m.closeOnce.Do(func() {
		retErr = m.stdin.Close()
		close(m.done)
		if m.cmd != nil {
			_ = m.cmd.Wait()
		}
	})
	return retErr
}

// writePacket encodes v as ETF and writes it with a 4-byte big-endian
// length prefix. Holds writeMu for the duration so concurrent callers
// cannot interleave their header and payload bytes.
func (m *Manager) writePacket(v interface{}) error {
	data, err := etf.Encode(v)
	if err != nil {
		return fmt.Errorf("etf encode: %w", err)
	}
	var hdr [4]byte
	binary.BigEndian.PutUint32(hdr[:], uint32(len(data)))
	m.writeMu.Lock()
	defer m.writeMu.Unlock()
	if _, err := m.stdin.Write(hdr[:]); err != nil {
		return err
	}
	_, err = m.stdin.Write(data)
	return err
}

// readLoop runs in its own goroutine and dispatches incoming packets to
// waiting Call goroutines. It terminates when r returns an error (EOF or
// process death).
func (m *Manager) readLoop(r io.Reader) {
	defer func() {
		// drain any callers still waiting with a connection-closed error
		m.drainPending(fmt.Errorf("port: connection closed"))
	}()

	var hdr [4]byte
	for {
		if _, err := io.ReadFull(r, hdr[:]); err != nil {
			return
		}
		n := binary.BigEndian.Uint32(hdr[:])
		if n > maxMessageBytes {
			m.drainPending(fmt.Errorf("port: oversized message (%d bytes)", n))
			return
		}
		payload := make([]byte, n)
		if _, err := io.ReadFull(r, payload); err != nil {
			return
		}
		term, err := etf.Decode(payload)
		if err != nil {
			// malformed packet; skip without aborting the loop
			continue
		}
		m.dispatch(term)
	}
}

// dispatch routes a decoded reply term to the waiting Call goroutine.
// Expected shapes:
//
//	{reply, SeqID, Result}
//	{error, SeqID, Reason}
func (m *Manager) dispatch(term interface{}) {
	tup, ok := term.(etf.Tuple)
	if !ok || len(tup) < 2 {
		return
	}
	seq, ok := termToUint32(tup[1])
	if !ok {
		return
	}
	m.pendMu.Lock()
	ch, found := m.pending[seq]
	if found {
		delete(m.pending, seq)
	}
	m.pendMu.Unlock()
	if !found {
		return
	}

	tag, _ := tup[0].(etf.Atom)
	switch tag {
	case "reply":
		var val interface{}
		if len(tup) >= 3 {
			val = tup[2]
		}
		ch <- callResult{val: val}
	case "error":
		var reason interface{}
		if len(tup) >= 3 {
			reason = tup[2]
		}
		ch <- callResult{err: fmt.Errorf("port: erlang error: %v", reason)}
	default:
		ch <- callResult{err: fmt.Errorf("port: unexpected reply tag %q", tag)}
	}
}

func (m *Manager) drainPending(err error) {
	m.pendMu.Lock()
	defer m.pendMu.Unlock()
	for seq, ch := range m.pending {
		ch <- callResult{err: err}
		delete(m.pending, seq)
	}
}

// termToUint32 extracts a uint32 sequence ID from an ETF integer term.
func termToUint32(v interface{}) (uint32, bool) {
	switch n := v.(type) {
	case int:
		return uint32(n), true
	case int64:
		return uint32(n), true
	case uint32:
		return n, true
	}
	return 0, false
}
