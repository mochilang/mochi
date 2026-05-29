// Package async provides a Go channel-based API for OTP process operations:
// spawn, send, monitor, demonitor, link, unlink, and a polling Mailbox for
// receiving messages from registered Erlang processes.
//
// All operations dispatch through the Caller interface (satisfied by
// *port.Manager) so no real `erl` binary is needed in tests.
//
// # Process operations
//
// Spawn, SpawnLink, Send, SendNamed, Exit, IsAlive, and ProcessInfo wrap the
// corresponding Erlang BIFs. They use synchronous Port calls and return
// typed results.
//
// # Mailbox
//
// A Mailbox wraps a Go channel. The caller calls NewMailbox with a Caller
// and the registered name of an Erlang mailbox gen_server (see the
// mochi_mailbox.erl template emitted by Phase 7). A background goroutine
// polls the Erlang mailbox at a configurable interval and pushes each
// message onto the channel. The goroutine stops when Close is called.
//
// For tests: use NewMailboxFromChan to inject a pre-filled channel directly;
// or call mailbox.Push to inject messages without polling.
package async

import (
	"fmt"
	"sync"
	"time"

	"mochi/package3/erlang/etf"
)

// Caller dispatches a synchronous function call to the Erlang node.
// *port.Manager satisfies this interface.
type Caller interface {
	Call(fun string, args []interface{}) (interface{}, error)
}

// defaultPollInterval is the rate at which Mailbox.poll drains the Erlang
// mailbox process when no custom interval is provided.
const defaultPollInterval = 50 * time.Millisecond

// pidArg converts an etf.Pid to a plain interface{} for use as a Call arg.
func pidArg(p etf.Pid) interface{} { return p }

// isAtom reports whether v is etf.Atom(a).
func isAtom(v interface{}, a string) bool {
	atom, ok := v.(etf.Atom)
	return ok && string(atom) == a
}

// expectOK returns nil when v is `ok`, or an error for {error, Reason}.
func expectOK(v interface{}) error {
	if isAtom(v, "ok") {
		return nil
	}
	if tup, ok := v.(etf.Tuple); ok && len(tup) == 2 && isAtom(tup[0], "error") {
		return fmt.Errorf("async: erlang error: %v", tup[1])
	}
	return fmt.Errorf("async: unexpected reply: %v", v)
}

// Mailbox buffers messages received from an Erlang mailbox process.
// Messages are pushed onto the channel by a background poll goroutine or
// directly via Push (in tests).
type Mailbox struct {
	ch     chan interface{}
	mu     sync.Mutex
	closed bool
	stop   chan struct{}
	done   chan struct{}
}

// NewMailbox starts a Mailbox that polls erlangName at pollInterval.
// erlangName is the registered name of the Erlang mailbox gen_server.
// The background goroutine calls `erlang_mailbox:recv(erlangName)` at each
// tick; the Erlang side returns {ok, Msg} or the atom `empty`.
// Call Close() to stop the poller and close the channel.
func NewMailbox(caller Caller, erlangName string, bufSize int, pollInterval time.Duration) *Mailbox {
	if pollInterval <= 0 {
		pollInterval = defaultPollInterval
	}
	if bufSize <= 0 {
		bufSize = 64
	}
	m := &Mailbox{
		ch:   make(chan interface{}, bufSize),
		stop: make(chan struct{}),
		done: make(chan struct{}),
	}
	go m.poll(caller, erlangName, pollInterval)
	return m
}

// NewMailboxFromChan wraps an existing channel as a Mailbox. Used by tests
// to inject a pre-populated channel without a live Erlang node.
// Close() closes the channel without waiting for a background goroutine.
func NewMailboxFromChan(ch chan interface{}) *Mailbox {
	done := make(chan struct{})
	close(done) // no background goroutine; Close() unblocks immediately
	return &Mailbox{ch: ch, stop: make(chan struct{}), done: done}
}

// Chan returns the receive-only message channel.
func (m *Mailbox) Chan() <-chan interface{} { return m.ch }

// Push injects a message directly onto the channel. Used only in tests.
func (m *Mailbox) Push(msg interface{}) {
	m.mu.Lock()
	defer m.mu.Unlock()
	if !m.closed {
		m.ch <- msg
	}
}

// Close stops the background poll goroutine (if any) and closes the channel.
func (m *Mailbox) Close() {
	m.mu.Lock()
	if m.closed {
		m.mu.Unlock()
		return
	}
	m.closed = true
	select {
	case <-m.stop:
	default:
		close(m.stop)
	}
	m.mu.Unlock()
	<-m.done
	close(m.ch)
}

// poll drains messages from the Erlang mailbox process at the given interval.
func (m *Mailbox) poll(caller Caller, erlangName string, interval time.Duration) {
	defer close(m.done)
	ticker := time.NewTicker(interval)
	defer ticker.Stop()
	for {
		select {
		case <-m.stop:
			return
		case <-ticker.C:
			for {
				v, err := caller.Call("erlang_mailbox:recv", []interface{}{etf.Atom(erlangName)})
				if err != nil {
					return
				}
				if isAtom(v, "empty") {
					break
				}
				tup, ok := v.(etf.Tuple)
				if !ok || len(tup) != 2 || !isAtom(tup[0], "ok") {
					break
				}
				m.mu.Lock()
				if m.closed {
					m.mu.Unlock()
					return
				}
				select {
				case m.ch <- tup[1]:
				default:
					// drop on full buffer
				}
				m.mu.Unlock()
			}
		}
	}
}
