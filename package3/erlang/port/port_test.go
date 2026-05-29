package port

import (
	"encoding/binary"
	"io"
	"sync"
	"testing"
	"time"

	"mochi/package3/erlang/etf"
)

// mockErlang simulates the Erlang side of the Port bridge.
// It reads {call, SeqID, Fun, Args} packets and sends back
// {reply, SeqID, RetVal} or {error, SeqID, Reason}.
type mockErlang struct {
	r     io.Reader
	w     io.Writer
	mu    sync.Mutex
	rules map[string]mockRule
}

type mockRule struct {
	retVal interface{}
	retErr string // non-empty → send {error, SeqID, retErr}
}

func newMockErlang(r io.Reader, w io.Writer) *mockErlang {
	return &mockErlang{r: r, w: w, rules: make(map[string]mockRule)}
}

func (e *mockErlang) expect(fun string, retVal interface{}) {
	e.mu.Lock()
	e.rules[fun] = mockRule{retVal: retVal}
	e.mu.Unlock()
}

func (e *mockErlang) expectErr(fun string, reason string) {
	e.mu.Lock()
	e.rules[fun] = mockRule{retErr: reason}
	e.mu.Unlock()
}

func (e *mockErlang) serveOne() error {
	var hdr [4]byte
	if _, err := io.ReadFull(e.r, hdr[:]); err != nil {
		return err
	}
	n := binary.BigEndian.Uint32(hdr[:])
	payload := make([]byte, n)
	if _, err := io.ReadFull(e.r, payload); err != nil {
		return err
	}
	term, err := etf.Decode(payload)
	if err != nil {
		return err
	}
	tup, ok := term.(etf.Tuple)
	if !ok || len(tup) < 3 {
		return nil // ignore unexpected shapes
	}
	tag, _ := tup[0].(etf.Atom)
	if tag == "stop" {
		return io.EOF
	}
	if tag != "call" || len(tup) < 4 {
		return nil
	}
	seq, _ := termToUint32(tup[1])
	fun, _ := tup[2].(etf.Atom)

	e.mu.Lock()
	rule, found := e.rules[string(fun)]
	e.mu.Unlock()

	var reply interface{}
	if !found {
		reply = etf.Tuple{etf.Atom("error"), int64(seq), etf.Atom("unknown_function")}
	} else if rule.retErr != "" {
		reply = etf.Tuple{etf.Atom("error"), int64(seq), etf.Atom(rule.retErr)}
	} else {
		reply = etf.Tuple{etf.Atom("reply"), int64(seq), rule.retVal}
	}

	data, err := etf.Encode(reply)
	if err != nil {
		return err
	}
	var rhdr [4]byte
	binary.BigEndian.PutUint32(rhdr[:], uint32(len(data)))
	if _, err := e.w.Write(rhdr[:]); err != nil {
		return err
	}
	_, err = e.w.Write(data)
	return err
}

// makeConnectedPair creates a Manager wired to a mockErlang via in-process pipes.
func makeConnectedPair(t *testing.T) (*Manager, *mockErlang) {
	t.Helper()
	// Go→Erlang: mgr writes to goW; mock reads from goR
	goR, goW := io.Pipe()
	// Erlang→Go: mock writes to erlW; mgr reads from erlR
	erlR, erlW := io.Pipe()

	mock := newMockErlang(goR, erlW)
	mgr := StartWithPipes(erlR, goW, 5*time.Second)
	return mgr, mock
}

func TestCall_SuccessfulReply(t *testing.T) {
	mgr, mock := makeConnectedPair(t)
	mock.expect("cowboy:start_http", int64(42))

	// Serve requests until the manager stops (pipe closes → serveOne returns EOF).
	go func() {
		for {
			if err := mock.serveOne(); err != nil {
				return
			}
		}
	}()

	result, err := mgr.Call("cowboy:start_http", []interface{}{int64(8080)})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result.(int64) != 42 {
		t.Errorf("result = %v, want 42", result)
	}
	mgr.Stop()
}

func TestCall_ErrorReply(t *testing.T) {
	mgr, mock := makeConnectedPair(t)
	mock.expectErr("cowboy:boom", "boom_reason")

	go func() {
		for {
			if err := mock.serveOne(); err != nil {
				return
			}
		}
	}()

	_, err := mgr.Call("cowboy:boom", nil)
	if err == nil {
		t.Fatal("expected error, got nil")
	}
	mgr.Stop()
}

func TestCall_UnknownFunction(t *testing.T) {
	mgr, mock := makeConnectedPair(t)

	go func() {
		for {
			if err := mock.serveOne(); err != nil {
				return
			}
		}
	}()

	_, err := mgr.Call("cowboy:nonexistent", nil)
	if err == nil {
		t.Fatal("expected error for unknown function")
	}
	mgr.Stop()
}

func TestCall_ConcurrentCalls(t *testing.T) {
	mgr, mock := makeConnectedPair(t)

	const n = 8
	for i := 0; i < n; i++ {
		mock.expect("fn", int64(i+100))
	}

	// Serve n requests in background.
	done := make(chan struct{})
	go func() {
		defer close(done)
		for i := 0; i < n; i++ {
			if err := mock.serveOne(); err != nil {
				return
			}
		}
	}()

	var wg sync.WaitGroup
	wg.Add(n)
	errs := make([]error, n)
	for i := 0; i < n; i++ {
		i := i
		go func() {
			defer wg.Done()
			_, err := mgr.Call("fn", []interface{}{int64(i)})
			errs[i] = err
		}()
	}
	wg.Wait()

	for i, err := range errs {
		if err != nil {
			t.Errorf("call %d: %v", i, err)
		}
	}

	mgr.Stop()
	<-done
}

func TestCall_Timeout(t *testing.T) {
	// Use a very short timeout; the mock never responds so Call must time out.
	goR, goW := io.Pipe()
	erlR, erlW := io.Pipe()

	mgr := StartWithPipes(erlR, goW, 50*time.Millisecond)

	// Drain the write side so writePacket doesn't block.
	go func() {
		var buf [4096]byte
		for {
			if _, err := goR.Read(buf[:]); err != nil {
				return
			}
		}
	}()

	_, err := mgr.Call("slow:fn", nil)
	if err == nil {
		t.Fatal("expected timeout error")
	}
	mgr.Stop()
	_ = erlW.Close()
	_ = goR.Close()
}

func TestStop_IdempotentAndClean(t *testing.T) {
	mgr, mock := makeConnectedPair(t)
	_ = mock

	if err := mgr.Stop(); err != nil {
		t.Errorf("first Stop: %v", err)
	}
	if err := mgr.Stop(); err != nil {
		t.Errorf("second Stop (idempotent): %v", err)
	}
}

func TestCall_AfterStop(t *testing.T) {
	mgr, mock := makeConnectedPair(t)
	_ = mock

	mgr.Stop()
	_, err := mgr.Call("any:fn", nil)
	if err == nil {
		t.Fatal("expected error after Stop")
	}
}

func TestWritePacket_ETFRoundtrip(t *testing.T) {
	// Verify that writePacket + readLoop correctly reconstructs complex tuples.
	mgr, mock := makeConnectedPair(t)

	want := etf.Tuple{
		etf.Atom("reply"),
		int64(99),
		etf.Tuple{etf.Atom("ok"), []byte("hello")},
	}
	mock.expect("complex:fn", want[2]) // rule returns the inner tuple

	go func() {
		for {
			if err := mock.serveOne(); err != nil {
				return
			}
		}
	}()

	result, err := mgr.Call("complex:fn", nil)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	inner, ok := result.(etf.Tuple)
	if !ok || len(inner) < 2 {
		t.Fatalf("unexpected result type %T: %v", result, result)
	}
	if inner[0].(etf.Atom) != "ok" {
		t.Errorf("inner[0] = %v, want 'ok'", inner[0])
	}
	mgr.Stop()
}

func TestReadLoop_MalformedPayload(t *testing.T) {
	// Inject a packet with garbage payload; the readLoop should skip it and
	// not crash. A subsequent valid reply should still be dispatched.
	goR, goW := io.Pipe()
	erlR, erlW := io.Pipe()

	mgr := StartWithPipes(erlR, goW, 2*time.Second)
	mock := newMockErlang(goR, erlW)
	mock.expect("ok:fn", int64(7))

	go func() {
		// Write a garbage packet (not valid ETF).
		garbage := []byte{0x00, 0x01, 0x02} // not 0x83 prefix
		var hdr [4]byte
		binary.BigEndian.PutUint32(hdr[:], uint32(len(garbage)))
		_, _ = erlW.Write(hdr[:])
		_, _ = erlW.Write(garbage)

		// Serve requests until the manager stops.
		for {
			if err := mock.serveOne(); err != nil {
				return
			}
		}
	}()

	result, err := mgr.Call("ok:fn", nil)
	if err != nil {
		t.Fatalf("unexpected error after garbage packet: %v", err)
	}
	if result.(int64) != 7 {
		t.Errorf("result = %v, want 7", result)
	}
	mgr.Stop()
}
