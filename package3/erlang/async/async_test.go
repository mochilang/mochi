package async

import (
	"fmt"
	"strings"
	"testing"
	"time"

	"mochi/package3/erlang/etf"
)

// ── mock Caller ───────────────────────────────────────────────────────────────

type mockCaller struct {
	responses []interface{}
	idx       int
	calls     []mockCall
}

type mockCall struct {
	fun  string
	args []interface{}
}

func (m *mockCaller) Call(fun string, args []interface{}) (interface{}, error) {
	m.calls = append(m.calls, mockCall{fun: fun, args: args})
	if m.idx >= len(m.responses) {
		return nil, fmt.Errorf("unexpected call %d to %s", m.idx, fun)
	}
	r := m.responses[m.idx]
	m.idx++
	if err, ok := r.(error); ok {
		return nil, err
	}
	return r, nil
}

func newMock(responses ...interface{}) *mockCaller {
	return &mockCaller{responses: responses}
}

func fakePid(id uint32) etf.Pid {
	return etf.Pid{Node: "nonode@nohost", ID: id}
}

func fakeRef() etf.Reference {
	return etf.Reference{Node: "nonode@nohost", Creation: 1, IDs: []uint32{1, 0, 0}}
}

// ── helpers ───────────────────────────────────────────────────────────────────

func TestIsAtom(t *testing.T) {
	if !isAtom(etf.Atom("ok"), "ok") {
		t.Error("expected ok match")
	}
	if isAtom("ok", "ok") {
		t.Error("string should not match atom")
	}
}

func TestExpectOK_OK(t *testing.T) {
	if err := expectOK(etf.Atom("ok")); err != nil {
		t.Errorf("unexpected: %v", err)
	}
}

func TestExpectOK_Error(t *testing.T) {
	err := expectOK(etf.Tuple{etf.Atom("error"), etf.Atom("noproc")})
	if err == nil || !strings.Contains(err.Error(), "noproc") {
		t.Errorf("expected noproc error, got: %v", err)
	}
}

// ── process operations ────────────────────────────────────────────────────────

func TestSpawn_Success(t *testing.T) {
	pid := fakePid(42)
	mock := newMock(pid)
	got, err := Spawn(mock, "mymod", "myfun", nil)
	if err != nil {
		t.Fatalf("Spawn: %v", err)
	}
	if got.ID != 42 {
		t.Errorf("pid.ID = %d", got.ID)
	}
	if mock.calls[0].fun != "erlang:spawn" {
		t.Errorf("fun = %q", mock.calls[0].fun)
	}
}

func TestSpawn_NonPidReply(t *testing.T) {
	mock := newMock(etf.Atom("error"))
	_, err := Spawn(mock, "m", "f", nil)
	if err == nil {
		t.Error("expected error for non-pid reply")
	}
}

func TestSpawn_PassesModFunArgs(t *testing.T) {
	mock := newMock(fakePid(1))
	_, _ = Spawn(mock, "counter", "start", []interface{}{int64(0)})
	args := mock.calls[0].args
	if !isAtom(args[0], "counter") {
		t.Errorf("module arg = %v", args[0])
	}
	if !isAtom(args[1], "start") {
		t.Errorf("fun arg = %v", args[1])
	}
}

func TestSpawnLink(t *testing.T) {
	pid := fakePid(7)
	mock := newMock(pid)
	got, err := SpawnLink(mock, "m", "f", nil)
	if err != nil {
		t.Fatalf("SpawnLink: %v", err)
	}
	if got.ID != 7 {
		t.Errorf("pid.ID = %d", got.ID)
	}
	if mock.calls[0].fun != "erlang:spawn_link" {
		t.Errorf("fun = %q", mock.calls[0].fun)
	}
}

func TestSpawnMonitor_Success(t *testing.T) {
	pid := fakePid(5)
	ref := fakeRef()
	mock := newMock(etf.Tuple{pid, ref})
	gotPid, gotRef, err := SpawnMonitor(mock, "m", "f", nil)
	if err != nil {
		t.Fatalf("SpawnMonitor: %v", err)
	}
	if gotPid.ID != 5 {
		t.Errorf("pid.ID = %d", gotPid.ID)
	}
	if gotRef.Creation != ref.Creation {
		t.Errorf("ref mismatch")
	}
}

func TestSpawnMonitor_BadShape(t *testing.T) {
	mock := newMock(etf.Atom("bad"))
	_, _, err := SpawnMonitor(mock, "m", "f", nil)
	if err == nil {
		t.Error("expected error")
	}
}

func TestSend_Success(t *testing.T) {
	mock := newMock(etf.Atom("hello"))
	pid := fakePid(3)
	if err := Send(mock, pid, etf.Atom("hello")); err != nil {
		t.Errorf("Send: %v", err)
	}
	if mock.calls[0].fun != "erlang:send" {
		t.Errorf("fun = %q", mock.calls[0].fun)
	}
}

func TestSend_TransportError(t *testing.T) {
	mock := newMock(fmt.Errorf("closed"))
	if err := Send(mock, fakePid(1), etf.Atom("msg")); err == nil {
		t.Error("expected error on transport failure")
	}
}

func TestSendNamed(t *testing.T) {
	mock := newMock(etf.Atom("msg"))
	if err := SendNamed(mock, "my_server", etf.Atom("msg")); err != nil {
		t.Errorf("SendNamed: %v", err)
	}
	args := mock.calls[0].args
	if !isAtom(args[0], "my_server") {
		t.Errorf("first arg = %v", args[0])
	}
}

func TestExit_OK(t *testing.T) {
	mock := newMock(etf.Atom("ok"))
	if err := Exit(mock, fakePid(2), etf.Atom("kill")); err != nil {
		t.Errorf("Exit: %v", err)
	}
	if mock.calls[0].fun != "erlang:exit" {
		t.Errorf("fun = %q", mock.calls[0].fun)
	}
}

func TestIsAlive_True(t *testing.T) {
	mock := newMock(etf.Atom("true"))
	alive, err := IsAlive(mock, fakePid(1))
	if err != nil {
		t.Fatalf("IsAlive: %v", err)
	}
	if !alive {
		t.Error("expected alive=true")
	}
}

func TestIsAlive_False(t *testing.T) {
	mock := newMock(etf.Atom("false"))
	alive, err := IsAlive(mock, fakePid(999))
	if err != nil {
		t.Fatalf("IsAlive: %v", err)
	}
	if alive {
		t.Error("expected alive=false")
	}
}

func TestIsAlive_UnexpectedReply(t *testing.T) {
	mock := newMock(etf.Atom("maybe"))
	_, err := IsAlive(mock, fakePid(1))
	if err == nil {
		t.Error("expected error for unexpected reply")
	}
}

func TestProcessInfo_Present(t *testing.T) {
	mock := newMock(etf.Tuple{etf.Atom("status"), etf.Atom("running")})
	val, ok, err := ProcessInfo(mock, fakePid(1), "status")
	if err != nil {
		t.Fatalf("ProcessInfo: %v", err)
	}
	if !ok {
		t.Error("expected ok=true")
	}
	if !isAtom(val, "running") {
		t.Errorf("val = %v", val)
	}
}

func TestProcessInfo_Undefined(t *testing.T) {
	mock := newMock(etf.Atom("undefined"))
	_, ok, err := ProcessInfo(mock, fakePid(1), "status")
	if err != nil {
		t.Fatalf("ProcessInfo: %v", err)
	}
	if ok {
		t.Error("expected ok=false for undefined")
	}
}

func TestSelf(t *testing.T) {
	pid := fakePid(99)
	mock := newMock(pid)
	got, err := Self(mock)
	if err != nil {
		t.Fatalf("Self: %v", err)
	}
	if got.ID != 99 {
		t.Errorf("pid.ID = %d", got.ID)
	}
	if mock.calls[0].fun != "erlang:self" {
		t.Errorf("fun = %q", mock.calls[0].fun)
	}
}

// ── monitor operations ────────────────────────────────────────────────────────

func TestMonitor_Success(t *testing.T) {
	ref := fakeRef()
	mock := newMock(ref)
	got, err := Monitor(mock, fakePid(5))
	if err != nil {
		t.Fatalf("Monitor: %v", err)
	}
	if got.Creation != ref.Creation {
		t.Errorf("ref mismatch")
	}
	if mock.calls[0].fun != "erlang:monitor" {
		t.Errorf("fun = %q", mock.calls[0].fun)
	}
}

func TestMonitor_PassesProcessAtom(t *testing.T) {
	mock := newMock(fakeRef())
	_, _ = Monitor(mock, fakePid(1))
	args := mock.calls[0].args
	if !isAtom(args[0], "process") {
		t.Errorf("first arg should be atom 'process', got: %v", args[0])
	}
}

func TestMonitor_NonRefReply(t *testing.T) {
	mock := newMock(etf.Atom("bad"))
	_, err := Monitor(mock, fakePid(1))
	if err == nil {
		t.Error("expected error for non-ref reply")
	}
}

func TestDemonitor_OK(t *testing.T) {
	mock := newMock(etf.Atom("true"))
	if err := Demonitor(mock, fakeRef()); err != nil {
		t.Errorf("Demonitor: %v", err)
	}
	if mock.calls[0].fun != "erlang:demonitor" {
		t.Errorf("fun = %q", mock.calls[0].fun)
	}
}

func TestDemonitor_PassesFlushOption(t *testing.T) {
	mock := newMock(etf.Atom("true"))
	_ = Demonitor(mock, fakeRef())
	args := mock.calls[0].args
	if len(args) < 2 {
		t.Fatal("expected 2 args: ref and options")
	}
	opts, ok := args[1].(etf.List)
	if !ok || len(opts) == 0 {
		t.Errorf("options should be non-empty list, got: %v", args[1])
	}
	if !isAtom(opts[0], "flush") {
		t.Errorf("flush option missing, got: %v", opts[0])
	}
}

func TestLink(t *testing.T) {
	mock := newMock(etf.Atom("true"))
	if err := Link(mock, fakePid(3)); err != nil {
		t.Errorf("Link: %v", err)
	}
	if mock.calls[0].fun != "erlang:link" {
		t.Errorf("fun = %q", mock.calls[0].fun)
	}
}

func TestUnlink(t *testing.T) {
	mock := newMock(etf.Atom("true"))
	if err := Unlink(mock, fakePid(3)); err != nil {
		t.Errorf("Unlink: %v", err)
	}
	if mock.calls[0].fun != "erlang:unlink" {
		t.Errorf("fun = %q", mock.calls[0].fun)
	}
}

func TestParseDownMessage_Valid(t *testing.T) {
	ref := fakeRef()
	pid := fakePid(10)
	term := etf.Tuple{
		etf.Atom("DOWN"),
		ref,
		etf.Atom("process"),
		pid,
		etf.Atom("normal"),
	}
	msg, ok := ParseDownMessage(term)
	if !ok {
		t.Fatal("expected ok=true")
	}
	if msg.Pid.ID != 10 {
		t.Errorf("pid.ID = %d", msg.Pid.ID)
	}
	if !isAtom(msg.Reason, "normal") {
		t.Errorf("Reason = %v", msg.Reason)
	}
}

func TestParseDownMessage_WrongTag(t *testing.T) {
	term := etf.Tuple{etf.Atom("UP"), fakeRef(), etf.Atom("process"), fakePid(1), etf.Atom("normal")}
	_, ok := ParseDownMessage(term)
	if ok {
		t.Error("should not parse non-DOWN tuple")
	}
}

func TestParseDownMessage_TooShort(t *testing.T) {
	_, ok := ParseDownMessage(etf.Tuple{etf.Atom("DOWN")})
	if ok {
		t.Error("should not parse short tuple")
	}
}

// ── Mailbox ───────────────────────────────────────────────────────────────────

func TestMailbox_Push(t *testing.T) {
	ch := make(chan interface{}, 10)
	mb := NewMailboxFromChan(ch)
	mb.Push(etf.Atom("hello"))
	mb.Push(etf.Atom("world"))
	if len(mb.Chan()) != 2 {
		t.Errorf("expected 2 messages, got %d", len(mb.Chan()))
	}
	msg := <-mb.Chan()
	if !isAtom(msg, "hello") {
		t.Errorf("msg = %v", msg)
	}
}

func TestMailbox_Close(t *testing.T) {
	ch := make(chan interface{}, 4)
	mb := NewMailboxFromChan(ch)
	mb.Push(etf.Atom("msg"))
	mb.Close()
	// Channel should be closed after Close.
	_, open := <-mb.Chan()
	if open {
		// The message is still in the buffer.
	}
	// Double-close should not panic.
	mb.Close()
}

func TestMailbox_Poll(t *testing.T) {
	// Simulate an Erlang mailbox that returns two messages then empty.
	responses := []interface{}{
		etf.Tuple{etf.Atom("ok"), etf.Atom("msg1")},
		etf.Tuple{etf.Atom("ok"), etf.Atom("msg2")},
		etf.Atom("empty"),
		// subsequent polls return empty
		etf.Atom("empty"),
		etf.Atom("empty"),
		etf.Atom("empty"),
		etf.Atom("empty"),
	}
	mock := &mockCaller{responses: responses}
	mb := NewMailbox(mock, "my_mailbox", 16, 10*time.Millisecond)

	var got []interface{}
	timeout := time.After(500 * time.Millisecond)
	for len(got) < 2 {
		select {
		case msg := <-mb.Chan():
			got = append(got, msg)
		case <-timeout:
			t.Fatal("timed out waiting for 2 messages")
		}
	}

	mb.Close()

	if len(got) != 2 {
		t.Fatalf("got %d messages", len(got))
	}
	if !isAtom(got[0], "msg1") {
		t.Errorf("msg[0] = %v", got[0])
	}
	if !isAtom(got[1], "msg2") {
		t.Errorf("msg[1] = %v", got[1])
	}
}

func TestMailbox_PollCallsCorrectFun(t *testing.T) {
	mock := &mockCaller{responses: []interface{}{
		etf.Atom("empty"),
		etf.Atom("empty"),
	}}
	mb := NewMailbox(mock, "inbox", 4, 10*time.Millisecond)
	time.Sleep(30 * time.Millisecond)
	mb.Close()

	for _, c := range mock.calls {
		if c.fun != "erlang_mailbox:recv" {
			t.Errorf("unexpected fun: %q", c.fun)
		}
		if len(c.args) < 1 || !isAtom(c.args[0], "inbox") {
			t.Errorf("args = %v", c.args)
		}
	}
}
