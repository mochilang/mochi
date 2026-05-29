package otp

import (
	"fmt"
	"strings"
	"testing"

	"mochi/package3/erlang/etf"
)

// mockCaller records calls and returns pre-configured responses.
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

// ── helpers ──────────────────────────────────────────────────────────────────

func TestIsAtom(t *testing.T) {
	if !isAtom(etf.Atom("ok"), "ok") {
		t.Error("isAtom: ok should match ok")
	}
	if isAtom(etf.Atom("ok"), "error") {
		t.Error("isAtom: ok should not match error")
	}
	if isAtom("not-an-atom", "ok") {
		t.Error("isAtom: string should not match")
	}
}

func TestExpectOK_OK(t *testing.T) {
	if err := expectOK(etf.Atom("ok")); err != nil {
		t.Errorf("unexpected: %v", err)
	}
}

func TestExpectOK_Error(t *testing.T) {
	err := expectOK(etf.Tuple{etf.Atom("error"), etf.Atom("noproc")})
	if err == nil {
		t.Error("expected error")
	}
}

func TestExpectOK_Unknown(t *testing.T) {
	err := expectOK(etf.Atom("something_else"))
	if err == nil {
		t.Error("expected error for unknown reply")
	}
}

func TestUnwrapOKValue_Success(t *testing.T) {
	val, err := unwrapOKValue(etf.Tuple{etf.Atom("ok"), etf.Atom("value")})
	if err != nil {
		t.Fatalf("unexpected: %v", err)
	}
	if !isAtom(val, "value") {
		t.Errorf("val = %v", val)
	}
}

func TestUnwrapOKValue_Error(t *testing.T) {
	_, err := unwrapOKValue(etf.Tuple{etf.Atom("error"), etf.Atom("badarg")})
	if err == nil || !strings.Contains(err.Error(), "badarg") {
		t.Errorf("expected badarg error, got: %v", err)
	}
}

func TestUnwrapOKValue_NotTuple(t *testing.T) {
	_, err := unwrapOKValue(etf.Atom("ok"))
	if err == nil {
		t.Error("expected error for bare atom")
	}
}

// ── GenServer ─────────────────────────────────────────────────────────────────

func TestGenServer_Call_Success(t *testing.T) {
	mock := newMock(etf.Atom("pong"))
	gs := NewGenServer(mock, "my_server")
	v, err := gs.Call(etf.Atom("ping"))
	if err != nil {
		t.Fatalf("Call: %v", err)
	}
	if !isAtom(v, "pong") {
		t.Errorf("reply = %v", v)
	}
	if mock.calls[0].fun != "gen_server:call" {
		t.Errorf("fun = %q", mock.calls[0].fun)
	}
}

func TestGenServer_Call_PassesServerName(t *testing.T) {
	mock := newMock(etf.Atom("ok"))
	gs := NewGenServer(mock, "counter")
	_, _ = gs.Call(etf.Atom("get"))
	if len(mock.calls) == 0 {
		t.Fatal("no calls recorded")
	}
	args := mock.calls[0].args
	if len(args) < 1 {
		t.Fatal("no args")
	}
	if !isAtom(args[0], "counter") {
		t.Errorf("first arg = %v, want atom counter", args[0])
	}
}

func TestGenServer_CallTimeout(t *testing.T) {
	mock := newMock(etf.Atom("result"))
	gs := NewGenServer(mock, "srv")
	v, err := gs.CallTimeout(etf.Atom("req"), 10000)
	if err != nil {
		t.Fatalf("CallTimeout: %v", err)
	}
	if !isAtom(v, "result") {
		t.Errorf("reply = %v", v)
	}
	args := mock.calls[0].args
	if len(args) != 3 {
		t.Errorf("expected 3 args (server, request, timeout), got %d", len(args))
	}
}

func TestGenServer_Cast_Success(t *testing.T) {
	mock := newMock(etf.Atom("ok"))
	gs := NewGenServer(mock, "worker")
	if err := gs.Cast(etf.Atom("do_work")); err != nil {
		t.Errorf("Cast: %v", err)
	}
	if mock.calls[0].fun != "gen_server:cast" {
		t.Errorf("fun = %q", mock.calls[0].fun)
	}
}

func TestGenServer_Cast_Error(t *testing.T) {
	mock := newMock(fmt.Errorf("noproc"))
	gs := NewGenServer(mock, "dead_server")
	if err := gs.Cast(etf.Atom("msg")); err == nil {
		t.Error("expected error")
	}
}

func TestGenServer_Stop(t *testing.T) {
	mock := newMock(etf.Atom("ok"))
	gs := NewGenServer(mock, "srv")
	if err := gs.Stop(); err != nil {
		t.Errorf("Stop: %v", err)
	}
	if mock.calls[0].fun != "gen_server:stop" {
		t.Errorf("fun = %q", mock.calls[0].fun)
	}
}

func TestStartLink_Success(t *testing.T) {
	pid := etf.Pid{Node: "nonode@nohost", ID: 1, Serial: 0, Creation: 0}
	mock := newMock(etf.Tuple{etf.Atom("ok"), pid})
	got, err := StartLink(mock, "my_module", "my_server", nil)
	if err != nil {
		t.Fatalf("StartLink: %v", err)
	}
	if got.ID != pid.ID {
		t.Errorf("pid ID = %d, want %d", got.ID, pid.ID)
	}
	if mock.calls[0].fun != "gen_server:start_link" {
		t.Errorf("fun = %q", mock.calls[0].fun)
	}
}

func TestStartLink_Error(t *testing.T) {
	mock := newMock(etf.Tuple{etf.Atom("error"), etf.Atom("already_started")})
	_, err := StartLink(mock, "mod", "srv", nil)
	if err == nil {
		t.Error("expected error")
	}
}

func TestStartLink_NonPidReply(t *testing.T) {
	mock := newMock(etf.Tuple{etf.Atom("ok"), etf.Atom("not_a_pid")})
	_, err := StartLink(mock, "mod", "srv", nil)
	if err == nil {
		t.Error("expected error for non-pid reply")
	}
}

// ── Supervisor ────────────────────────────────────────────────────────────────

func makeChildTuple(id string, pid etf.Pid, typ string) etf.Tuple {
	return etf.Tuple{
		etf.Atom(id),
		pid,
		etf.Atom(typ),
		etf.List{etf.Atom(id + "_mod")},
	}
}

func TestSupervisor_WhichChildren(t *testing.T) {
	pid1 := etf.Pid{Node: "n@h", ID: 10}
	pid2 := etf.Pid{Node: "n@h", ID: 11}
	list := etf.List{
		makeChildTuple("child1", pid1, "worker"),
		makeChildTuple("child2", pid2, "supervisor"),
	}
	mock := newMock(list)
	sup := NewSupervisor(mock, "my_sup")
	children, err := sup.WhichChildren()
	if err != nil {
		t.Fatalf("WhichChildren: %v", err)
	}
	if len(children) != 2 {
		t.Fatalf("len = %d, want 2", len(children))
	}
	if termToString(children[0].ID) != "child1" {
		t.Errorf("child[0].ID = %v", children[0].ID)
	}
	if children[0].PID == nil || children[0].PID.ID != 10 {
		t.Errorf("child[0].PID = %v", children[0].PID)
	}
	if children[0].Type != "worker" {
		t.Errorf("child[0].Type = %q", children[0].Type)
	}
	if children[1].Type != "supervisor" {
		t.Errorf("child[1].Type = %q", children[1].Type)
	}
}

func TestSupervisor_WhichChildren_UndefinedPID(t *testing.T) {
	item := etf.Tuple{
		etf.Atom("child1"),
		etf.Atom("undefined"),
		etf.Atom("worker"),
		etf.List{},
	}
	mock := newMock(etf.List{item})
	sup := NewSupervisor(mock, "sup")
	children, err := sup.WhichChildren()
	if err != nil {
		t.Fatalf("WhichChildren: %v", err)
	}
	if len(children) != 1 {
		t.Fatalf("len = %d", len(children))
	}
	if children[0].PID != nil {
		t.Error("PID should be nil for undefined child")
	}
}

func TestSupervisor_WhichChildren_NonList(t *testing.T) {
	mock := newMock(etf.Atom("badarg"))
	sup := NewSupervisor(mock, "sup")
	_, err := sup.WhichChildren()
	if err == nil {
		t.Error("expected error for non-list reply")
	}
}

func TestSupervisor_CountChildren(t *testing.T) {
	list := etf.List{
		etf.Tuple{etf.Atom("specs"), int64(3)},
		etf.Tuple{etf.Atom("active"), int64(2)},
		etf.Tuple{etf.Atom("supervisors"), int64(1)},
		etf.Tuple{etf.Atom("workers"), int64(1)},
	}
	mock := newMock(list)
	sup := NewSupervisor(mock, "sup")
	cc, err := sup.CountChildren()
	if err != nil {
		t.Fatalf("CountChildren: %v", err)
	}
	if cc.Specs != 3 {
		t.Errorf("Specs = %d", cc.Specs)
	}
	if cc.Active != 2 {
		t.Errorf("Active = %d", cc.Active)
	}
	if cc.Supervisors != 1 {
		t.Errorf("Supervisors = %d", cc.Supervisors)
	}
	if cc.Workers != 1 {
		t.Errorf("Workers = %d", cc.Workers)
	}
}

func TestSupervisor_TerminateChild_OK(t *testing.T) {
	mock := newMock(etf.Atom("ok"))
	sup := NewSupervisor(mock, "sup")
	if err := sup.TerminateChild(etf.Atom("child1")); err != nil {
		t.Errorf("TerminateChild: %v", err)
	}
	if mock.calls[0].fun != "supervisor:terminate_child" {
		t.Errorf("fun = %q", mock.calls[0].fun)
	}
}

func TestSupervisor_TerminateChild_Error(t *testing.T) {
	mock := newMock(etf.Tuple{etf.Atom("error"), etf.Atom("not_found")})
	sup := NewSupervisor(mock, "sup")
	err := sup.TerminateChild(etf.Atom("ghost"))
	if err == nil {
		t.Error("expected error")
	}
}

func TestSupervisor_RestartChild_OK(t *testing.T) {
	pid := etf.Pid{Node: "n@h", ID: 42}
	mock := newMock(etf.Tuple{etf.Atom("ok"), pid})
	sup := NewSupervisor(mock, "sup")
	got, err := sup.RestartChild(etf.Atom("worker1"))
	if err != nil {
		t.Fatalf("RestartChild: %v", err)
	}
	if got.ID != 42 {
		t.Errorf("pid.ID = %d", got.ID)
	}
}

func TestSupervisor_RestartChild_Error(t *testing.T) {
	mock := newMock(etf.Tuple{etf.Atom("error"), etf.Atom("running")})
	sup := NewSupervisor(mock, "sup")
	_, err := sup.RestartChild(etf.Atom("c1"))
	if err == nil {
		t.Error("expected error")
	}
}

func TestSupervisor_DeleteChild(t *testing.T) {
	mock := newMock(etf.Atom("ok"))
	sup := NewSupervisor(mock, "sup")
	if err := sup.DeleteChild(etf.Atom("child1")); err != nil {
		t.Errorf("DeleteChild: %v", err)
	}
	if mock.calls[0].fun != "supervisor:delete_child" {
		t.Errorf("fun = %q", mock.calls[0].fun)
	}
}

// ── Application ──────────────────────────────────────────────────────────────

func TestApplication_Start_OK(t *testing.T) {
	mock := newMock(etf.Atom("ok"))
	app := NewApplication(mock, "cowboy")
	if err := app.Start(); err != nil {
		t.Errorf("Start: %v", err)
	}
	if mock.calls[0].fun != "application:start" {
		t.Errorf("fun = %q", mock.calls[0].fun)
	}
}

func TestApplication_Start_AlreadyStarted(t *testing.T) {
	reason := etf.Tuple{etf.Atom("already_started"), etf.Atom("cowboy")}
	mock := newMock(etf.Tuple{etf.Atom("error"), reason})
	app := NewApplication(mock, "cowboy")
	if err := app.Start(); err != nil {
		t.Errorf("already_started should not be an error: %v", err)
	}
}

func TestApplication_Start_OtherError(t *testing.T) {
	mock := newMock(etf.Tuple{etf.Atom("error"), etf.Atom("bad_return")})
	app := NewApplication(mock, "cowboy")
	if err := app.Start(); err == nil {
		t.Error("expected error for bad_return")
	}
}

func TestApplication_StartPermanent(t *testing.T) {
	mock := newMock(etf.Atom("ok"))
	app := NewApplication(mock, "myapp")
	if err := app.StartPermanent(); err != nil {
		t.Errorf("StartPermanent: %v", err)
	}
	args := mock.calls[0].args
	if len(args) < 2 || !isAtom(args[1], "permanent") {
		t.Errorf("second arg should be atom permanent, got: %v", args)
	}
}

func TestApplication_Stop(t *testing.T) {
	mock := newMock(etf.Atom("ok"))
	app := NewApplication(mock, "cowboy")
	if err := app.Stop(); err != nil {
		t.Errorf("Stop: %v", err)
	}
	if mock.calls[0].fun != "application:stop" {
		t.Errorf("fun = %q", mock.calls[0].fun)
	}
}

func TestApplication_EnsureAllStarted(t *testing.T) {
	list := etf.List{etf.Atom("ranch"), etf.Atom("cowlib"), etf.Atom("cowboy")}
	mock := newMock(etf.Tuple{etf.Atom("ok"), list})
	app := NewApplication(mock, "cowboy")
	apps, err := app.EnsureAllStarted()
	if err != nil {
		t.Fatalf("EnsureAllStarted: %v", err)
	}
	if len(apps) != 3 {
		t.Fatalf("len = %d", len(apps))
	}
	if apps[0] != "ranch" || apps[2] != "cowboy" {
		t.Errorf("apps = %v", apps)
	}
}

func TestApplication_EnsureAllStarted_Error(t *testing.T) {
	mock := newMock(etf.Tuple{etf.Atom("error"), etf.Atom("missing_dep")})
	app := NewApplication(mock, "myapp")
	_, err := app.EnsureAllStarted()
	if err == nil {
		t.Error("expected error")
	}
}

func TestApplication_GetEnv_Present(t *testing.T) {
	mock := newMock(etf.Tuple{etf.Atom("ok"), int64(8080)})
	app := NewApplication(mock, "myapp")
	val, ok, err := app.GetEnv("port")
	if err != nil {
		t.Fatalf("GetEnv: %v", err)
	}
	if !ok {
		t.Error("expected ok=true")
	}
	n, _ := termToInt64(val)
	if n != 8080 {
		t.Errorf("val = %v", val)
	}
}

func TestApplication_GetEnv_Undefined(t *testing.T) {
	mock := newMock(etf.Atom("undefined"))
	app := NewApplication(mock, "myapp")
	_, ok, err := app.GetEnv("missing_key")
	if err != nil {
		t.Fatalf("GetEnv: %v", err)
	}
	if ok {
		t.Error("expected ok=false for undefined")
	}
}

func TestApplication_GetEnv_PassesAppAndKey(t *testing.T) {
	mock := newMock(etf.Atom("undefined"))
	app := NewApplication(mock, "ranch")
	_, _, _ = app.GetEnv("max_connections")
	if mock.calls[0].fun != "application:get_env" {
		t.Errorf("fun = %q", mock.calls[0].fun)
	}
	args := mock.calls[0].args
	if !isAtom(args[0], "ranch") || !isAtom(args[1], "max_connections") {
		t.Errorf("args = %v", args)
	}
}

func TestApplication_SetEnv(t *testing.T) {
	mock := newMock(etf.Atom("ok"))
	app := NewApplication(mock, "myapp")
	if err := app.SetEnv("debug", etf.Atom("true")); err != nil {
		t.Errorf("SetEnv: %v", err)
	}
	if mock.calls[0].fun != "application:set_env" {
		t.Errorf("fun = %q", mock.calls[0].fun)
	}
}

func TestLoadedApplications(t *testing.T) {
	list := etf.List{
		etf.Tuple{etf.Atom("kernel"), etf.Atom("ERTS kernel"), etf.Atom("9.0")},
		etf.Tuple{etf.Atom("stdlib"), etf.Atom("ERTS stdlib"), etf.Atom("5.0")},
	}
	mock := newMock(list)
	apps, err := LoadedApplications(mock)
	if err != nil {
		t.Fatalf("LoadedApplications: %v", err)
	}
	if len(apps) != 2 || apps[0] != "kernel" {
		t.Errorf("apps = %v", apps)
	}
}

func TestWhichApplications(t *testing.T) {
	list := etf.List{
		etf.Tuple{etf.Atom("cowboy"), etf.Atom("HTTP server"), etf.Atom("2.12.0")},
	}
	mock := newMock(list)
	apps, err := WhichApplications(mock)
	if err != nil {
		t.Fatalf("WhichApplications: %v", err)
	}
	if len(apps) != 1 || apps[0] != "cowboy" {
		t.Errorf("apps = %v", apps)
	}
}

func TestWhichApplications_UsesWhichFun(t *testing.T) {
	mock := newMock(etf.List{})
	_, _ = WhichApplications(mock)
	if mock.calls[0].fun != "application:which_applications" {
		t.Errorf("fun = %q", mock.calls[0].fun)
	}
}
