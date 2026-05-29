package asyncbridge

import (
	"strings"
	"testing"
)

func validFn() AsyncFn {
	return AsyncFn{
		Name:       "fetch",
		SyncName:   "fetch_sync",
		ParamNames: []string{"url"},
		ParamTypes: []string{"str"},
		Return:     "str",
	}
}

func TestAsyncFnValidateOK(t *testing.T) {
	if err := validFn().Validate(); err != nil {
		t.Fatalf("Validate: %v", err)
	}
}

func TestAsyncFnValidateEmptyName(t *testing.T) {
	f := validFn()
	f.Name = ""
	if err := f.Validate(); err == nil || !strings.Contains(err.Error(), "Name") {
		t.Fatalf("expected Name error, got %v", err)
	}
}

func TestAsyncFnValidateEmptySyncName(t *testing.T) {
	f := validFn()
	f.SyncName = ""
	if err := f.Validate(); err == nil || !strings.Contains(err.Error(), "SyncName") {
		t.Fatalf("expected SyncName error, got %v", err)
	}
}

func TestAsyncFnValidateSyncNameEqualsName(t *testing.T) {
	f := validFn()
	f.SyncName = f.Name
	if err := f.Validate(); err == nil || !strings.Contains(err.Error(), "differ") {
		t.Fatalf("expected differ error, got %v", err)
	}
}

func TestAsyncFnValidateParamLenMismatch(t *testing.T) {
	f := validFn()
	f.ParamTypes = []string{"str", "int"}
	if err := f.Validate(); err == nil || !strings.Contains(err.Error(), "param") {
		t.Fatalf("expected param length error, got %v", err)
	}
}

func TestAsyncFnValidateEmptyParamName(t *testing.T) {
	f := validFn()
	f.ParamNames = []string{""}
	f.ParamTypes = []string{"str"}
	if err := f.Validate(); err == nil || !strings.Contains(err.Error(), "empty name") {
		t.Fatalf("expected empty name error, got %v", err)
	}
}

func TestAsyncFnValidateEmptyParamType(t *testing.T) {
	f := validFn()
	f.ParamNames = []string{"x"}
	f.ParamTypes = []string{""}
	if err := f.Validate(); err == nil || !strings.Contains(err.Error(), "empty type") {
		t.Fatalf("expected empty type error, got %v", err)
	}
}

func TestAsyncFnValidateEmptyReturn(t *testing.T) {
	f := validFn()
	f.Return = ""
	if err := f.Validate(); err == nil || !strings.Contains(err.Error(), "Return") {
		t.Fatalf("expected Return error, got %v", err)
	}
}

func TestDefaultSyncName(t *testing.T) {
	if got := DefaultSyncName("fetch"); got != "fetch_sync" {
		t.Fatalf("DefaultSyncName = %q", got)
	}
}
