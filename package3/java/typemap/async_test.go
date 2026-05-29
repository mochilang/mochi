package typemap_test

import (
	"testing"

	"mochi/package3/java/typemap"
)

func TestNewAsyncMapping_Future(t *testing.T) {
	m, sr, _ := typemap.Map("java.util.concurrent.CompletableFuture<java.lang.String>")
	if sr != 0 {
		t.Fatalf("expected valid mapping; got skip reason %d", sr)
	}
	am := typemap.NewAsyncMapping(m, typemap.AsyncGoroutine)
	if am == nil {
		t.Fatal("NewAsyncMapping returned nil for KindFuture mapping")
	}
	if am.Policy != typemap.AsyncGoroutine {
		t.Errorf("Policy = %v; want AsyncGoroutine", am.Policy)
	}
	if am.ChannelType == "" {
		t.Error("ChannelType is empty")
	}
}

func TestNewAsyncMapping_NonFuture(t *testing.T) {
	m, _, _ := typemap.Map("java.lang.String")
	am := typemap.NewAsyncMapping(m, typemap.AsyncBlocking)
	if am != nil {
		t.Error("expected nil for non-future mapping")
	}
}

func TestNewAsyncMapping_Nil(t *testing.T) {
	am := typemap.NewAsyncMapping(nil, typemap.AsyncBlocking)
	if am != nil {
		t.Error("expected nil for nil mapping")
	}
}

func TestAsyncPolicyString(t *testing.T) {
	if typemap.AsyncBlocking.String() != "blocking" {
		t.Errorf("AsyncBlocking.String() = %q; want blocking", typemap.AsyncBlocking.String())
	}
	if typemap.AsyncGoroutine.String() != "goroutine" {
		t.Errorf("AsyncGoroutine.String() = %q; want goroutine", typemap.AsyncGoroutine.String())
	}
}
