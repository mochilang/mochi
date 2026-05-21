//go:build !(darwin && arm64) && !(linux && amd64)

package vm3jit

import (
	"errors"
	"testing"
)

// TestPageRefusesOnUnsupportedPlatform is the structurally-secure
// fallback gate. On any OS/arch without a real W^X backend in tree
// (page_stub.go), pageAlloc must return errNoPageBackend so the
// runtime falls back to the interpreter rather than producing an
// unmapped or possibly-RWX page. The test compiles only on stub
// platforms; the darwin/arm64 and linux/amd64 counterparts exercise
// the success-path TestPageWXTransition.
func TestPageRefusesOnUnsupportedPlatform(t *testing.T) {
	if _, err := pageAlloc(64); !errors.Is(err, errNoPageBackend) {
		t.Errorf("pageAlloc on stub platform: got err=%v, want errNoPageBackend", err)
	}
	if err := pageMakeExecOS(nil, nil); !errors.Is(err, errNoPageBackend) {
		t.Errorf("pageMakeExecOS on stub platform: got err=%v, want errNoPageBackend", err)
	}
	if err := pageFree(nil); !errors.Is(err, errNoPageBackend) {
		t.Errorf("pageFree on stub platform: got err=%v, want errNoPageBackend", err)
	}
}
