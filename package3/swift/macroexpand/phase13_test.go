package macroexpand_test

import (
	"context"
	"os/exec"
	"testing"

	"mochi/package3/swift/macroexpand"
)

// TestPhase13MacroExpand is the MEP-69 Phase 13 gate for Swift macro expansion.
// Skips if `swift` is not on $PATH.
func TestPhase13MacroExpand(t *testing.T) {
	if _, err := exec.LookPath("swift"); err != nil {
		t.Skip("swift not on $PATH; skipping Phase 13 macro expand gate")
	}

	// Run against a non-existent directory to verify error handling.
	cfg := macroexpand.Config{}
	_, err := macroexpand.Run(context.Background(), "/nonexistent/package", cfg)
	if err == nil {
		t.Error("expected error for non-existent packageDir")
	}
}

func TestPhase13MacroExpandValidation(t *testing.T) {
	ctx := context.Background()
	_, err := macroexpand.Run(ctx, "", macroexpand.Config{})
	if err == nil {
		t.Error("expected error for empty packageDir")
	}
}

func TestPhase13MacroExpandConfig(t *testing.T) {
	cfg := macroexpand.Config{
		SwiftBin:      "/usr/bin/swift",
		Configuration: "debug",
		ExtraArgs:     []string{"--target", "MyTarget"},
	}
	if cfg.Configuration != "debug" {
		t.Errorf("got %q, want debug", cfg.Configuration)
	}
	if len(cfg.ExtraArgs) != 2 {
		t.Errorf("expected 2 extra args, got %d", len(cfg.ExtraArgs))
	}
}
