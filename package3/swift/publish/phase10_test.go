package publish_test

import (
	"context"
	"net/http"
	"net/http/httptest"
	"os"
	"testing"

	"mochi/package3/swift/publish"
)

// TestPhase10Publish is the MEP-69 Phase 10 gate for Swift package publishing.
// Requires MOCHI_SWIFT_PUBLISH_TEST=1 for the integration path.
func TestPhase10Publish(t *testing.T) {
	if os.Getenv("MOCHI_SWIFT_PUBLISH_TEST") == "" {
		t.Skip("set MOCHI_SWIFT_PUBLISH_TEST=1 to run")
	}
	t.Log("integration test placeholder")
}

func TestPhase10PublishGitTagValidation(t *testing.T) {
	ctx := context.Background()

	t.Run("EmptyRepoDir", func(t *testing.T) {
		if err := publish.PublishGitTag(ctx, "", "1.0.0"); err == nil {
			t.Error("expected error for empty repoDir")
		}
	})

	t.Run("EmptyVersion", func(t *testing.T) {
		if err := publish.PublishGitTag(ctx, "/tmp", ""); err == nil {
			t.Error("expected error for empty version")
		}
	})
}

func TestPhase10PublishSE0292(t *testing.T) {
	// Verify function signatures.
	var _ func(context.Context, publish.Registry, string, string, string) error = publish.PublishSE0292

	t.Run("EmptyBaseURL", func(t *testing.T) {
		ctx := context.Background()
		reg := publish.Registry{}
		if err := publish.PublishSE0292(ctx, reg, "scope.pkg", "1.0.0", "/tmp/archive.zip"); err == nil {
			t.Error("expected error for empty BaseURL")
		}
	})

	t.Run("BadIDFormat", func(t *testing.T) {
		ctx := context.Background()
		reg := publish.Registry{BaseURL: "https://registry.example.com"}
		if err := publish.PublishSE0292(ctx, reg, "noscope", "1.0.0", "/tmp/archive.zip"); err == nil {
			t.Error("expected error for id without scope")
		}
	})

	t.Run("MockServerSuccess", func(t *testing.T) {
		srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			w.WriteHeader(http.StatusCreated)
		}))
		defer srv.Close()

		// Create a temp archive file.
		f, err := os.CreateTemp(t.TempDir(), "archive*.zip")
		if err != nil {
			t.Fatalf("create temp: %v", err)
		}
		_, _ = f.WriteString("fake zip content")
		f.Close()

		ctx := context.Background()
		reg := publish.Registry{BaseURL: srv.URL, HTTP: srv.Client()}
		if err := publish.PublishSE0292(ctx, reg, "myorg.mypkg", "1.0.0", f.Name()); err != nil {
			t.Errorf("PublishSE0292: %v", err)
		}
	})
}

func TestPhase10OIDCMintToken(t *testing.T) {
	// Verify function signature.
	var _ func(context.Context, string) (string, error) = publish.OIDCMintToken

	// Without env vars set, should return an error.
	ctx := context.Background()
	_, err := publish.OIDCMintToken(ctx, "swift-registry")
	if err == nil {
		t.Error("expected error when ACTIONS_ID_TOKEN_REQUEST_URL is not set")
	}
}
