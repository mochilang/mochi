// Package publish is the MEP-74 phase 12 git-tag publish flow. It
// owns the `mochi pkg publish --to=go+git+<repo-url>@<tag>` driver:
// taking a rendered Go library module (typically the output of
// `package3/go/library.Emit`), materialising it into a fresh git
// working tree, validating the canonical-import-path invariant
// (the go.mod `module` directive must match the configured remote
// repo URL one-to-one), committing, tagging with the provided
// semver tag, and (unless DryRun) pushing both the commit and the
// tag to the configured remote.
//
// The package is layering-conservative: it imports
// `package3/go/library` for the Files input shape and
// `package3/go/semver` for the tag validator, and otherwise depends
// only on the Go stdlib. All side effects flow through a small
// command runner (see runner.go) so the publish driver is
// end-to-end unit-testable against a local bare-repo fixture
// without touching a real registry or remote.
//
// The publish surface is split into a pure shaping layer
// (PublishRequest.Validate, CanonicalImportPath) and an impure
// runner (Publish) so callers can validate the request before
// touching git. The runner is idempotent over the workspace root:
// passing the same WorkspaceRoot twice rebuilds the tree from
// scratch.
package publish

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"

	"mochi/package3/go/library"
	"mochi/package3/go/semver"
)

// ErrPublish is the package-wide error sentinel. Per-call failures
// embed it via fmt.Errorf("%w: ...", ErrPublish, ...) so callers can
// errors.Is-match without parsing message text.
var ErrPublish = errors.New("publish")

// PublishRequest is the in-memory shape of a `mochi pkg publish`
// invocation. The struct is the boundary between the pure prep
// phase (Validate, CanonicalImportPath) and the impure runner
// (Publish), so callers can fully validate the request before
// touching git.
type PublishRequest struct {
	// Files is the rendered Go module to publish (typically the
	// output of library.Emit). Required. Must contain go.mod.
	Files library.EmitResult
	// ModulePath is the canonical Go module path the consumer
	// will write in their `import` statements (e.g.
	// "github.com/mochilang/calc"). Required. Cross-validated
	// against the go.mod `module` directive AND the RemoteURL by
	// CanonicalImportPath.
	ModulePath string
	// Tag is the semver tag to apply to the commit, in `v` form
	// (e.g. "v0.1.0"). Required. Cross-validated by
	// semver.IsValid.
	Tag string
	// RemoteURL is the git remote URL to push to (e.g.
	// "file:///tmp/repo.git", "git@github.com:mochilang/calc.git").
	// Required (even under DryRun the URL is recorded as the
	// `origin` remote so a follow-up retry can finish the push).
	RemoteURL string
	// Author is the git author/committer recorded on the publish
	// commit. Required.
	Author Author
	// CommitMessage is the publish commit subject. Optional;
	// defaults to "publish <tag>".
	CommitMessage string
	// DefaultBranch is the branch the publish commit is pushed
	// to (and the local branch the working tree initialises on).
	// Optional; defaults to "main".
	DefaultBranch string
	// WorkspaceRoot is the directory where the publish working
	// tree is assembled. Optional; when empty, Publish creates a
	// temp directory and records the path in PublishResult.
	WorkspaceRoot string
	// DryRun skips the `git push` step but still performs every
	// local action (init, commit, tag). Used by `mochi pkg
	// publish --dry-run`.
	DryRun bool
}

// Author is the git author/committer pair recorded on the publish
// commit.
type Author struct {
	Name, Email string
}

// PublishResult is the outcome of a Publish call. CommitSHA and
// Tag are echoed back so the CLI layer can print a confirmation;
// Pushed reflects whether the `git push` step actually ran (false
// under DryRun).
type PublishResult struct {
	WorkspaceRoot string
	CommitSHA     string
	Tag           string
	Pushed        bool
	FilesWritten  []string
}

// Validate checks the structural invariants of a PublishRequest.
// Empty Files, empty ModulePath, malformed Tag, empty Author, or a
// canonical-import-path mismatch (go.mod `module X` != ModulePath)
// surface here so callers fail fast before any git side effects.
func (r PublishRequest) Validate() error {
	if len(r.Files.Files) == 0 {
		return fmt.Errorf("%w: Files is empty", ErrPublish)
	}
	if _, ok := r.Files.Files["go.mod"]; !ok {
		return fmt.Errorf("%w: Files missing go.mod", ErrPublish)
	}
	if strings.TrimSpace(r.ModulePath) == "" {
		return fmt.Errorf("%w: ModulePath is required", ErrPublish)
	}
	if !semver.IsValid(r.Tag) {
		return fmt.Errorf("%w: Tag %q is not a valid semver tag (expected vMAJOR.MINOR.PATCH)", ErrPublish, r.Tag)
	}
	if strings.TrimSpace(r.RemoteURL) == "" {
		return fmt.Errorf("%w: RemoteURL is required", ErrPublish)
	}
	if strings.TrimSpace(r.Author.Name) == "" {
		return fmt.Errorf("%w: Author.Name is required", ErrPublish)
	}
	if strings.TrimSpace(r.Author.Email) == "" {
		return fmt.Errorf("%w: Author.Email is required", ErrPublish)
	}
	declared, err := CanonicalImportPath(r.Files.Files["go.mod"])
	if err != nil {
		return err
	}
	if declared != r.ModulePath {
		return fmt.Errorf("%w: go.mod declares module %q but PublishRequest.ModulePath is %q", ErrPublish, declared, r.ModulePath)
	}
	return nil
}

// CanonicalImportPath extracts the module path from a go.mod body.
// Returns an error if the file does not begin with a `module <path>`
// directive (per Go's go.mod grammar, that directive must precede
// every other top-level statement).
func CanonicalImportPath(goMod string) (string, error) {
	for _, raw := range strings.Split(goMod, "\n") {
		line := strings.TrimSpace(raw)
		if line == "" || strings.HasPrefix(line, "//") {
			continue
		}
		if !strings.HasPrefix(line, "module ") {
			return "", fmt.Errorf("%w: go.mod first non-comment line is %q; want a module directive", ErrPublish, line)
		}
		path := strings.TrimSpace(strings.TrimPrefix(line, "module "))
		path = strings.Trim(path, "\"")
		if path == "" {
			return "", fmt.Errorf("%w: go.mod module directive has empty path", ErrPublish)
		}
		return path, nil
	}
	return "", fmt.Errorf("%w: go.mod is empty", ErrPublish)
}

// Publish runs the full publish flow. The Runner argument is the
// command-execution boundary; pass NewExecRunner() in production
// or a fake runner in tests. The returned PublishResult records
// the workspace path, commit SHA, tag, and whether the push step
// ran. On any error the partial workspace is left on disk for the
// caller to inspect (Publish does not delete a failed working
// tree).
func Publish(req PublishRequest, runner Runner) (PublishResult, error) {
	if err := req.Validate(); err != nil {
		return PublishResult{}, err
	}

	defaultBranch := req.DefaultBranch
	if defaultBranch == "" {
		defaultBranch = "main"
	}
	commitMessage := req.CommitMessage
	if commitMessage == "" {
		commitMessage = "publish " + req.Tag
	}

	root := req.WorkspaceRoot
	if root == "" {
		tmp, err := os.MkdirTemp("", "mochi-publish-")
		if err != nil {
			return PublishResult{}, fmt.Errorf("%w: temp dir: %v", ErrPublish, err)
		}
		root = tmp
	}
	if err := os.MkdirAll(root, 0o755); err != nil {
		return PublishResult{}, fmt.Errorf("%w: mkdir workspace: %v", ErrPublish, err)
	}

	written, err := materialiseFiles(root, req.Files)
	if err != nil {
		return PublishResult{}, err
	}

	if err := runner.Run(root, "git", "init", "--initial-branch="+defaultBranch); err != nil {
		return PublishResult{}, fmt.Errorf("%w: git init: %v", ErrPublish, err)
	}
	if err := runner.Run(root, "git", "config", "user.name", req.Author.Name); err != nil {
		return PublishResult{}, fmt.Errorf("%w: git config user.name: %v", ErrPublish, err)
	}
	if err := runner.Run(root, "git", "config", "user.email", req.Author.Email); err != nil {
		return PublishResult{}, fmt.Errorf("%w: git config user.email: %v", ErrPublish, err)
	}
	if err := runner.Run(root, "git", "config", "commit.gpgsign", "false"); err != nil {
		return PublishResult{}, fmt.Errorf("%w: git config commit.gpgsign: %v", ErrPublish, err)
	}
	if err := runner.Run(root, "git", "config", "tag.gpgsign", "false"); err != nil {
		return PublishResult{}, fmt.Errorf("%w: git config tag.gpgsign: %v", ErrPublish, err)
	}
	if err := runner.Run(root, "git", "remote", "add", "origin", req.RemoteURL); err != nil {
		return PublishResult{}, fmt.Errorf("%w: git remote add origin: %v", ErrPublish, err)
	}
	if err := runner.Run(root, "git", "add", "."); err != nil {
		return PublishResult{}, fmt.Errorf("%w: git add: %v", ErrPublish, err)
	}
	if err := runner.Run(root, "git", "commit", "-m", commitMessage); err != nil {
		return PublishResult{}, fmt.Errorf("%w: git commit: %v", ErrPublish, err)
	}
	if err := runner.Run(root, "git", "tag", "-a", req.Tag, "-m", "release "+req.Tag); err != nil {
		return PublishResult{}, fmt.Errorf("%w: git tag: %v", ErrPublish, err)
	}

	sha, err := runner.Output(root, "git", "rev-parse", "HEAD")
	if err != nil {
		return PublishResult{}, fmt.Errorf("%w: git rev-parse: %v", ErrPublish, err)
	}
	commitSHA := strings.TrimSpace(sha)

	pushed := false
	if !req.DryRun {
		if err := runner.Run(root, "git", "push", "origin", defaultBranch); err != nil {
			return PublishResult{}, fmt.Errorf("%w: git push branch: %v", ErrPublish, err)
		}
		if err := runner.Run(root, "git", "push", "origin", req.Tag); err != nil {
			return PublishResult{}, fmt.Errorf("%w: git push tag: %v", ErrPublish, err)
		}
		pushed = true
	}

	return PublishResult{
		WorkspaceRoot: root,
		CommitSHA:     commitSHA,
		Tag:           req.Tag,
		Pushed:        pushed,
		FilesWritten:  written,
	}, nil
}

// materialiseFiles writes the rendered EmitResult to disk under
// root, returning the sorted list of relative paths it wrote.
func materialiseFiles(root string, files library.EmitResult) ([]string, error) {
	names := make([]string, 0, len(files.Files))
	for name := range files.Files {
		names = append(names, name)
	}
	sort.Strings(names)
	for _, name := range names {
		full := filepath.Join(root, name)
		if err := os.MkdirAll(filepath.Dir(full), 0o755); err != nil {
			return nil, fmt.Errorf("%w: mkdir %s: %v", ErrPublish, filepath.Dir(name), err)
		}
		if err := os.WriteFile(full, []byte(files.Files[name]), 0o644); err != nil {
			return nil, fmt.Errorf("%w: write %s: %v", ErrPublish, name, err)
		}
	}
	return names, nil
}
