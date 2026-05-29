// Package vanity is the MEP-74 phase 17 vanity-import path
// resolver plus the wasm-wasip1 / wasm-js publish gate.
//
// The Go ecosystem distinguishes two kinds of import paths:
//
//  1. Direct paths whose first segment is a known VCS host
//     (github.com, gitlab.com, bitbucket.org, etc.) where the
//     module proxy can fetch the source directly from the host.
//  2. Vanity paths (golang.org/x/*, gopkg.in/*, k8s.io/*,
//     google.golang.org/*, sigs.k8s.io/*, and any user-controlled
//     domain) that delegate to a real VCS via the `<meta
//     name="go-import" content="<root> <vcs> <repo-url>">` HTML
//     redirect tag served at the path-prefix URL.
//
// MEP-74's `import go "<module>@<semver>"` form needs to resolve
// vanity paths before the phase 1 module-proxy client can fetch
// the source. This package owns that resolution: a single
// `Resolve(path)` call that, for a vanity path, fetches the
// go-get probe URL, parses the meta tag, and returns the
// `Resolution{Module, VCS, RepoURL}` the proxy client can use.
//
// The wasm publish-direction gate is a separate concern that
// happens to live in the same package: the
// `WasmTarget` profile constants name wasm-wasip1, wasm-js, and a
// `CheckPublish(target, pkg)` walker reports violations for any
// surface that cannot be published under the given target (e.g.,
// cgo-using packages cannot be published as wasm-wasip1; packages
// importing `syscall/js` cannot be published as wasm-wasip1
// either).
//
// Out of scope for v1 (deferred to 17.1+): a real HTTP transport
// against live vanity hosts (the unit tests use an in-memory
// transport), a per-mochi.toml override table for vendored
// vanity paths, and the wazero-host smoke run that actually
// instantiates the published wasm module.
package vanity

import (
	"errors"
	"fmt"
	"io"
	"net/http"
	"sort"
	"strings"

	"mochi/package3/go/apisurface"
)

// ErrVanity is the package-wide error sentinel.
var ErrVanity = errors.New("vanity")

// KnownVCSHosts is the closed list of import-path first-segments
// that the proxy fetches from directly (no vanity redirect needed).
// Anything else is treated as a vanity path and goes through the
// go-import meta-tag resolver.
//
// Sorted lexicographically; the order is load-bearing for the
// `IsVanity` predicate's stability.
var KnownVCSHosts = []string{
	"bitbucket.org",
	"github.com",
	"gitlab.com",
}

// Resolution is the result of resolving a vanity path: the canonical
// module root, the VCS system ("git", "hg", "svn", "fossil"), and
// the real VCS repository URL.
type Resolution struct {
	// Module is the canonical module root from the meta tag's
	// first field (e.g., "golang.org/x/sync").
	Module string
	// VCS is the version-control system the meta tag's second
	// field names. The Go convention restricts this to "git", "hg",
	// "svn", or "fossil"; the resolver does not further validate
	// (the proxy client owns that check).
	VCS string
	// RepoURL is the meta tag's third field, the real VCS-clonable
	// URL (e.g., "https://go.googlesource.com/sync").
	RepoURL string
}

// IsVanity reports whether path's first segment is NOT a known VCS
// host (and so requires meta-tag resolution).
func IsVanity(path string) bool {
	if path == "" {
		return false
	}
	first := path
	if slash := strings.Index(path, "/"); slash >= 0 {
		first = path[:slash]
	}
	for _, host := range KnownVCSHosts {
		if first == host {
			return false
		}
	}
	return true
}

// Fetcher is the minimal HTTP-fetch interface the resolver needs.
// Implementations: the default `http.DefaultClient`-backed fetcher,
// or a unit-test in-memory map keyed by URL.
type Fetcher interface {
	// Fetch returns the body of `https://<host>/<path>?go-get=1`
	// as a string. Returns ErrVanity-wrapped errors on transport
	// failure or non-2xx status.
	Fetch(url string) (string, error)
}

// HTTPFetcher is the default Fetcher implementation backed by
// `net/http`. It uses the supplied client (or `http.DefaultClient`
// if nil) and follows redirects per stdlib defaults.
type HTTPFetcher struct {
	Client *http.Client
}

// Fetch implements Fetcher.
func (f *HTTPFetcher) Fetch(url string) (string, error) {
	client := f.Client
	if client == nil {
		client = http.DefaultClient
	}
	resp, err := client.Get(url)
	if err != nil {
		return "", fmt.Errorf("%w: GET %s: %v", ErrVanity, url, err)
	}
	defer resp.Body.Close()
	if resp.StatusCode/100 != 2 {
		return "", fmt.Errorf("%w: GET %s: status %d", ErrVanity, url, resp.StatusCode)
	}
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return "", fmt.Errorf("%w: read %s: %v", ErrVanity, url, err)
	}
	return string(body), nil
}

// MapFetcher is a hermetic Fetcher backed by an in-memory map keyed
// by URL. Used by the package's own tests; consumers can also wrap
// it for offline reproducibility.
type MapFetcher map[string]string

// Fetch implements Fetcher; returns ErrVanity if the URL is not
// in the map.
func (m MapFetcher) Fetch(url string) (string, error) {
	body, ok := m[url]
	if !ok {
		return "", fmt.Errorf("%w: MapFetcher: no entry for %s", ErrVanity, url)
	}
	return body, nil
}

// Resolve takes an import path, decides whether it's a vanity path,
// and either returns a direct-VCS Resolution (no fetch) or fetches
// the go-import meta tag from the path's `?go-get=1` URL and
// returns the parsed Resolution.
//
// For direct paths (github.com/..., gitlab.com/..., etc.) the
// returned Resolution has VCS="git" and RepoURL="https://<path>"
// (truncated to the module root, i.e., the first three segments
// for hosted git providers); the resolver does not fetch.
func Resolve(path string, fetcher Fetcher) (Resolution, error) {
	if strings.TrimSpace(path) == "" {
		return Resolution{}, fmt.Errorf("%w: empty path", ErrVanity)
	}
	if !IsVanity(path) {
		return directResolution(path), nil
	}
	if fetcher == nil {
		return Resolution{}, fmt.Errorf("%w: vanity path %q needs a Fetcher", ErrVanity, path)
	}
	url := "https://" + path + "?go-get=1"
	body, err := fetcher.Fetch(url)
	if err != nil {
		return Resolution{}, err
	}
	res, err := ParseGoImport(body, path)
	if err != nil {
		return Resolution{}, fmt.Errorf("%w: %s: %v", ErrVanity, path, err)
	}
	return res, nil
}

// directResolution builds a Resolution for a non-vanity path. The
// module root for a hosted-git provider is the first three
// segments (host + owner + repo); deeper paths are sub-packages
// inside the same module.
func directResolution(path string) Resolution {
	parts := strings.Split(path, "/")
	root := path
	if len(parts) >= 3 {
		root = strings.Join(parts[:3], "/")
	}
	return Resolution{
		Module:  root,
		VCS:     "git",
		RepoURL: "https://" + root,
	}
}

// ParseGoImport scans HTML body for `<meta name="go-import"
// content="<root> <vcs> <repo>">` and returns the Resolution whose
// Module is the import-path-most-specific prefix of `wantPrefix`.
// If multiple meta tags match, the longest matching Module wins
// (so `golang.org/x/sync` picks the `x/sync`-rooted tag, not a
// `golang.org`-wide fallback). Returns an error if no go-import
// meta matches `wantPrefix`.
func ParseGoImport(body, wantPrefix string) (Resolution, error) {
	tags := scanGoImportTags(body)
	if len(tags) == 0 {
		return Resolution{}, fmt.Errorf("no go-import meta tag found")
	}
	var best Resolution
	bestLen := -1
	for _, t := range tags {
		if t.Module == wantPrefix || strings.HasPrefix(wantPrefix+"/", t.Module+"/") {
			if len(t.Module) > bestLen {
				best = t
				bestLen = len(t.Module)
			}
		}
	}
	if bestLen < 0 {
		return Resolution{}, fmt.Errorf("no go-import meta tag covers %q", wantPrefix)
	}
	return best, nil
}

// scanGoImportTags returns every well-formed
// `<meta name="go-import" content="<root> <vcs> <repo>">`
// occurrence in body. The scanner is intentionally tag-level (not
// a full HTML parser): it handles single- and double-quoted
// attribute values and is tolerant of whitespace and the `name` /
// `content` attribute order.
func scanGoImportTags(body string) []Resolution {
	var out []Resolution
	lower := strings.ToLower(body)
	i := 0
	for {
		idx := strings.Index(lower[i:], "<meta")
		if idx < 0 {
			break
		}
		start := i + idx
		end := strings.Index(body[start:], ">")
		if end < 0 {
			break
		}
		tag := body[start : start+end+1]
		i = start + end + 1
		nameVal := extractAttr(tag, "name")
		if !strings.EqualFold(nameVal, "go-import") {
			continue
		}
		contentVal := extractAttr(tag, "content")
		if contentVal == "" {
			continue
		}
		fields := strings.Fields(contentVal)
		if len(fields) != 3 {
			continue
		}
		out = append(out, Resolution{Module: fields[0], VCS: fields[1], RepoURL: fields[2]})
	}
	return out
}

// extractAttr returns the value of the named attribute in tag, or
// "" if absent. Handles single- and double-quoted values.
func extractAttr(tag, name string) string {
	lower := strings.ToLower(tag)
	key := strings.ToLower(name) + "="
	idx := strings.Index(lower, key)
	if idx < 0 {
		return ""
	}
	rest := tag[idx+len(key):]
	if rest == "" {
		return ""
	}
	quote := rest[0]
	if quote != '"' && quote != '\'' {
		return ""
	}
	end := strings.IndexByte(rest[1:], quote)
	if end < 0 {
		return ""
	}
	return rest[1 : 1+end]
}

// WasmTarget names the publish-direction wasm target. The
// `CheckPublish` walker uses it to decide which import + type
// bans apply.
type WasmTarget string

const (
	// WasmTargetWasip1 is the WASI Preview 1 target. Has no JS host;
	// `syscall/js` is banned. cgo is banned (no wasm cgo runtime).
	WasmTargetWasip1 WasmTarget = "wasm-wasip1"
	// WasmTargetJS is the wasm-js / `js/wasm` target. Has the JS
	// host; `syscall/js` is allowed. cgo is still banned.
	WasmTargetJS WasmTarget = "wasm-js"
)

// IsValid reports whether t is a recognised wasm target.
func (t WasmTarget) IsValid() bool {
	switch t {
	case WasmTargetWasip1, WasmTargetJS:
		return true
	}
	return false
}

// wasmBanned returns the closed banned-import set for target t.
// Both wasm targets ban runtime/cgo and the C-toolchain-coupled
// debug packages; wasip1 additionally bans `syscall/js`.
func wasmBanned(t WasmTarget) map[string]struct{} {
	base := map[string]struct{}{
		"runtime/cgo": {},
		"debug/elf":   {},
		"debug/macho": {},
		"debug/pe":    {},
		"os/exec":     {},
		"plugin":      {},
	}
	if t == WasmTargetWasip1 {
		base["syscall/js"] = struct{}{}
	}
	return base
}

// Violation is one wasm-publish incompatibility. Mirrors the shape
// of `tinygo.Violation` so a consumer can fold both gates' output
// into a single SkipReport stream.
type Violation struct {
	Kind   string
	Where  string
	Reason string
}

// String renders the violation as `<kind>: <where>: <reason>`.
func (v Violation) String() string {
	return v.Kind + ": " + v.Where + ": " + v.Reason
}

// CheckPublish walks pkg and reports every import or surface element
// that would prevent publishing to wasm target t. Sorted by
// (Kind, Where) for byte-deterministic output.
func CheckPublish(t WasmTarget, pkg apisurface.Package) []Violation {
	if !t.IsValid() {
		return nil
	}
	banned := wasmBanned(t)
	var out []Violation
	for _, imp := range pkg.Imports {
		if _, hit := banned[imp]; hit {
			out = append(out, Violation{
				Kind:   "import",
				Where:  imp,
				Reason: fmt.Sprintf("import is unavailable on %s", t),
			})
		}
	}
	sort.SliceStable(out, func(i, j int) bool {
		if out[i].Kind != out[j].Kind {
			return out[i].Kind < out[j].Kind
		}
		return out[i].Where < out[j].Where
	})
	return out
}

// IsPublishable reports whether pkg has zero violations under
// target t.
func IsPublishable(t WasmTarget, pkg apisurface.Package) bool {
	return len(CheckPublish(t, pkg)) == 0
}
