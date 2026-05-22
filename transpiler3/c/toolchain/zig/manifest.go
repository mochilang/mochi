package zig

// Version is the pinned Zig release this MEP-45 phase ships against.
// The manifest below must always reflect this exact version; bumping
// requires re-fetching ziglang.org/download/index.json and updating
// every Asset URL and SHA-256.
const Version = "0.16.0"

// Asset is one downloadable Zig archive for a specific (arch, os)
// host. URL, SHA256 (lowercase hex), and Size (uncompressed byte
// length is not required; Size is the tarball byte length per the
// upstream index.json).
type Asset struct {
	URL    string
	SHA256 string
	Size   int64
	// Kind discriminates extraction: "tar.xz" for POSIX hosts,
	// "zip" for Windows hosts.
	Kind string
}

// Manifest is the (arch, os) → Asset map for Zig Version. Keys are
// the same `<arch>-<os>` shape ziglang.org/download/index.json uses.
// Phase 1.3 ships the tier-1 host set: x86_64-linux, aarch64-linux,
// x86_64-macos, aarch64-macos, x86_64-windows, aarch64-windows.
// Other hosts can be added by appending rows; the install path
// derives the lookup key from runtime.GOARCH and runtime.GOOS.
var Manifest = map[string]Asset{
	"x86_64-linux": {
		URL:    "https://ziglang.org/download/0.16.0/zig-x86_64-linux-0.16.0.tar.xz",
		SHA256: "70e49664a74374b48b51e6f3fdfbf437f6395d42509050588bd49abe52ba3d00",
		Size:   55478392,
		Kind:   "tar.xz",
	},
	"aarch64-linux": {
		URL:    "https://ziglang.org/download/0.16.0/zig-aarch64-linux-0.16.0.tar.xz",
		SHA256: "ea4b09bfb22ec6f6c6ceac57ab63efb6b46e17ab08d21f69f3a48b38e1534f17",
		Size:   51211944,
		Kind:   "tar.xz",
	},
	"x86_64-macos": {
		URL:    "https://ziglang.org/download/0.16.0/zig-x86_64-macos-0.16.0.tar.xz",
		SHA256: "0387557ed1877bc6a2e1802c8391953baddba76081876301c522f52977b52ba7",
		Size:   57396836,
		Kind:   "tar.xz",
	},
	"aarch64-macos": {
		URL:    "https://ziglang.org/download/0.16.0/zig-aarch64-macos-0.16.0.tar.xz",
		SHA256: "b23d70deaa879b5c2d486ed3316f7eaa53e84acf6fc9cc747de152450d401489",
		Size:   52238004,
		Kind:   "tar.xz",
	},
	"x86_64-windows": {
		URL:    "https://ziglang.org/download/0.16.0/zig-x86_64-windows-0.16.0.zip",
		SHA256: "68659eb5f1e4eb1437a722f1dd889c5a322c9954607f5edcf337bc3684a75a7e",
		Size:   97217739,
		Kind:   "zip",
	},
	"aarch64-windows": {
		URL:    "https://ziglang.org/download/0.16.0/zig-aarch64-windows-0.16.0.zip",
		SHA256: "aee38316ee4111717900f45dd3130145c39289e105541d737eb8c5ed653c78ef",
		Size:   93109828,
		Kind:   "zip",
	},
}

// goosToZig and goarchToZig translate Go runtime identifiers into
// the Manifest key shape. Zig uses "macos" for darwin and shares
// "x86_64"/"aarch64" with Go's "amd64"/"arm64".
var (
	goosToZig = map[string]string{
		"linux":   "linux",
		"darwin":  "macos",
		"windows": "windows",
	}
	goarchToZig = map[string]string{
		"amd64": "x86_64",
		"arm64": "aarch64",
	}
)

// ManifestKey returns the Manifest key for the given Go (goos,
// goarch) pair, or "" if the combination is not supported.
func ManifestKey(goos, goarch string) string {
	os, ok := goosToZig[goos]
	if !ok {
		return ""
	}
	arch, ok := goarchToZig[goarch]
	if !ok {
		return ""
	}
	return arch + "-" + os
}
