package packagist

import (
	"fmt"
	"sort"
	"strings"
)

// PackageResponse is the top-level Packagist v2 JSON envelope returned by
// GET /p2/<vendor>/<package>.json. The Packages map key is always the
// full "<vendor>/<name>" string.
type PackageResponse struct {
	// Packages maps "<vendor>/<name>" to the ordered list of version objects.
	Packages map[string][]PackageVersion `json:"packages"`
	// Security is the optional security advisory list. Not used by the bridge
	// in phase 1; preserved for round-trip fidelity.
	Security []interface{} `json:"security,omitempty"`
}

// Versions returns all PackageVersion records for the given vendor/name,
// or nil when the key is absent.
func (r *PackageResponse) Versions(vendor, name string) []PackageVersion {
	if r == nil || r.Packages == nil {
		return nil
	}
	return r.Packages[vendor+"/"+name]
}

// PackageVersion is a single version record in the Packagist v2 response.
// Fields match the Packagist v2 API shape; unknown keys are ignored.
type PackageVersion struct {
	// Name is the full Composer package name, e.g. "guzzlehttp/guzzle".
	Name string `json:"name"`
	// Version is the human-readable version string, e.g. "7.8.1" or
	// "dev-main". Packagist also emits "v7.8.1" with a "v" prefix; callers
	// should normalise via NormaliseVersion.
	Version string `json:"version"`
	// VersionNormalized is the four-part semver-ish string Packagist
	// produces internally, e.g. "7.8.1.0". May be empty on older entries.
	VersionNormalized string `json:"version_normalized,omitempty"`
	// Dist carries the download archive information.
	Dist DistInfo `json:"dist"`
	// Require is the [require] section of composer.json for this version.
	Require map[string]string `json:"require,omitempty"`
	// RequireDev is the [require-dev] section.
	RequireDev map[string]string `json:"require-dev,omitempty"`
	// Autoload carries the PSR-4 (and other) autoload maps.
	Autoload AutoloadInfo `json:"autoload,omitempty"`
	// Description is the short package description.
	Description string `json:"description,omitempty"`
	// License is the SPDX license identifier list.
	License []string `json:"license,omitempty"`
	// PHPConstraint is the php engine constraint from require."php".
	// Populated by NormalisePHPConstraint; not present in the raw JSON.
	PHPConstraint string `json:"-"`
}

// DistInfo holds download metadata for a single package version.
type DistInfo struct {
	// Type is the archive type: "zip" is universal for Packagist.
	Type string `json:"type"`
	// URL is the absolute download URL for the dist archive.
	URL string `json:"url"`
	// Shasum is the lowercase hex SHA-1 of the dist archive as emitted
	// by Packagist. Note: this is SHA-1, not SHA-256; phase 2 recomputes
	// a SHA-256 after download for the content-addressed cache.
	Shasum string `json:"shasum,omitempty"`
	// Reference is the git commit/tag/branch the dist was built from.
	Reference string `json:"reference,omitempty"`
}

// AutoloadInfo carries the PSR-4 (and legacy PSR-0 / classmap) autoload maps
// from composer.json. The bridge uses PSR4 in phase 7 to generate vendor/autoload.php.
type AutoloadInfo struct {
	// PSR4 maps namespace prefix to relative source directory, e.g.
	// {"GuzzleHttp\\": "src/"}.
	PSR4 map[string]string `json:"psr-4,omitempty"`
	// PSR0 is the legacy PSR-0 classmap.
	PSR0 map[string]string `json:"psr-0,omitempty"`
	// Classmap is a list of directories or files to classmap-scan.
	Classmap []string `json:"classmap,omitempty"`
	// Files is a list of files to include unconditionally.
	Files []string `json:"files,omitempty"`
}

// NormaliseVersion strips a leading "v" from version strings, returning
// the bare semver. Packagist inconsistently emits both "v7.8.1" and "7.8.1".
func NormaliseVersion(v string) string {
	return strings.TrimPrefix(v, "v")
}

// NormalisePHPConstraint extracts the PHP engine constraint from the Require
// map (key "php") and stores it in PHPConstraint. If no "php" key is present,
// PHPConstraint is set to "*" (any version).
func NormalisePHPConstraint(pv *PackageVersion) {
	if pv.Require != nil {
		if c, ok := pv.Require["php"]; ok && c != "" {
			pv.PHPConstraint = c
			return
		}
	}
	pv.PHPConstraint = "*"
}

// ParseName splits a full Composer package name "vendor/name" into its
// two components. Returns an error when the name does not contain exactly
// one slash or either component is empty or the name component itself
// contains a slash.
func ParseName(full string) (vendor, name string, err error) {
	parts := strings.Split(full, "/")
	if len(parts) != 2 || parts[0] == "" || parts[1] == "" {
		return "", "", fmt.Errorf("packagist: invalid package name %q: expected vendor/name", full)
	}
	return parts[0], parts[1], nil
}

// SortVersions sorts a slice of PackageVersion descending by the normalized
// four-part version string. Versions that look like stable semver sort above
// dev/ branch aliases. Mutates in place.
func SortVersions(versions []PackageVersion) {
	sort.SliceStable(versions, func(i, j int) bool {
		vi := versions[i].VersionNormalized
		vj := versions[j].VersionNormalized
		if vi == "" {
			vi = NormaliseVersion(versions[i].Version)
		}
		if vj == "" {
			vj = NormaliseVersion(versions[j].Version)
		}
		// dev- aliases sort last.
		iDev := strings.HasPrefix(vi, "dev-") || strings.HasSuffix(vi, "-dev")
		jDev := strings.HasPrefix(vj, "dev-") || strings.HasSuffix(vj, "-dev")
		if iDev != jDev {
			return !iDev
		}
		// Lexicographic descending on the normalised string is a reasonable
		// approximation for Packagist's four-part X.Y.Z.P scheme.
		return vi > vj
	})
}

// LatestStable returns the first non-dev PackageVersion from an already-sorted
// slice (highest stable first). Returns a zero value and false when no stable
// version is present.
func LatestStable(versions []PackageVersion) (PackageVersion, bool) {
	for _, v := range versions {
		ver := v.VersionNormalized
		if ver == "" {
			ver = NormaliseVersion(v.Version)
		}
		if strings.HasPrefix(ver, "dev-") || strings.HasSuffix(ver, "-dev") {
			continue
		}
		return v, true
	}
	return PackageVersion{}, false
}

// MatchConstraint returns the highest-sorted PackageVersion whose normalised
// version satisfies a simple Composer constraint string. The supported forms
// are: bare "X.Y.Z", "^X.Y", "~X.Y", ">= X.Y" and the wildcard "*".
// This is deliberately a simplified matcher; the full Composer semver solver
// is deferred to phase 8.
func MatchConstraint(versions []PackageVersion, constraint string) (PackageVersion, bool) {
	constraint = strings.TrimSpace(constraint)
	if constraint == "*" || constraint == "" {
		return LatestStable(versions)
	}
	for _, v := range versions {
		ver := v.VersionNormalized
		if ver == "" {
			ver = NormaliseVersion(v.Version)
		}
		if strings.HasPrefix(ver, "dev-") || strings.HasSuffix(ver, "-dev") {
			continue
		}
		if matchesConstraint(ver, constraint) {
			return v, true
		}
	}
	return PackageVersion{}, false
}

// matchesConstraint checks whether normalised version ver satisfies a single
// Composer constraint expression. Supports: exact, ^, ~, >=, >, <=, <.
func matchesConstraint(ver, constraint string) bool {
	switch {
	case strings.HasPrefix(constraint, "^"):
		return matchesCaret(ver, strings.TrimPrefix(constraint, "^"))
	case strings.HasPrefix(constraint, "~"):
		return matchesTilde(ver, strings.TrimPrefix(constraint, "~"))
	case strings.HasPrefix(constraint, ">="):
		req := strings.TrimSpace(strings.TrimPrefix(constraint, ">="))
		return compareVersions(ver, req) >= 0
	case strings.HasPrefix(constraint, ">"):
		req := strings.TrimSpace(strings.TrimPrefix(constraint, ">"))
		return compareVersions(ver, req) > 0
	case strings.HasPrefix(constraint, "<="):
		req := strings.TrimSpace(strings.TrimPrefix(constraint, "<="))
		return compareVersions(ver, req) <= 0
	case strings.HasPrefix(constraint, "<"):
		req := strings.TrimSpace(strings.TrimPrefix(constraint, "<"))
		return compareVersions(ver, req) < 0
	default:
		// Bare version or wildcard: exact match on normalised prefix.
		return strings.HasPrefix(ver, NormaliseVersion(constraint))
	}
}

// matchesCaret implements Composer's ^ operator: ^1.2.3 means >=1.2.3 <2.0.0.
func matchesCaret(ver, req string) bool {
	parts := splitVersion(req)
	verParts := splitVersion(ver)
	if len(parts) == 0 || len(verParts) == 0 {
		return false
	}
	// Major must match; version must be >= req.
	if verParts[0] != parts[0] {
		return false
	}
	return compareVersions(ver, req) >= 0
}

// matchesTilde implements Composer's ~ operator: ~1.2 means >=1.2 <2.0.
func matchesTilde(ver, req string) bool {
	parts := splitVersion(req)
	verParts := splitVersion(ver)
	if len(parts) == 0 || len(verParts) == 0 {
		return false
	}
	if compareVersions(ver, req) < 0 {
		return false
	}
	// Increment the first component of req by 1 to get the upper bound.
	upper := parts[0]
	if n := parseDigit(upper); n >= 0 {
		upper = fmt.Sprintf("%d", n+1)
	}
	return parseDigit(verParts[0]) < parseDigit(upper)
}

func splitVersion(v string) []string {
	// Normalise: strip leading v, split on dots, take up to 4 parts.
	v = strings.TrimPrefix(v, "v")
	parts := strings.SplitN(v, ".", 4)
	return parts
}

func parseDigit(s string) int {
	n := 0
	for _, ch := range s {
		if ch < '0' || ch > '9' {
			return -1
		}
		n = n*10 + int(ch-'0')
	}
	return n
}

// compareVersions compares two normalised version strings lexicographically
// by numeric component. Returns -1, 0, or 1.
func compareVersions(a, b string) int {
	ap := splitVersion(a)
	bp := splitVersion(b)
	max := len(ap)
	if len(bp) > max {
		max = len(bp)
	}
	for i := 0; i < max; i++ {
		var na, nb int
		if i < len(ap) {
			na = parseDigit(ap[i])
		}
		if i < len(bp) {
			nb = parseDigit(bp[i])
		}
		if na < 0 {
			na = 0
		}
		if nb < 0 {
			nb = 0
		}
		if na < nb {
			return -1
		}
		if na > nb {
			return 1
		}
	}
	return 0
}
