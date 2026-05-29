// Package sparse implements the cargo sparse-index protocol described at
// https://doc.rust-lang.org/cargo/reference/registry-index.html and the
// content-addressed crate cache used by Mochi to materialise downloaded
// crates. Sparse is the protocol introduced in cargo 1.68 as the default
// alternative to the git-backed index.
//
// The package is intentionally tiny so that higher phases of MEP-73 can
// compose it: Phase 1 only needs to ask "what versions exist for crate X",
// "give me the manifest record for X@Y", and "fetch the .crate tarball into
// the cache, verifying it against the index checksum".
package sparse

import (
	"fmt"
	"strings"
)

// BucketPath returns the index path for a crate name following cargo's
// bucket scheme. The name is folded to lowercase (cargo treats crate names
// case-insensitively for index lookup) but the original separator
// (- vs _) is preserved because the registry stores files under the
// canonical spelling used at publish time.
//
//	BucketPath("a")       => "1/a"
//	BucketPath("ab")      => "2/ab"
//	BucketPath("abc")     => "3/a/abc"
//	BucketPath("serde")   => "se/rd/serde"
//	BucketPath("Tokio")   => "to/ki/tokio"
//
// The cargo reference §"Index files" specifies that names with length 3
// nest under a single-letter directory derived from the first byte of the
// lowercased name, and names of length 4 or more nest under two
// two-letter directories taken from positions 0..2 and 2..4.
func BucketPath(name string) (string, error) {
	if err := ValidateCrateName(name); err != nil {
		return "", err
	}
	n := strings.ToLower(name)
	switch len(n) {
	case 1:
		return "1/" + n, nil
	case 2:
		return "2/" + n, nil
	case 3:
		return "3/" + n[:1] + "/" + n, nil
	default:
		return n[:2] + "/" + n[2:4] + "/" + n, nil
	}
}

// ValidateCrateName enforces cargo's crate-name rules: 1..=64 characters,
// starts with a Unicode XID_Start equivalent (ASCII letter or underscore
// here, since the registry only accepts ASCII), and the remaining
// characters are letters, digits, '-' or '_'. cargo also reserves a set
// of names (con, prn, ...) and forbids names that conflict with Windows
// device files, but those rules are publisher-side and are checked
// separately in Phase 10.
func ValidateCrateName(name string) error {
	if name == "" {
		return fmt.Errorf("sparse: empty crate name")
	}
	if len(name) > 64 {
		return fmt.Errorf("sparse: crate name %q exceeds 64 bytes", name)
	}
	for i, r := range name {
		ok := (r >= 'A' && r <= 'Z') || (r >= 'a' && r <= 'z') || r == '_'
		if i > 0 {
			ok = ok || (r >= '0' && r <= '9') || r == '-'
		}
		if !ok {
			return fmt.Errorf("sparse: invalid character %q at position %d in %q", r, i, name)
		}
	}
	return nil
}

// CrateNameEqual reports whether two crate names refer to the same crate
// under cargo's lookup rules. Cargo folds case and treats '-' and '_' as
// equivalent for the purposes of resolution (a Cargo.toml dep on
// "foo-bar" matches an index entry for "foo_bar"). The registry itself
// rejects publishing two crates whose canonical forms collide.
func CrateNameEqual(a, b string) bool {
	return canonicalCrateName(a) == canonicalCrateName(b)
}

func canonicalCrateName(s string) string {
	var b strings.Builder
	b.Grow(len(s))
	for _, r := range s {
		switch {
		case r >= 'A' && r <= 'Z':
			b.WriteRune(r + ('a' - 'A'))
		case r == '-':
			b.WriteByte('_')
		default:
			b.WriteRune(r)
		}
	}
	return b.String()
}
