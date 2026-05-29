package abi2026

import (
	"fmt"
	"strings"
)

// PromoteToABI2026 rewrites a `cp32-abi3-<platform>` wheel filename
// as `cp314-abi2026-<platform>`. The promotion is a filename-level
// operation only; sub-phase 18.2 will pair it with a re-link step
// that swaps the embedded interpreter-tag metadata in `.dist-info/`.
//
// Returns an error when name is not in the abi3 shape.
func PromoteToABI2026(name string) (string, error) {
	prefix, tag, err := splitWheelFilename(name)
	if err != nil {
		return "", err
	}
	parts := strings.Split(tag, "-")
	if len(parts) != 3 {
		return "", fmt.Errorf("abi2026: tag %q is not interpreter-abi-platform", tag)
	}
	if parts[1] != "abi3" {
		return "", fmt.Errorf("abi2026: cannot promote %q: ABI tag is %q, want abi3", name, parts[1])
	}
	parts[0] = "cp314"
	parts[1] = "abi2026"
	return prefix + "-" + strings.Join(parts, "-") + ".whl", nil
}

// DowngradeToABI3 is the inverse: `cp314-abi2026-<platform>` ->
// `cp32-abi3-<platform>`. Useful during the migration window so a
// vendor can verify that a promoted wheel still parses + installs
// in the legacy resolver path.
func DowngradeToABI3(name string) (string, error) {
	prefix, tag, err := splitWheelFilename(name)
	if err != nil {
		return "", err
	}
	parts := strings.Split(tag, "-")
	if len(parts) != 3 {
		return "", fmt.Errorf("abi2026: tag %q is not interpreter-abi-platform", tag)
	}
	if parts[1] != "abi2026" {
		return "", fmt.Errorf("abi2026: cannot downgrade %q: ABI tag is %q, want abi2026", name, parts[1])
	}
	parts[0] = "cp32"
	parts[1] = "abi3"
	return prefix + "-" + strings.Join(parts, "-") + ".whl", nil
}

func splitWheelFilename(name string) (prefix, tag string, err error) {
	base := strings.TrimSuffix(name, ".whl")
	if base == name {
		return "", "", fmt.Errorf("abi2026: %q does not end in .whl", name)
	}
	parts := strings.Split(base, "-")
	if len(parts) < 5 {
		return "", "", fmt.Errorf("abi2026: wheel %q is missing fields", name)
	}
	tag = strings.Join(parts[len(parts)-3:], "-")
	prefix = strings.Join(parts[:len(parts)-3], "-")
	return prefix, tag, nil
}
