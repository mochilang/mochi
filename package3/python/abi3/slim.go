package abi3

import (
	"fmt"
	"strings"
)

// RenameWheelFilename rewrites the abi field of a wheel filename in
// place. ParseWheelTag rejects malformed inputs upstream; this helper
// surfaces only the rename concern.
func RenameWheelFilename(name, newABI string) (string, error) {
	if strings.TrimSpace(newABI) == "" {
		return "", fmt.Errorf("abi3: RenameWheelFilename requires a non-empty abi")
	}
	prefix, tag, err := SplitWheelFilename(name)
	if err != nil {
		return "", err
	}
	tag.ABI = newABI
	return prefix + "-" + tag.String() + ".whl", nil
}

// PromoteWheelToABI3 rewrites a wheel filename so the interpreter +
// abi fields claim the stable limited API (`cp32-abi3-...`). The
// platform field is preserved; callers that also need to retag the
// platform should pass the result through RenameWheelFilename's
// platform-handling caller after.
func PromoteWheelToABI3(name string) (string, error) {
	prefix, tag, err := SplitWheelFilename(name)
	if err != nil {
		return "", err
	}
	abi3Tag, err := ToABI3(tag)
	if err != nil {
		return "", err
	}
	return prefix + "-" + abi3Tag.String() + ".whl", nil
}

// RenderWHEELMarker produces the PEP 427 WHEEL metadata stub for a
// rendered abi3 wheel. The Tag field embeds the limited-API triple
// (`cp32-abi3-<platform>`) so downstream installers know they can
// resolve the wheel against any CPython ≥ 3.2.
//
// The caller is responsible for writing this to
// `<dist>-<ver>.dist-info/WHEEL` inside the wheel zip.
func RenderWHEELMarker(generator, platform string) string {
	if strings.TrimSpace(generator) == "" {
		generator = "mochi-pkg"
	}
	if strings.TrimSpace(platform) == "" {
		platform = "any"
	}
	var b strings.Builder
	b.WriteString("Wheel-Version: 1.0\n")
	fmt.Fprintf(&b, "Generator: %s\n", generator)
	b.WriteString("Root-Is-Purelib: false\n")
	fmt.Fprintf(&b, "Tag: cp32-abi3-%s\n", platform)
	return b.String()
}
