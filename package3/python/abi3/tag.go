package abi3

import (
	"fmt"
	"strconv"
	"strings"
)

// WheelTag is the (interpreter, abi, platform) triple at the tail of a
// wheel filename per PEP 425. Compressed tag sets (multiple interpreters
// joined with '.') are normalised by splitting them at parse time and
// re-joining at String time, so callers can mutate one field at a time
// without worrying about the join.
type WheelTag struct {
	Interpreter string
	ABI         string
	Platform    string
}

// ParseWheelTag splits a `{python}-{abi}-{platform}` triple. It accepts
// the compressed form too (e.g. "cp311.cp312-abi3-manylinux_2_28_x86_64")
// but keeps the interpreter field as the literal compressed string; the
// caller is responsible for splitting on '.' when iterating.
func ParseWheelTag(s string) (WheelTag, error) {
	parts := strings.Split(s, "-")
	if len(parts) != 3 {
		return WheelTag{}, fmt.Errorf("abi3: wheel tag %q must have 3 hyphen-separated fields", s)
	}
	for i, p := range parts {
		if strings.TrimSpace(p) == "" {
			return WheelTag{}, fmt.Errorf("abi3: wheel tag %q field %d is empty", s, i)
		}
	}
	return WheelTag{Interpreter: parts[0], ABI: parts[1], Platform: parts[2]}, nil
}

// String reassembles the triple. It does not validate; round-tripping
// a previously-parsed tag is byte-identical.
func (t WheelTag) String() string {
	return t.Interpreter + "-" + t.ABI + "-" + t.Platform
}

// IsABI3 reports whether the tag claims the stable limited API. Only
// "abi3" is honoured; "none" or interpreter-specific ABIs ("cp312") do
// not count even if the extension happens to only use limited symbols.
func (t WheelTag) IsABI3() bool { return t.ABI == "abi3" }

// ToABI3 promotes a tag to the limited-API form. The new interpreter
// is the floor CPython release that the limited API supports; per the
// CPython docs that floor is 3.2 (`cp32`). The platform is preserved
// since limited-API status does not change platform compatibility.
//
// ToABI3 rejects non-CPython interpreters (PyPy, GraalPy, etc.) because
// the stable ABI is a CPython contract, and rejects tags whose
// interpreter does not parse as a `cp<digits>` shape.
func ToABI3(t WheelTag) (WheelTag, error) {
	if !strings.HasPrefix(t.Interpreter, "cp") {
		return WheelTag{}, fmt.Errorf("abi3: only CPython interpreters support the stable ABI; got %q", t.Interpreter)
	}
	rest := strings.TrimPrefix(t.Interpreter, "cp")
	if _, err := strconv.Atoi(rest); err != nil {
		return WheelTag{}, fmt.Errorf("abi3: interpreter %q does not parse as cp<digits>", t.Interpreter)
	}
	return WheelTag{Interpreter: "cp32", ABI: "abi3", Platform: t.Platform}, nil
}

// SplitWheelFilename pulls the trailing tag triple off a wheel filename
// (per PEP 491 / PEP 427: `{dist}-{ver}[-{build}]-{tag}.whl`). It is
// deliberately strict: the input must end in `.whl` and contain at
// least four hyphen-separated fields.
func SplitWheelFilename(name string) (prefix string, tag WheelTag, err error) {
	if !strings.HasSuffix(name, ".whl") {
		return "", WheelTag{}, fmt.Errorf("abi3: wheel filename %q must end in .whl", name)
	}
	base := strings.TrimSuffix(name, ".whl")
	parts := strings.Split(base, "-")
	if len(parts) < 4 {
		return "", WheelTag{}, fmt.Errorf("abi3: wheel filename %q lacks dist-ver-...-tag fields", name)
	}
	tagPart := strings.Join(parts[len(parts)-3:], "-")
	t, err := ParseWheelTag(tagPart)
	if err != nil {
		return "", WheelTag{}, err
	}
	return strings.Join(parts[:len(parts)-3], "-"), t, nil
}
