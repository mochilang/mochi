// Package yaml is the Mochi-semantics YAML runtime for the Go target.
// Mochi's existing VM uses gopkg.in/yaml.v3 (see runtime/data/yaml.go)
// and we keep that dependency choice here so the emitted Go code does
// not introduce a second YAML library to the build.
//
// Mochi's `yaml(value)` builtin prints a YAML document with sorted
// keys, the standard "---\n" prefix omitted, two-space indent. The
// upstream yaml.v3 Encoder already sorts string keys and uses
// two-space indent, so Marshal/Fprint are thin pass-throughs.
package yaml

import (
	"bytes"
	"io"
	"os"

	"gopkg.in/yaml.v3"
)

// Marshal returns the YAML encoding of v.
func Marshal(v any) ([]byte, error) { return yaml.Marshal(v) }

// Unmarshal parses YAML data into the value pointed to by v.
func Unmarshal(data []byte, v any) error { return yaml.Unmarshal(data, v) }

// Print writes the YAML encoding of v to stdout. yaml.Marshal already
// adds a trailing newline.
func Print(v any) { _ = Fprint(os.Stdout, v) }

// Fprint writes the YAML encoding of v to w.
func Fprint(w io.Writer, v any) error {
	b, err := Marshal(v)
	if err != nil {
		return err
	}
	_, err = io.Copy(w, bytes.NewReader(b))
	return err
}
