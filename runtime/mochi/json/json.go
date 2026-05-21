// Package json is the Mochi-semantics JSON runtime for the Go target.
// The exported entry points match the VM's `json(value)` builtin: a
// canonical, sorted-key, two-space-indented form (PrintIndent), plus
// a compact single-line form (Marshal) for round-tripping.
//
// Sorted-key serialisation is the load-bearing piece: Mochi's test
// fixtures under tests/vm/valid/*.mochi.out compare textually, so the
// emitter must produce stable JSON regardless of map iteration order
// inside the emitting Go program. `encoding/json` already sorts string
// map keys; we re-emit the JSON tree to also stabilise the indent and
// to be explicit about the contract.
package json

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"os"
)

// Marshal returns the compact JSON encoding of v with sorted map keys.
// Errors are surfaced rather than panicked; callers in emitted Mochi
// code panic on serialisation failures because Mochi's `json` is
// total over the value tree it accepts.
func Marshal(v any) ([]byte, error) { return json.Marshal(v) }

// MarshalIndent returns a two-space-indented form of v with sorted
// map keys. This is the form Mochi's `json(value)` prints by default.
func MarshalIndent(v any) ([]byte, error) {
	var buf bytes.Buffer
	enc := json.NewEncoder(&buf)
	enc.SetIndent("", "  ")
	enc.SetEscapeHTML(false)
	if err := enc.Encode(v); err != nil {
		return nil, err
	}
	out := buf.Bytes()
	// json.Encoder.Encode appends a newline; strip it so the emitter
	// can choose whether to add one (matches `fmt.Print` semantics).
	if n := len(out); n > 0 && out[n-1] == '\n' {
		out = out[:n-1]
	}
	return out, nil
}

// Print writes the indented JSON form of v to stdout, with a trailing
// newline. This is the exact behaviour of Mochi's `json(value)` op.
func Print(v any) {
	_ = Fprint(os.Stdout, v)
}

// Fprint writes the indented JSON form of v to w with a trailing
// newline.
func Fprint(w io.Writer, v any) error {
	b, err := MarshalIndent(v)
	if err != nil {
		return err
	}
	if _, err := w.Write(b); err != nil {
		return err
	}
	_, err = fmt.Fprintln(w)
	return err
}

// Unmarshal parses JSON data into the value pointed to by v.
func Unmarshal(data []byte, v any) error { return json.Unmarshal(data, v) }
