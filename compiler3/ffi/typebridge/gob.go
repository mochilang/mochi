package typebridge

import (
	"bytes"
	"encoding/gob"
	"fmt"
)

// gobVersion is the leading byte of every gob-encoded Type. A Mochi
// upgrade that adds a new Kind / OpaqueReason bumps this constant
// and the Phase 2 binding cache discards stale entries via the
// version mismatch rather than returning garbage.
const gobVersion byte = 0x01

type wireType struct {
	Kind         uint8
	Width        uint8
	Elem         *Type
	Key          *Type
	ArrayLen     int64
	Fields       []Field
	Params       []Type
	Results      []Type
	Variadic     bool
	Name         string
	PkgPath      string
	TypeArgs     []Type
	Methods      []Method
	ChanDir      uint8
	OpaqueReason uint8
	GoType       string
}

// GobEncode implements gob.GobEncoder. The leading byte is the
// format version; the remainder is the gob-encoded wireType.
func (t Type) GobEncode() ([]byte, error) {
	var buf bytes.Buffer
	buf.WriteByte(gobVersion)
	w := wireType{
		Kind:         uint8(t.Kind),
		Width:        uint8(t.Width),
		Elem:         t.Elem,
		Key:          t.Key,
		ArrayLen:     t.ArrayLen,
		Fields:       t.Fields,
		Params:       t.Params,
		Results:      t.Results,
		Variadic:     t.Variadic,
		Name:         t.Name,
		PkgPath:      t.PkgPath,
		TypeArgs:     t.TypeArgs,
		Methods:      t.Methods,
		ChanDir:      uint8(t.ChanDir),
		OpaqueReason: uint8(t.OpaqueReason),
		GoType:       t.GoType,
	}
	enc := gob.NewEncoder(&buf)
	if err := enc.Encode(w); err != nil {
		return nil, err
	}
	return buf.Bytes(), nil
}

// GobDecode implements gob.GobDecoder. Returns an error on version
// mismatch so the cache invalidator can drop stale entries.
func (t *Type) GobDecode(data []byte) error {
	if len(data) == 0 {
		return fmt.Errorf("typebridge: empty gob payload")
	}
	if data[0] != gobVersion {
		return fmt.Errorf("typebridge: unsupported gob version 0x%02x (want 0x%02x)", data[0], gobVersion)
	}
	var w wireType
	dec := gob.NewDecoder(bytes.NewReader(data[1:]))
	if err := dec.Decode(&w); err != nil {
		return err
	}
	*t = Type{
		Kind:         Kind(w.Kind),
		Width:        Width(w.Width),
		Elem:         w.Elem,
		Key:          w.Key,
		ArrayLen:     w.ArrayLen,
		Fields:       w.Fields,
		Params:       w.Params,
		Results:      w.Results,
		Variadic:     w.Variadic,
		Name:         w.Name,
		PkgPath:      w.PkgPath,
		TypeArgs:     w.TypeArgs,
		Methods:      w.Methods,
		ChanDir:      ChanDir(w.ChanDir),
		OpaqueReason: OpaqueReason(w.OpaqueReason),
		GoType:       w.GoType,
	}
	return nil
}
