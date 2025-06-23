package bmonl

import (
	"bytes"
	"fmt"
	"io"
	"os"

	"mochi/runtime/bmon"
)

// Marshal encodes rows into BMONL bytes.
func Marshal(rows []map[string]any) ([]byte, error) {
	var buf bytes.Buffer
	if err := NewEncoder(&buf).EncodeAll(rows); err != nil {
		return nil, err
	}
	return buf.Bytes(), nil
}

// Unmarshal parses data as BMONL into dest which must be a pointer to a slice.
func Unmarshal(data []byte, dest *[]map[string]any) error {
	dec := NewDecoder(bytes.NewReader(data))
	rows, err := dec.DecodeAll()
	if err != nil {
		return err
	}
	*dest = rows
	return nil
}

// Encoder writes rows of maps to an io.Writer in BMONL format.
type Encoder struct {
	enc *bmon.Encoder
}

// NewEncoder returns a new Encoder that writes to w.
func NewEncoder(w io.Writer) *Encoder {
	return &Encoder{enc: bmon.NewEncoder(w)}
}

// Encode writes m as a single BMON value.
func (e *Encoder) Encode(m map[string]any) error {
	return e.enc.Encode(m)
}

// EncodeAll writes all rows in order.
func (e *Encoder) EncodeAll(rows []map[string]any) error {
	for _, row := range rows {
		if err := e.Encode(row); err != nil {
			return err
		}
	}
	return nil
}

// Decoder reads maps from an io.Reader in BMONL format.
type Decoder struct {
	dec *bmon.Decoder
}

// NewDecoder returns a new Decoder that reads from r.
func NewDecoder(r io.Reader) *Decoder {
	return &Decoder{dec: bmon.NewDecoder(r)}
}

// Decode reads the next row from the stream into m.
func (d *Decoder) Decode(m *map[string]any) error {
	var v any
	if err := d.dec.Decode(&v); err != nil {
		return err
	}
	if v == nil {
		*m = nil
		return nil
	}
	mv, ok := v.(map[string]any)
	if !ok {
		return fmt.Errorf("expected map, got %T", v)
	}
	*m = mv
	return nil
}

// DecodeAll reads all remaining rows from d.
func (d *Decoder) DecodeAll() ([]map[string]any, error) {
	var rows []map[string]any
	for {
		var m map[string]any
		if err := d.Decode(&m); err != nil {
			if err == io.EOF {
				break
			}
			return nil, err
		}
		if m != nil {
			rows = append(rows, m)
		}
	}
	return rows, nil
}

// ReadFile reads a BMONL file from path.
func ReadFile(path string) ([]map[string]any, error) {
	r, closeFn, err := openReader(path)
	if err != nil {
		return nil, err
	}
	defer closeFn()
	dec := NewDecoder(r)
	return dec.DecodeAll()
}

// WriteFile writes rows to path in BMONL format.
func WriteFile(rows []map[string]any, path string) error {
	w, closeFn, err := openWriter(path)
	if err != nil {
		return err
	}
	defer closeFn()
	enc := NewEncoder(w)
	return enc.EncodeAll(rows)
}

// helpers
func openReader(path string) (io.Reader, func() error, error) {
	if path == "" || path == "-" {
		return os.Stdin, func() error { return nil }, nil
	}
	f, err := os.Open(path)
	if err != nil {
		return nil, nil, err
	}
	return f, f.Close, nil
}

type nopWriteCloser struct{ io.Writer }

func (nopWriteCloser) Close() error { return nil }

func openWriter(path string) (io.Writer, func() error, error) {
	if path == "" || path == "-" {
		w := nopWriteCloser{os.Stdout}
		return w, func() error { return nil }, nil
	}
	f, err := os.Create(path)
	if err != nil {
		return nil, nil, err
	}
	return f, f.Close, nil
}
