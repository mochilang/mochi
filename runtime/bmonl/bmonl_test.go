package bmonl

import (
	"bytes"
	"os"
	"testing"

	"github.com/stretchr/testify/require"
)

func TestBmonReadWriteFile(t *testing.T) {
	rows := []map[string]any{
		{"a": int64(1)},
		{"b": "two"},
	}
	dir := t.TempDir()
	file := dir + "/rows.bmonl"
	require.NoError(t, WriteFile(rows, file))
	out, err := ReadFile(file)
	require.NoError(t, err)
	require.Equal(t, rows, out)

	// Test stdin/stdout with path "-"
	rfile := dir + "/rows2.bmonl"
	f, err := os.Create(rfile)
	require.NoError(t, err)
	defer f.Close()
	oldStdout := os.Stdout
	os.Stdout = f
	require.NoError(t, WriteFile(rows, "-"))
	os.Stdout = oldStdout
	f.Close()
	out2, err := ReadFile(rfile)
	require.NoError(t, err)
	require.Equal(t, rows, out2)
}

func TestBmonlMarshalUnmarshal(t *testing.T) {
	rows := []map[string]any{
		{"a": int64(1)},
		{"b": "two"},
	}
	data, err := Marshal(rows)
	require.NoError(t, err)
	var out []map[string]any
	require.NoError(t, Unmarshal(data, &out))
	require.Equal(t, rows, out)
}

func TestBmonlEncodeDecode(t *testing.T) {
	buf := &bytes.Buffer{}
	enc := NewEncoder(buf)
	require.NoError(t, enc.Encode(map[string]any{"a": int64(1)}))
	require.NoError(t, enc.Encode(map[string]any{"b": int64(2)}))

	dec := NewDecoder(bytes.NewReader(buf.Bytes()))
	var m1, m2 map[string]any
	require.NoError(t, dec.Decode(&m1))
	require.NoError(t, dec.Decode(&m2))
	require.Equal(t, map[string]any{"a": int64(1)}, m1)
	require.Equal(t, map[string]any{"b": int64(2)}, m2)
}
