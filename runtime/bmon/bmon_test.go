package bmon

import (
	"bytes"
	"testing"
	"time"

	"github.com/stretchr/testify/require"
)

func roundTrip(t *testing.T, v any) any {
	t.Helper()
	data, err := Marshal(v)
	require.NoError(t, err)
	var out any
	err = Unmarshal(data, &out)
	require.NoError(t, err)
	return out
}

func TestBmonBasics(t *testing.T) {
	require.Equal(t, "ok", roundTrip(t, "ok"))
	require.Equal(t, int64(42), roundTrip(t, int64(42)))
	require.Equal(t, 3.14, roundTrip(t, 3.14))
	require.Equal(t, true, roundTrip(t, true))
	require.Nil(t, roundTrip(t, nil))
}

func TestBmonArrayAndMap(t *testing.T) {
	arr := []any{"a", int64(1)}
	require.Equal(t, arr, roundTrip(t, arr))

	m := map[string]any{"a": int64(1), "b": "two"}
	out := roundTrip(t, m).(map[string]any)
	require.Equal(t, m, out)
}

func TestBmonTime(t *testing.T) {
	now := time.Date(2025, 6, 23, 1, 2, 3, 0, time.UTC)
	v := roundTrip(t, now).(time.Time)
	require.True(t, v.Equal(now))
}

func TestBmonStreamDecodingMultiple(t *testing.T) {
	buf := &bytes.Buffer{}
	enc := NewEncoder(buf)
	require.NoError(t, enc.Encode(map[string]any{"a": int64(1)}))
	require.NoError(t, enc.Encode(map[string]any{"b": int64(2)}))

	dec := NewDecoder(bytes.NewReader(buf.Bytes()))
	var v1 any
	require.NoError(t, dec.Decode(&v1))
	require.Equal(t, map[string]any{"a": int64(1)}, v1)
	var v2 any
	require.NoError(t, dec.Decode(&v2))
	require.Equal(t, map[string]any{"b": int64(2)}, v2)
}
