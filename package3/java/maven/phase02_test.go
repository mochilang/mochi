package maven

import (
	"errors"
	"testing"
)

// TestPhase2JARCache is the phase-gate sentinel test for MEP-67 phase 2.
// It verifies the content-addressed JAR cache end-to-end.
func TestPhase2JARCache(t *testing.T) {
	t.Run("put_get_roundtrip", func(t *testing.T) {
		dir := t.TempDir()
		c := NewCache(dir)
		key := "deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"
		data := []byte("mep-67 test jar data")
		if err := c.Put(key, data); err != nil {
			t.Fatalf("Put: %v", err)
		}
		got, err := c.Get(key)
		if err != nil {
			t.Fatalf("Get: %v", err)
		}
		if string(got) != string(data) {
			t.Errorf("roundtrip mismatch: got %q, want %q", got, data)
		}
	})

	t.Run("has_miss", func(t *testing.T) {
		dir := t.TempDir()
		c := NewCache(dir)
		key := "deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"
		if c.Has(key) {
			t.Error("Has returns true for non-existent key")
		}
		_, err := c.Get(key)
		if !errors.Is(err, ErrCacheMiss) {
			t.Errorf("Get returns %v; want ErrCacheMiss", err)
		}
	})

	t.Run("path_layout", func(t *testing.T) {
		c := NewCache("/base")
		key := "0102030405060708090a0b0c0d0e0f10112233445566778899aabbccddeeff00"
		path := c.Path(key)
		if path != "/base/01/02/"+key+".jar" {
			t.Errorf("Path() = %q; want /base/01/02/%s.jar", path, key)
		}
	})

	t.Run("idempotent_put", func(t *testing.T) {
		dir := t.TempDir()
		c := NewCache(dir)
		key := "deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"
		data := []byte("idempotent test")
		for i := 0; i < 3; i++ {
			if err := c.Put(key, data); err != nil {
				t.Fatalf("Put iteration %d: %v", i, err)
			}
		}
		if !c.Has(key) {
			t.Error("Has returns false after repeated Puts")
		}
	})
}
