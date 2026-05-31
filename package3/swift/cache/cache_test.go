package cache_test

import (
	"bytes"
	"os"
	"testing"

	"mochi/package3/swift/cache"
)

func TestCacheRoundTrip(t *testing.T) {
	dir := t.TempDir()
	c := &cache.Cache{Dir: dir}

	key := c.KeyFor("https://reg.example.com", "scope.pkg", "1.0.0")
	if key == "" {
		t.Fatal("KeyFor returned empty key")
	}

	// Key must be deterministic
	key2 := c.KeyFor("https://reg.example.com", "scope.pkg", "1.0.0")
	if key != key2 {
		t.Errorf("KeyFor is not deterministic: %q != %q", key, key2)
	}

	// Different inputs must produce different keys
	keyOther := c.KeyFor("https://reg.example.com", "scope.pkg", "2.0.0")
	if key == keyOther {
		t.Error("KeyFor collision for different versions")
	}

	// Nothing cached yet
	if _, ok := c.Get(key); ok {
		t.Error("Get returned true before Put")
	}

	// Put content
	content := []byte("fake archive bytes")
	path, err := c.Put(key, bytes.NewReader(content))
	if err != nil {
		t.Fatalf("Put error: %v", err)
	}

	// Get should now return the path
	got, ok := c.Get(key)
	if !ok {
		t.Error("Get returned false after Put")
	}
	if got != path {
		t.Errorf("Get path = %q, want %q", got, path)
	}

	// Content should match
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("ReadFile error: %v", err)
	}
	if !bytes.Equal(data, content) {
		t.Errorf("content mismatch: got %q, want %q", data, content)
	}
}

func TestCacheKeyUniqueness(t *testing.T) {
	c := &cache.Cache{Dir: t.TempDir()}
	keys := map[string]string{}
	inputs := [][3]string{
		{"reg1", "scope.a", "1.0.0"},
		{"reg1", "scope.b", "1.0.0"},
		{"reg2", "scope.a", "1.0.0"},
		{"reg1", "scope.a", "2.0.0"},
	}
	for _, inp := range inputs {
		k := c.KeyFor(inp[0], inp[1], inp[2])
		combo := inp[0] + "|" + inp[1] + "|" + inp[2]
		for prevCombo, prevKey := range keys {
			if prevKey == k {
				t.Errorf("collision: %s and %s produced same key %s", prevCombo, combo, k)
			}
		}
		keys[combo] = k
	}
}
