package maven

import (
	"errors"
	"os"
	"sync"
	"testing"
)

func TestCachePutGet(t *testing.T) {
	dir := t.TempDir()
	c := NewCache(dir)

	key := "aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899"
	data := []byte("hello, cache!")
	if err := c.Put(key, data); err != nil {
		t.Fatalf("Put: %v", err)
	}
	got, err := c.Get(key)
	if err != nil {
		t.Fatalf("Get: %v", err)
	}
	if string(got) != string(data) {
		t.Errorf("Get returned %q; want %q", got, data)
	}
}

func TestCacheHas(t *testing.T) {
	dir := t.TempDir()
	c := NewCache(dir)
	key := "aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899"
	if c.Has(key) {
		t.Error("Has() = true before Put")
	}
	if err := c.Put(key, []byte("data")); err != nil {
		t.Fatalf("Put: %v", err)
	}
	if !c.Has(key) {
		t.Error("Has() = false after Put")
	}
}

func TestCacheMiss(t *testing.T) {
	dir := t.TempDir()
	c := NewCache(dir)
	key := "aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899"
	_, err := c.Get(key)
	if !errors.Is(err, ErrCacheMiss) {
		t.Errorf("Get missing: got %v; want ErrCacheMiss", err)
	}
}

func TestCachePath(t *testing.T) {
	c := NewCache("/cache")
	key := "aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899"
	got := c.Path(key)
	want := "/cache/aa/bb/" + key + ".jar"
	if got != want {
		t.Errorf("Path() = %q; want %q", got, want)
	}
}

func TestCacheAtomicWrite(t *testing.T) {
	dir := t.TempDir()
	c := NewCache(dir)
	key := "aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899"
	data := make([]byte, 10*1024) // 10 KB

	const n = 20
	var wg sync.WaitGroup
	errs := make(chan error, n)
	for i := 0; i < n; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			if err := c.Put(key, data); err != nil {
				errs <- err
			}
		}()
	}
	wg.Wait()
	close(errs)
	for err := range errs {
		t.Errorf("concurrent Put: %v", err)
	}

	got, err := c.Get(key)
	if err != nil {
		t.Fatalf("Get after concurrent Put: %v", err)
	}
	if len(got) != len(data) {
		t.Errorf("Get returned %d bytes; want %d", len(got), len(data))
	}

	// Verify no temp files left.
	entries, _ := os.ReadDir(dir)
	for _, e := range entries {
		for _, sub := range []string{"aa", "bb"} {
			_ = sub
			subEntries, _ := os.ReadDir(dir + "/aa/bb/")
			for _, se := range subEntries {
				if se.Name() != key+".jar" {
					t.Errorf("unexpected file in cache: %s", se.Name())
				}
			}
		}
		_ = e
	}
}
