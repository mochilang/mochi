package pypackage

import (
	"os"
	"path/filepath"
	"reflect"
	"sort"
	"testing"
)

func TestLayoutAddOverwrites(t *testing.T) {
	l := NewLayout()
	l.Add("foo.txt", "v1")
	l.Add("foo.txt", "v2")
	if got := l.Files["foo.txt"]; got != "v2" {
		t.Fatalf("expected overwrite, got %q", got)
	}
}

func TestLayoutPathsSorted(t *testing.T) {
	l := NewLayout()
	l.Add("c", "")
	l.Add("a", "")
	l.Add("b", "")
	got := l.Paths()
	want := []string{"a", "b", "c"}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("Paths = %v, want %v", got, want)
	}
}

func TestLayoutWriteRejectsEmptyDst(t *testing.T) {
	l := NewLayout()
	if _, err := l.Write(""); err == nil {
		t.Fatal("expected error for empty destination")
	}
}

func TestLayoutWriteCreatesNestedDirs(t *testing.T) {
	dst := t.TempDir()
	l := NewLayout()
	l.Add("pkg/sub/file.txt", "hello")
	l.Add("top.txt", "world")
	written, err := l.Write(dst)
	if err != nil {
		t.Fatalf("Write: %v", err)
	}
	if len(written) != 2 {
		t.Fatalf("expected 2 written, got %d", len(written))
	}
	sort.Strings(written)
	body, err := os.ReadFile(filepath.Join(dst, "pkg/sub/file.txt"))
	if err != nil {
		t.Fatalf("read: %v", err)
	}
	if string(body) != "hello" {
		t.Fatalf("file body = %q, want hello", body)
	}
}
