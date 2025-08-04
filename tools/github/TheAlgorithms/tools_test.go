package thealgorithms

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestListAndDownload(t *testing.T) {
	paths, err := List(nil, "Python")
	if err != nil {
		t.Fatalf("List failed: %v", err)
	}
	if len(paths) == 0 {
		t.Fatalf("List returned no paths")
	}

	root, err := repoRoot()
	if err != nil {
		t.Fatalf("repoRoot: %v", err)
	}
	repoName := "Python"
	indexPath := filepath.Join(root, "tests", "github", repoOwner, repoName, "index.txt")
	f, err := os.Open(indexPath)
	if err != nil {
		t.Fatalf("index file not found: %v", err)
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	var previous string
	lineNum := 0
	for scanner.Scan() {
		line := scanner.Text()
		lineNum++
		var idx int
		var path string
		fmt.Sscanf(line, "%d %s", &idx, &path)
		if idx != lineNum {
			t.Fatalf("index mismatch at line %d: got %d", lineNum, idx)
		}
		if previous != "" && strings.Compare(previous, path) > 0 {
			t.Fatalf("paths not sorted: %s before %s", previous, path)
		}
		previous = path
	}
	if err := scanner.Err(); err != nil {
		t.Fatalf("read index: %v", err)
	}

	// Download first file
	outPath, err := DownloadByNumber(nil, repoName, 1)
	if err != nil {
		t.Fatalf("DownloadByNumber failed: %v", err)
	}
	if _, err := os.Stat(outPath); err != nil {
		t.Fatalf("downloaded file missing: %v", err)
	}
}
