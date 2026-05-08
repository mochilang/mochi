package types_test

import (
	"bufio"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestMEP7ObligationsTableMatchesFixtures keeps the obligations table
// in website/docs/mep/mep-0007.md honest. Each row in the table names
// a fixture pointer; if a fixture is renamed without updating the
// MEP, this test fails so the MEP gets fixed in the same commit.
//
// Pointers of the form "none yet, ..." are skipped on purpose: those
// rows track gaps that we have not yet filled.
func TestMEP7ObligationsTableMatchesFixtures(t *testing.T) {
	repoRoot := findRepoRoot(t)
	mepPath := filepath.Join(repoRoot, "website", "docs", "mep", "mep-0007.md")

	rows := parseObligationsTable(t, mepPath)
	if len(rows) == 0 {
		t.Fatalf("no obligations rows parsed from %s", mepPath)
	}

	for _, row := range rows {
		property := row.property
		pointer := row.pointer
		for _, ptr := range splitPointers(pointer) {
			ptr = strings.TrimSpace(ptr)
			if ptr == "" || strings.HasPrefix(ptr, "none yet") {
				continue
			}
			abs := filepath.Join(repoRoot, ptr)
			matches, err := filepath.Glob(abs)
			if err != nil {
				t.Errorf("property %q: bad glob %q: %v", property, ptr, err)
				continue
			}
			if len(matches) == 0 {
				if _, statErr := os.Stat(abs); statErr == nil {
					continue
				}
				if _, statErr := os.Stat(abs + ".mochi"); statErr == nil {
					continue
				}
				t.Errorf("property %q: pointer %q matches no file under %s", property, ptr, repoRoot)
			}
		}
	}
}

type obligationRow struct {
	property string
	pointer  string
}

func parseObligationsTable(t *testing.T, path string) []obligationRow {
	t.Helper()
	f, err := os.Open(path)
	if err != nil {
		t.Fatalf("open %s: %v", path, err)
	}
	defer f.Close()

	var rows []obligationRow
	inTable := false
	scanner := bufio.NewScanner(f)
	scanner.Buffer(make([]byte, 1024*1024), 1024*1024)
	for scanner.Scan() {
		line := scanner.Text()
		if strings.HasPrefix(line, "| Property") {
			inTable = true
			continue
		}
		if !inTable {
			continue
		}
		if !strings.HasPrefix(line, "|") {
			break
		}
		cells := splitMarkdownRow(line)
		if len(cells) < 3 {
			continue
		}
		if strings.HasPrefix(cells[0], "-") || strings.Contains(cells[0], "Property") {
			continue
		}
		rows = append(rows, obligationRow{
			property: cells[0],
			pointer:  cells[2],
		})
	}
	if err := scanner.Err(); err != nil {
		t.Fatalf("scan %s: %v", path, err)
	}
	return rows
}

func splitMarkdownRow(line string) []string {
	parts := strings.Split(line, "|")
	if len(parts) >= 2 {
		parts = parts[1 : len(parts)-1]
	}
	out := make([]string, 0, len(parts))
	for _, p := range parts {
		out = append(out, strings.TrimSpace(p))
	}
	return out
}

// splitPointers extracts every backticked substring in the cell and
// returns each as a distinct pointer. Cells with no backticks are
// returned as a single entry; the caller decides whether to skip
// "none yet" rows.
func splitPointers(cell string) []string {
	if !strings.Contains(cell, "`") {
		return []string{strings.TrimSpace(cell)}
	}
	var out []string
	for {
		i := strings.Index(cell, "`")
		if i < 0 {
			break
		}
		j := strings.Index(cell[i+1:], "`")
		if j < 0 {
			break
		}
		out = append(out, strings.TrimSpace(cell[i+1:i+1+j]))
		cell = cell[i+1+j+1:]
	}
	return out
}

func findRepoRoot(t *testing.T) string {
	t.Helper()
	dir, err := os.Getwd()
	if err != nil {
		t.Fatalf("getwd: %v", err)
	}
	for {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			t.Fatalf("could not locate go.mod from %s", dir)
		}
		dir = parent
	}
}
