package build

import (
	"runtime"
	"testing"
)

// TestPhase8CSVAdapters is the MEP-45 Phase 8.4 gate for the CSV load/save
// adapters. It walks every fixture under tests/transpiler3/c/fixtures/csv_io
// and runs the same end-to-end pipeline as all other phase gates.
//
// The fixtures probe:
//   - loadCSV: basic 2-column, 2-row file (cells printed individually)
//   - loadCSV: 3x3 grid (row count and column count per row)
//   - loadCSV: empty file returns list of length 0
//   - loadCSV + saveCSV round-trip (3x3 grid loaded, saved, reloaded)
//   - loadCSV: RFC 4180 quoted fields (comma in cell, "" escaped quote)
//   - saveCSV: build list<list<string>> from literals, save, reload and print
//   - loadCSV: single-row file (len == 1, len(row) == 3)
//   - loadCSV: 5-column file, print len of each row and last cell value
func TestPhase8CSVAdapters(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("C toolchain not available in Windows CI")
	}
	runFixtureSuite(t, "csv_io")
}
