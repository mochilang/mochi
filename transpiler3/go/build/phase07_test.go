package build

import (
	"testing"
)

// TestPhase7Query is the MEP-54 Phase 7 gate for the Go transpiler.
//
// Phase 7 is subdivided:
//
//   7.1-7.5  Query DSL (from/where/select/order/skip/take/join/group-by)
//            Lowered to imperative Go: a result slice, for loops, optional
//            sort.Slice, and slice trimming for skip/take.
//
//   7.6      String builtins: upper/lower/reverse/split/join/substr/contains
//            Mapped to strings.ToUpper, strings.ToLower, mochiStrReverse,
//            strings.Split, strings.Join, mochiSubstr, strings.Contains.
//
//   7.7      File I/O: read_file, write_file, append_file, lines
//            Mapped to os.ReadFile / os.WriteFile (flag O_APPEND) / bufio scanner.
//
//   7.8      CSV: load_csv → mochiLoadCSV, save_csv → mochiSaveCSV
//            Each backed by encoding/csv reader/writer helpers in the prelude.
//
//   7.9      Math: abs/floor/ceil/sqrt/pow/min/max → math package calls;
//            int abs → mochiAbsInt helper.
//
//   7.10     Ordered maps (omap): backed by a []pair slice + map for O(1)
//            get; put keeps insertion order.
//
//   7.11     JSON: json_decode backed by encoding/json.Unmarshal into
//            interface{}; typed decode helpers cast the result.
//
//   7.12     Error handling: try/catch → defer+recover pattern;
//            panic(code, msg) → panic(msg).
func TestPhase7Query(t *testing.T) {
	runFixturesMatchingPrefixes(t,
		"query_", "arena_",
	)
}

// TestPhase7Strings gates the Phase 7.6 string builtin fixtures.
func TestPhase7Strings(t *testing.T) {
	runFixturesMatchingPrefixes(t,
		"str_upper", "str_lower", "str_reverse", "str_split", "str_join",
		"str_substring", "str_contains", "str_index", "str_methods",
	)
}

// TestPhase7FileIO gates the Phase 7.7 file I/O fixtures.
func TestPhase7FileIO(t *testing.T) {
	runFixturesMatchingPrefixes(t,
		"file_", "lines_",
	)
}

// TestPhase7CSV gates the Phase 7.8 CSV fixtures.
func TestPhase7CSV(t *testing.T) {
	runFixturesMatchingPrefixes(t,
		"csv_",
	)
}

// TestPhase7Math gates the Phase 7.9 math fixtures.
func TestPhase7Math(t *testing.T) {
	runFixturesMatchingPrefixes(t,
		"abs_", "ceil_", "floor_", "math_", "min_max",
	)
}

// TestPhase7OMap gates the Phase 7.10 ordered-map fixtures.
func TestPhase7OMap(t *testing.T) {
	runFixturesMatchingPrefixes(t,
		"omap_",
	)
}

// TestPhase7JSON gates the Phase 7.11 JSON decode fixtures.
func TestPhase7JSON(t *testing.T) {
	runFixturesMatchingPrefixes(t,
		"json_",
	)
}

// TestPhase7Errors gates the Phase 7.12 error-handling fixtures.
func TestPhase7Errors(t *testing.T) {
	runFixturesMatchingPrefixes(t,
		"try_catch_", "user_panic_",
	)
}
