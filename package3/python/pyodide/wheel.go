package pyodide

import (
	"fmt"
	"sort"
	"strings"
)

// WheelCandidate is one resolver-supplied wheel the selector ranks.
// Filename is the canonical PEP 491 filename (i.e. what the
// `<dist>-<ver>-<py>-<abi>-<platform>.whl` shape the simple-index
// emits). URL is what the install loop downloads.
type WheelCandidate struct {
	Filename string
	URL      string
}

// SelectionResult is what the selector returns: the winning candidate
// plus a per-candidate reason map so the CLI can explain why each
// rejected wheel did not match.
type SelectionResult struct {
	Chosen   *WheelCandidate
	Reasons  map[string]string
	ChosenTag Tag
}

// Selector picks the best wheel from candidates for target. Ordering:
//
//  1. Pyodide vintage wheels rank by descending Vintage (newer wins).
//  2. Emscripten wheels rank by descending EmscriptenVersion.
//  3. WASI Preview 2 wheels are unordered (there is no per-host
//     vintage today; the component-model ABI is host-agnostic).
//
// MinVintage is enforced as a hard floor: any pyodide wheel older
// than it is rejected with `older than MinVintage`. ABI mismatch is
// also a hard reject. Anything that survives both filters competes
// on the ranking above.
type Selector struct {
	Target Target
}

// Select runs the picker. It is deterministic: ties break on the
// Filename string compare so two runs over the same input return the
// same Chosen.
func (s Selector) Select(candidates []WheelCandidate) (*SelectionResult, error) {
	if err := s.Target.Validate(); err != nil {
		return nil, err
	}
	result := &SelectionResult{Reasons: map[string]string{}}

	type ranked struct {
		cand WheelCandidate
		tag  Tag
	}
	var pool []ranked

	for _, c := range candidates {
		_, platTag, err := splitWheelFilename(c.Filename)
		if err != nil {
			result.Reasons[c.Filename] = err.Error()
			continue
		}
		abi := wheelABI(c.Filename)
		if s.Target.PythonABI != "" && abi != s.Target.PythonABI {
			result.Reasons[c.Filename] = fmt.Sprintf("abi %q != target %q", abi, s.Target.PythonABI)
			continue
		}
		tag, err := ParsePlatformTag(platTag)
		if err != nil {
			result.Reasons[c.Filename] = err.Error()
			continue
		}
		if !tag.SatisfiesRuntime(s.Target.Runtime) {
			result.Reasons[c.Filename] = fmt.Sprintf("tag kind %q is not %q", tag.Kind, s.Target.Runtime)
			continue
		}
		if tag.Kind == TagPyodide && !s.Target.MinVintage.Zero() && tag.Vintage.Less(s.Target.MinVintage) {
			result.Reasons[c.Filename] = fmt.Sprintf("vintage %s older than min %s", tag.Vintage, s.Target.MinVintage)
			continue
		}
		pool = append(pool, ranked{cand: c, tag: tag})
	}

	if len(pool) == 0 {
		return result, nil
	}

	sort.SliceStable(pool, func(i, j int) bool {
		a, b := pool[i].tag, pool[j].tag
		// Pyodide vintage descending.
		if a.Kind == TagPyodide && b.Kind == TagPyodide {
			if a.Vintage.Less(b.Vintage) {
				return false
			}
			if b.Vintage.Less(a.Vintage) {
				return true
			}
			return pool[i].cand.Filename < pool[j].cand.Filename
		}
		// Emscripten version descending.
		if a.Kind == TagEmscripten && b.Kind == TagEmscripten {
			av, bv := a.Emscripten, b.Emscripten
			if av.Major != bv.Major {
				return av.Major > bv.Major
			}
			if av.Minor != bv.Minor {
				return av.Minor > bv.Minor
			}
			if av.Patch != bv.Patch {
				return av.Patch > bv.Patch
			}
			return pool[i].cand.Filename < pool[j].cand.Filename
		}
		// Mixed Pyodide + Emscripten: prefer Pyodide (it is the
		// modern shape; emscripten_X_Y_Z_wasm32 only survives in
		// legacy wheels).
		if a.Kind == TagPyodide && b.Kind == TagEmscripten {
			return true
		}
		if a.Kind == TagEmscripten && b.Kind == TagPyodide {
			return false
		}
		// WASI Preview 2 (unordered) or any other tie: filename.
		return pool[i].cand.Filename < pool[j].cand.Filename
	})

	winner := pool[0]
	result.Chosen = &winner.cand
	result.ChosenTag = winner.tag
	return result, nil
}

func splitWheelFilename(name string) (prefix string, platform string, err error) {
	base := strings.TrimSuffix(name, ".whl")
	if base == name {
		return "", "", fmt.Errorf("pyodide: %q does not end in .whl", name)
	}
	parts := strings.Split(base, "-")
	if len(parts) < 5 {
		return "", "", fmt.Errorf("pyodide: wheel %q is missing fields (want dist-ver-py-abi-plat)", name)
	}
	// PEP 491: <dist>-<ver>(-<build>)?-<py>-<abi>-<plat>. The
	// platform tag is always the last hyphenated segment, even for
	// pyodide_2024_0_wasm32 because PEP 491 escapes underscores in
	// the platform tag (no extra hyphens).
	platform = parts[len(parts)-1]
	prefix = strings.Join(parts[:len(parts)-1], "-")
	return prefix, platform, nil
}

func wheelABI(name string) string {
	base := strings.TrimSuffix(name, ".whl")
	parts := strings.Split(base, "-")
	if len(parts) < 5 {
		return ""
	}
	return parts[len(parts)-2]
}
