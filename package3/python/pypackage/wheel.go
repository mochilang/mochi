package pypackage

import (
	"fmt"
	"sort"
)

// RenderWheel builds the wheel layout. The result is the flat path -> body
// map a downstream zipper turns into the actual .whl. The wheel layout is
// rooted at the wheel's top level (no leading directory) and contains:
//
//   - <module>/__init__.py
//   - <module>/__init__.pyi
//   - <dist>-<ver>.dist-info/METADATA
//   - <dist>-<ver>.dist-info/WHEEL
//   - <dist>-<ver>.dist-info/RECORD
//
// RECORD is computed last by hashing every other file body with sha256
// (base64-urlsafe, no padding) per PEP 376.
func RenderWheel(p Package) (*Layout, error) {
	if err := p.Validate(); err != nil {
		return nil, err
	}
	mod := p.ModuleName()
	distInfo := fmt.Sprintf("%s-%s.dist-info", p.Distribution, p.Version)
	l := NewLayout()
	l.Add(mod+"/__init__.py", InitPy(p))
	l.Add(mod+"/__init__.pyi", InitPYI(p))
	l.Add(distInfo+"/METADATA", PKGInfo(p))
	l.Add(distInfo+"/WHEEL", WheelMetadata())
	l.Add(distInfo+"/RECORD", recordBody(l, distInfo+"/RECORD"))
	return l, nil
}

// recordBody computes the RECORD body. RECORD itself is listed without a
// hash or size per PEP 376; every other file gets a sha256 hash and byte
// count.
func recordBody(l *Layout, recordPath string) string {
	paths := make([]string, 0, len(l.Files)+1)
	for p := range l.Files {
		paths = append(paths, p)
	}
	paths = append(paths, recordPath)
	sort.Strings(paths)
	var out string
	for _, p := range paths {
		if p == recordPath {
			out += RecordLine(RecordEntry{Path: p}) + "\n"
			continue
		}
		body := l.Files[p]
		out += RecordLine(RecordEntry{
			Path: p,
			Hash: HashBody(body),
			Size: len(body),
		}) + "\n"
	}
	return out
}
