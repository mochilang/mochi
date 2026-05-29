package pypackage

import "fmt"

// RenderSdist builds the source-distribution layout. The result is a flat
// path -> body map rooted at the sdist's top-level directory
// (`<distribution>-<version>/`). A downstream packer (Phase 11) turns the
// Layout into the actual .tar.gz.
//
// Files emitted:
//
//   - pyproject.toml          // PEP 517 build config + project metadata
//   - PKG-INFO                // METADATA-2.1 body
//   - README.md               // Summary as a minimal README so PyPI shows it
//   - <module>/__init__.py    // runtime re-export of mochi_runtime
//   - <module>/__init__.pyi   // typed surface stub
//   - mochi_build/__init__.py // bundled PEP 517 backend
func RenderSdist(p Package) (*Layout, error) {
	if err := p.Validate(); err != nil {
		return nil, err
	}
	root := fmt.Sprintf("%s-%s", p.Distribution, p.Version)
	mod := p.ModuleName()
	l := NewLayout()
	l.Add(root+"/pyproject.toml", PyprojectTOML(p))
	l.Add(root+"/PKG-INFO", PKGInfo(p))
	l.Add(root+"/README.md", readme(p))
	l.Add(root+"/"+mod+"/__init__.py", InitPy(p))
	l.Add(root+"/"+mod+"/__init__.pyi", InitPYI(p))
	l.Add(root+"/mochi_build/__init__.py", MochiBuildBackend())
	return l, nil
}

func readme(p Package) string {
	return "# " + p.Distribution + "\n\n" + p.Summary + "\n"
}
