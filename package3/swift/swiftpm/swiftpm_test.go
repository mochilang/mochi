package swiftpm_test

import (
	"testing"

	"mochi/package3/swift/swiftpm"
)

const sampleDump = `{
  "name": "MyLib",
  "platforms": [
    {"platformName": "macos", "version": "13.0"}
  ],
  "products": [
    {
      "name": "MyLib",
      "targets": ["MyLib"],
      "type": {"library": ["automatic"]}
    }
  ],
  "targets": [
    {
      "name": "MyLib",
      "type": "regular",
      "path": "Sources/MyLib",
      "dependencies": [
        {"byName": {"name": "OtherLib", "condition": null}}
      ]
    }
  ],
  "dependencies": [
    {
      "sourceControl": [
        {
          "location": {
            "remote": [{"urlString": "https://github.com/example/other.git"}]
          },
          "requirement": {
            "range": [{"lowerBound": "1.0.0", "upperBound": "2.0.0"}]
          }
        }
      ]
    }
  ]
}`

func TestParseDumpJSON(t *testing.T) {
	m, err := swiftpm.ParseDumpJSON([]byte(sampleDump))
	if err != nil {
		t.Fatalf("ParseDumpJSON error: %v", err)
	}
	if m.Name != "MyLib" {
		t.Errorf("Name = %q, want %q", m.Name, "MyLib")
	}
	if len(m.Platforms) != 1 || m.Platforms[0].Name != "macos" {
		t.Errorf("Platforms = %v", m.Platforms)
	}
	if len(m.Products) != 1 || m.Products[0].Name != "MyLib" {
		t.Errorf("Products = %v", m.Products)
	}
	if len(m.Targets) != 1 || m.Targets[0].Name != "MyLib" {
		t.Errorf("Targets = %v", m.Targets)
	}
	if len(m.Targets[0].Dependencies) != 1 || m.Targets[0].Dependencies[0] != "OtherLib" {
		t.Errorf("Target deps = %v", m.Targets[0].Dependencies)
	}
	if len(m.Dependencies) != 1 {
		t.Fatalf("Dependencies len = %d, want 1", len(m.Dependencies))
	}
	if m.Dependencies[0].URL != "https://github.com/example/other.git" {
		t.Errorf("Dep URL = %q", m.Dependencies[0].URL)
	}
	if m.Dependencies[0].Requirement != `"1.0.0"..<"2.0.0"` {
		t.Errorf("Dep Req = %q", m.Dependencies[0].Requirement)
	}
}

func TestParseDumpJSONExact(t *testing.T) {
	data := `{
		"name": "Pkg",
		"platforms": [],
		"products": [],
		"targets": [],
		"dependencies": [
			{
				"sourceControl": [{
					"location": {"remote": [{"urlString": "https://example.com/pkg.git"}]},
					"requirement": {"exact": ["1.2.3"]}
				}]
			}
		]
	}`
	m, err := swiftpm.ParseDumpJSON([]byte(data))
	if err != nil {
		t.Fatalf("ParseDumpJSON error: %v", err)
	}
	if len(m.Dependencies) != 1 {
		t.Fatalf("Dependencies len = %d", len(m.Dependencies))
	}
	if m.Dependencies[0].Requirement != `exact: "1.2.3"` {
		t.Errorf("Req = %q", m.Dependencies[0].Requirement)
	}
}
