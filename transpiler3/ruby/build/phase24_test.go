package build

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase24TargetIRubyKernel covers TargetIRubyKernel: emit a Jupyter
// notebook whose code cell contains the lowered Ruby and whose kernelspec
// targets IRuby.
func TestPhase24TargetIRubyKernel(t *testing.T) {
	if _, err := resolveToolchain(); err != nil {
		t.Skipf("ruby toolchain not available: %v", err)
	}

	srcDir := t.TempDir()
	src := filepath.Join(srcDir, "hello_nb.mochi")
	body := "print(\"hi from notebook\")\nprint(2 + 2)\n"
	if err := os.WriteFile(src, []byte(body), 0o644); err != nil {
		t.Fatalf("write src: %v", err)
	}

	out := t.TempDir()
	d := &Driver{CacheDir: t.TempDir()}
	if err := d.Build(src, out, TargetIRubyKernel); err != nil {
		t.Fatalf("Build TargetIRubyKernel: %v", err)
	}

	nbPath := filepath.Join(out, "hello_nb.ipynb")
	raw, err := os.ReadFile(nbPath)
	if err != nil {
		t.Fatalf("read notebook: %v", err)
	}

	var nb struct {
		NbFormat int `json:"nbformat"`
		Metadata struct {
			Kernelspec struct {
				Name        string `json:"name"`
				DisplayName string `json:"display_name"`
				Language    string `json:"language"`
			} `json:"kernelspec"`
			LanguageInfo struct {
				Name string `json:"name"`
			} `json:"language_info"`
		} `json:"metadata"`
		Cells []struct {
			CellType string   `json:"cell_type"`
			Source   []string `json:"source"`
		} `json:"cells"`
	}
	if err := json.Unmarshal(raw, &nb); err != nil {
		t.Fatalf("notebook is not valid JSON: %v\nraw:\n%s", err, raw)
	}
	if nb.NbFormat != 4 {
		t.Fatalf("nbformat = %d, want 4", nb.NbFormat)
	}
	if nb.Metadata.Kernelspec.Name != "ruby" {
		t.Fatalf("kernelspec.name = %q, want ruby", nb.Metadata.Kernelspec.Name)
	}
	if nb.Metadata.Kernelspec.Language != "ruby" {
		t.Fatalf("kernelspec.language = %q, want ruby", nb.Metadata.Kernelspec.Language)
	}
	if nb.Metadata.LanguageInfo.Name != "ruby" {
		t.Fatalf("language_info.name = %q, want ruby", nb.Metadata.LanguageInfo.Name)
	}
	if len(nb.Cells) != 1 || nb.Cells[0].CellType != "code" {
		t.Fatalf("want one code cell, got %#v", nb.Cells)
	}
	joined := strings.Join(nb.Cells[0].Source, "")
	if !strings.Contains(joined, "hi from notebook") {
		t.Fatalf("code cell missing source literal:\n%s", joined)
	}
	if !strings.Contains(joined, "frozen_string_literal") {
		t.Fatalf("code cell missing frozen_string_literal header:\n%s", joined)
	}
	// Source lines (except possibly the last) must end in newline per
	// nbformat convention.
	for i, line := range nb.Cells[0].Source {
		if i == len(nb.Cells[0].Source)-1 {
			continue
		}
		if !strings.HasSuffix(line, "\n") {
			t.Fatalf("source line %d missing trailing newline: %q", i, line)
		}
	}
}
