// Package library implements the TargetPhpLibrary build target that emits a
// PSR-4 src/ tree, composer.json, README.md, and LICENSE from a Mochi source
// module.
//
// The emitted library structure:
//
//	composer.json          # Composer metadata
//	README.md              # generated documentation stub
//	LICENSE                # MIT license text
//	src/
//	  <Vendor>/<Pkg>/      # PSR-4 root directory
//	    <ClassName>.php    # one file per class/interface/enum
//
// Usage:
//
//	result, err := library.Emit(config)
//	if err != nil { ... }
//	// result.Files maps file path -> content
package library

import (
	"encoding/json"
	"fmt"
	"maps"
	"strings"
	"time"
)

// Config holds the metadata needed to emit a PHP library.
type Config struct {
	// ComposerName is the Composer package name, e.g. "acme/my-lib".
	ComposerName string
	// Description is a short package description for composer.json.
	Description string
	// Version is the semver version string, e.g. "1.0.0".
	Version string
	// License is the SPDX license identifier, e.g. "MIT".
	License string
	// Authors is a list of author name/email pairs.
	Authors []Author
	// PSR4Namespace is the PHP namespace root, e.g. "Acme\\MyLib\\".
	// Must end with a backslash.
	PSR4Namespace string
	// PHPRequire is the minimum PHP version constraint, e.g. "^8.4".
	PHPRequire string
	// Require is the additional composer require entries (beyond php).
	Require map[string]string
	// Keywords is the list of Packagist search keywords.
	Keywords []string
	// Homepage is the project URL for composer.json.
	Homepage string
}

// Author is a composer.json author entry.
type Author struct {
	Name  string `json:"name"`
	Email string `json:"email,omitempty"`
}

// ClassFile is one PHP source file to emit under src/.
type ClassFile struct {
	// FQCN is the fully-qualified class name, e.g. "Acme\\MyLib\\Client".
	FQCN string
	// Source is the PHP source text.
	Source string
}

// EmitResult holds the file set produced by Emit.
type EmitResult struct {
	// Files maps relative file path -> file content.
	Files map[string]string
}

// Emit generates the complete PHP library file set.
func Emit(cfg Config, classes []ClassFile) (*EmitResult, error) {
	if cfg.ComposerName == "" {
		return nil, fmt.Errorf("library: ComposerName is required")
	}
	if cfg.PSR4Namespace == "" {
		return nil, fmt.Errorf("library: PSR4Namespace is required")
	}

	result := &EmitResult{Files: make(map[string]string)}

	// composer.json
	cj, err := renderComposerJSON(cfg)
	if err != nil {
		return nil, fmt.Errorf("library: composer.json: %w", err)
	}
	result.Files["composer.json"] = cj

	// README.md
	result.Files["README.md"] = renderREADME(cfg)

	// LICENSE
	if cfg.License == "" || strings.EqualFold(cfg.License, "MIT") {
		result.Files["LICENSE"] = renderMITLicense(cfg)
	}

	// src/ tree
	ns := strings.TrimSuffix(cfg.PSR4Namespace, "\\")
	for _, cls := range classes {
		path := classFilePath(ns, cls.FQCN)
		result.Files["src/"+path] = cls.Source
	}

	return result, nil
}

// classFilePath converts a FQCN to a PSR-4 relative file path.
// Given namespace root "Acme\\MyLib" and FQCN "Acme\\MyLib\\Client",
// returns "Acme/MyLib/Client.php".
func classFilePath(_ string, fqcn string) string {
	fqcn = strings.TrimPrefix(fqcn, "\\")
	path := strings.ReplaceAll(fqcn, "\\", "/")
	return path + ".php"
}

// composerJSONFields is the ordered composer.json structure for JSON encoding.
type composerJSONFields struct {
	Name             string            `json:"name"`
	Description      string            `json:"description"`
	Type             string            `json:"type"`
	License          string            `json:"license"`
	Authors          []Author          `json:"authors,omitempty"`
	Keywords         []string          `json:"keywords,omitempty"`
	Homepage         string            `json:"homepage,omitempty"`
	Require          map[string]string `json:"require"`
	Autoload         map[string]any    `json:"autoload"`
	MinimumStability string            `json:"minimum-stability"`
}

func renderComposerJSON(cfg Config) (string, error) {
	phpReq := cfg.PHPRequire
	if phpReq == "" {
		phpReq = "^8.4"
	}
	ns := cfg.PSR4Namespace
	if !strings.HasSuffix(ns, "\\") {
		ns = ns + "\\"
	}

	require := map[string]string{
		"php": phpReq,
	}
	maps.Copy(require, cfg.Require)

	license := cfg.License
	if license == "" {
		license = "MIT"
	}

	fields := composerJSONFields{
		Name:        cfg.ComposerName,
		Description: cfg.Description,
		Type:        "library",
		License:     license,
		Authors:     cfg.Authors,
		Keywords:    cfg.Keywords,
		Homepage:    cfg.Homepage,
		Require:     require,
		Autoload: map[string]any{
			"psr-4": map[string]string{
				ns: "src/",
			},
		},
		MinimumStability: "stable",
	}

	data, err := json.MarshalIndent(fields, "", "    ")
	if err != nil {
		return "", err
	}
	return string(data) + "\n", nil
}

func renderREADME(cfg Config) string {
	name := cfg.ComposerName
	desc := cfg.Description
	if desc == "" {
		desc = "A PHP library generated by Mochi."
	}
	return fmt.Sprintf("# %s\n\n%s\n\n## Installation\n\n```bash\ncomposer require %s\n```\n", name, desc, name)
}

func renderMITLicense(cfg Config) string {
	year := time.Now().Year()
	author := "Contributors"
	if len(cfg.Authors) > 0 {
		author = cfg.Authors[0].Name
	}
	return fmt.Sprintf(`MIT License

Copyright (c) %d %s

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
`, year, author)
}
