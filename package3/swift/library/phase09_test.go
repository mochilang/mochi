package library_test

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/package3/swift/library"
)

func TestPhase9LibraryEmit(t *testing.T) {
	outDir := t.TempDir()

	pkg := &library.SwiftPackage{
		Name:    "MyMochiLib",
		Version: "1.0.0",
		Functions: []library.SwiftFunction{
			{Name: "greet", ParamTypes: []string{"String"}, ReturnType: "String"},
			{Name: "add", ParamTypes: []string{"Int32", "Int32"}, ReturnType: "Int32"},
			{Name: "toggle", ParamTypes: []string{"Bool"}, ReturnType: "Bool"},
		},
	}

	if err := library.EmitPackage(pkg, outDir); err != nil {
		t.Fatalf("EmitPackage error: %v", err)
	}

	// Check Package.swift
	pkgSwift, err := os.ReadFile(filepath.Join(outDir, "Package.swift"))
	if err != nil {
		t.Fatalf("Package.swift not created: %v", err)
	}
	pkgContent := string(pkgSwift)
	if !strings.Contains(pkgContent, "swift-tools-version: 5.9") {
		t.Errorf("Package.swift missing swift-tools-version: 5.9")
	}
	if !strings.Contains(pkgContent, `"MyMochiLib"`) {
		t.Errorf("Package.swift missing package name")
	}
	if !strings.Contains(pkgContent, ".macOS") {
		t.Errorf("Package.swift missing macOS platform")
	}

	// Check MochiExports.swift
	exports, err := os.ReadFile(filepath.Join(outDir, "Sources", "MochiExports", "MochiExports.swift"))
	if err != nil {
		t.Fatalf("MochiExports.swift not created: %v", err)
	}
	exportContent := string(exports)
	if !strings.Contains(exportContent, "@_cdecl") {
		t.Errorf("MochiExports.swift missing @_cdecl")
	}
	if !strings.Contains(exportContent, "__mochi_greet") {
		t.Errorf("MochiExports.swift missing __mochi_greet")
	}
	if !strings.Contains(exportContent, "UnsafeMutablePointer<CChar>") {
		t.Errorf("MochiExports.swift missing String C type")
	}
	if !strings.Contains(exportContent, "import Foundation") {
		t.Errorf("MochiExports.swift missing Foundation import")
	}
	if !strings.Contains(exportContent, "__mochi_add") {
		t.Errorf("MochiExports.swift missing __mochi_add")
	}
	if !strings.Contains(exportContent, "Int32") {
		t.Errorf("MochiExports.swift missing Int32 type")
	}

	// Check README.md
	readme, err := os.ReadFile(filepath.Join(outDir, "README.md"))
	if err != nil {
		t.Fatalf("README.md not created: %v", err)
	}
	readmeContent := string(readme)
	if !strings.Contains(readmeContent, "MyMochiLib") {
		t.Errorf("README.md missing package name")
	}
	if !strings.Contains(readmeContent, "greet") {
		t.Errorf("README.md missing function list")
	}
}

func TestPhase9LibraryEmitEmpty(t *testing.T) {
	outDir := t.TempDir()
	pkg := &library.SwiftPackage{
		Name:      "EmptyPkg",
		Version:   "0.1.0",
		Functions: nil,
	}
	if err := library.EmitPackage(pkg, outDir); err != nil {
		t.Fatalf("EmitPackage error: %v", err)
	}
	// All three files should still be created
	for _, rel := range []string{
		"Package.swift",
		"Sources/MochiExports/MochiExports.swift",
		"README.md",
	} {
		if _, err := os.Stat(filepath.Join(outDir, rel)); err != nil {
			t.Errorf("file not created: %s", rel)
		}
	}
}
