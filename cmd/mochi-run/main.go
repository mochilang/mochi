package main

import (
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strings"

	"mochi/ast"
	"mochi/interpreter"
	"mochi/parser"
	"mochi/types"
)

var version = "dev" // Set via -ldflags "-X main.version=..."

func main() {
	fmt.Printf("📦 Mochi Examples Generator (v%s)\n", version)

	const exampleDir = "examples"
	const llmDir = "llm"

	exampleOut := filepath.Join(llmDir, fmt.Sprintf("examples.v%s.md", version))
	llmOut := filepath.Join(llmDir, fmt.Sprintf("llm.v%s.md", version))
	grammarOut := filepath.Join(llmDir, "grammar.ebnf")

	if err := os.MkdirAll(llmDir, 0755); err != nil {
		exitf("❌ Failed to create %s: %v", llmDir, err)
	}

	files, err := listExampleFiles(exampleDir)
	if err != nil {
		exitf("❌ Failed to list example files: %v", err)
	}

	// Generate Examples Markdown
	if err := writeFile(exampleOut, buildMarkdown(exampleDir, files)); err != nil {
		exitf("❌ Failed to write %s: %v", exampleOut, err)
	}
	fmt.Printf("✅ Generated %s with %d example(s)\n", exampleOut, len(files))

	// Generate LLM Source Markdown
	if err := generateLLMMarkdown(llmOut); err != nil {
		exitf("❌ Failed to write %s: %v", llmOut, err)
	}
	fmt.Printf("✅ Generated %s for LLM processing\n", llmOut)

	// Generate EBNF Grammar
	if err := writeFile(grammarOut, parser.Parser.String()); err != nil {
		exitf("❌ Failed to write %s: %v", grammarOut, err)
	}
	fmt.Printf("✅ Generated %s for grammar EBNF\n", grammarOut)

	// Copy latest
	copyFile(exampleOut, filepath.Join(llmDir, "examples.latest.md"))
	copyFile(llmOut, filepath.Join(llmDir, "llm.latest.md"))
}

func listExampleFiles(root string) ([]string, error) {
	var files []string
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil || d.IsDir() {
			return err
		}
		if filepath.Ext(path) == ".mochi" {
			files = append(files, path)
		}
		return nil
	})
	sort.Strings(files)
	return files, err
}

func buildMarkdown(exampleDir string, files []string) string {
	var sb strings.Builder
	var errors []string
	var section strings.Builder

	sb.WriteString("# Mochi Examples Index\n\n")
	fmt.Fprintf(&sb, "> 📦 Generated with [Mochi Compiler](https://github.com/mochi-lang/mochi) `v%s`\n\n", version)
	sb.WriteString("Each example includes source code, AST, and runtime output.\n\n")

	sb.WriteString("## 📚 Files\n\n")
	for _, file := range files {
		rel, _ := filepath.Rel(exampleDir, file)
		anchor := strings.ReplaceAll(strings.TrimSuffix(strings.ToLower(rel), ".mochi"), string(os.PathSeparator), "-")
		fmt.Fprintf(&sb, "- [%s](#%s)\n", rel, anchor)
	}
	sb.WriteString("\n---\n")

	for _, file := range files {
		fmt.Printf("🔍 Processing %s\n", file)
		if err := appendExampleSection(&section, file); err != nil {
			fmt.Fprintf(os.Stderr, "❌ %s: %v\n", file, err)
			errors = append(errors, fmt.Sprintf("- `%s`: %v", file, err))
		}
	}

	if len(errors) > 0 {
		sb.WriteString("## ⚠️ Errors\n\nThe following files failed to compile or run:\n\n")
		for _, err := range errors {
			sb.WriteString(err + "\n")
		}
		sb.WriteString("\n---\n\n")
	}

	sb.WriteString(section.String())
	return sb.String()
}

func appendExampleSection(sb *strings.Builder, path string) error {
	rel, _ := filepath.Rel("examples", path)

	source, err := os.ReadFile(path)
	if err != nil {
		fmt.Fprintf(sb, "## %s\n\n❌ Failed to read file: %v\n", rel, err)
		return err
	}

	fmt.Fprintf(sb, "\n## %s\n\n", rel)
	sb.WriteString("<details>\n<summary>View Source, AST, and Output</summary>\n\n")

	sb.WriteString("#### 📄 Source\n```mochi\n")
	sb.WriteString(strings.TrimSpace(string(source)))
	sb.WriteString("\n```\n\n")

	prog, err := parser.Parse(path)
	if err != nil {
		sb.WriteString("#### ❌ Parse Error\n```text\n")
		sb.WriteString(err.Error() + "\n")
		sb.WriteString("```\n</details>\n")
		return fmt.Errorf("parse error: %w", err)
	}

	sb.WriteString("#### 🌲 AST\n```lisp\n")
	sb.WriteString(ast.FromProgram(prog).String())
	sb.WriteString("```\n")

	sb.WriteString("#### ▶️ Output\n```text\n")
	output, runErr := runProgram(prog)
	sb.WriteString(output)
	sb.WriteString("```\n</details>\n")

	return runErr
}

func runProgram(prog *parser.Program) (string, error) {
	output := &strings.Builder{}
	env := types.NewEnv(nil)

	typeErrors := types.Check(prog, env)
	if len(typeErrors) > 0 {
		fmt.Fprintf(output, "🧠 Type Check Failed\n\n")
		for i, err := range typeErrors {
			fmt.Fprintf(output, "  %2d. %v\n", i+1, err)
		}
		return output.String(), fmt.Errorf("type error (%d issue(s))", len(typeErrors))
	}

	interp := interpreter.New(prog, env)
	interp.Env().SetWriter(output)

	if err := interp.Run(); err != nil {
		fmt.Fprintf(output, "💥 Runtime Error\n\n  → %v\n", err)
		return output.String(), fmt.Errorf("runtime error: %w", err)
	}

	return output.String(), nil
}

func generateLLMMarkdown(outPath string) error {
	type sourceFile struct {
		Path, Header, Description string
	}

	sources := []sourceFile{
		{"cmd/mochi/main.go", "## cmd/mochi/main.go", "Entry point of the Mochi CLI tool."},
		{"diagnostic/diagnostic.go", "## diagnostic/diagnostic.go", "Structured error reporting with source hints."},
		{"parser/parser.go", "## parser/parser.go", "Top-down recursive descent parser."},
		{"parser/errors.go", "## parser/errors.go", "Parser error handling logic."},
		{"ast/ast.go", "## ast/ast.go", "AST node types."},
		{"ast/convert.go", "## ast/convert.go", "Converts parser output to typed AST."},
		{"ast/pretty.go", "## ast/pretty.go", "Pretty-print AST in Lisp-style format."},
		{"types/env.go", "## types/env.go", "Type environment and symbol resolution."},
		{"types/check.go", "## types/check.go", "Static type checker implementation."},
		{"types/errors.go", "## types/errors.go", "Type checker error definitions."},
		{"interpreter/interpreter.go", "## interpreter/interpreter.go", "Interpreter that executes AST."},
		{"interpreter/errors.go", "## interpreter/errors.go", "Runtime error types."},
		{"golden/golden.go", "## golden/golden.go", "Shared golden file test framework."},
	}

	var sb strings.Builder
	sb.WriteString("// generated by mochi-run, do not edit\n\n")
	fmt.Fprintf(&sb, "# Mochi Source Files for LLM Ingestion\n\n> Version: `%s`\n\n", version)
	sb.WriteString("This document includes all core source files used by the Mochi interpreter and runtime toolchain.\n\n")
	sb.WriteString("## 📖 Overview\n\n")
	for _, src := range sources {
		fmt.Fprintf(&sb, "- **%s**: %s\n", src.Path, src.Description)
	}
	sb.WriteString("\n---\n\n")

	for _, src := range sources {
		sb.WriteString(src.Header + "\n\n")
		data, err := os.ReadFile(src.Path)
		if err != nil {
			return fmt.Errorf("failed to read %s: %w", src.Path, err)
		}
		sb.WriteString("```go\n")
		sb.WriteString(string(data))
		sb.WriteString("\n```\n\n")
	}

	return writeFile(outPath, sb.String())
}

func writeFile(path, content string) error {
	return os.WriteFile(path, []byte(content), 0644)
}

func copyFile(src, dst string) {
	in, err := os.Open(src)
	if err != nil {
		fmt.Fprintf(os.Stderr, "❌ Failed to open %s: %v\n", src, err)
		return
	}
	defer func() {
		if err := in.Close(); err != nil {
			fmt.Fprintf(os.Stderr, "⚠️ Failed to close %s: %v\n", src, err)
		}
	}()

	out, err := os.Create(dst)
	if err != nil {
		fmt.Fprintf(os.Stderr, "❌ Failed to create %s: %v\n", dst, err)
		return
	}
	defer func() {
		if err := out.Close(); err != nil {
			fmt.Fprintf(os.Stderr, "⚠️ Failed to close %s: %v\n", dst, err)
		}
	}()

	if _, err := io.Copy(out, in); err != nil {
		fmt.Fprintf(os.Stderr, "❌ Failed to copy %s to %s: %v\n", src, dst, err)
	}
}

func exitf(format string, args ...any) {
	fmt.Fprintf(os.Stderr, format+"\n", args...)
	os.Exit(1)
}
