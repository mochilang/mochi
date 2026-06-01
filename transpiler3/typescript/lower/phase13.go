// Phase 13: File I/O lowering.
//
// File operations require per-runtime adaptation. A `__mochi_runtime`
// detection block is emitted in the prelude, then each operation
// delegates to the runtime-specific API.
//
// ReadFileExpr  → mochi_read_file(path)
// WriteFileStmt → mochi_write_file(path, content)
// AppendFileStmt → mochi_append_file(path, content)
// LinesExpr     → mochi_lines(path)
package lower

import (
	"fmt"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/typescript/tstree"
)

func (l *lowerer) lowerReadFileExpr(e *aotir.ReadFileExpr) (tstree.Expr, error) {
	l.runtime.needsFileIO = true
	path, err := l.lowerExpr(e.Path)
	if err != nil {
		return nil, fmt.Errorf("ts lower: read file path: %w", err)
	}
	return &tstree.CallExpr{
		Callee: &tstree.IdentExpr{Name: "mochi_read_file"},
		Args:   []tstree.Expr{path},
	}, nil
}

func (l *lowerer) lowerWriteFileStmt(s *aotir.WriteFileStmt) ([]tstree.Stmt, error) {
	l.runtime.needsFileIO = true
	path, err := l.lowerExpr(s.Path)
	if err != nil {
		return nil, fmt.Errorf("ts lower: write file path: %w", err)
	}
	content, err := l.lowerExpr(s.Content)
	if err != nil {
		return nil, fmt.Errorf("ts lower: write file content: %w", err)
	}
	return []tstree.Stmt{
		&tstree.ExprStmt{Expr: &tstree.CallExpr{
			Callee: &tstree.IdentExpr{Name: "mochi_write_file"},
			Args:   []tstree.Expr{path, content},
		}},
	}, nil
}

func (l *lowerer) lowerAppendFileStmt(s *aotir.AppendFileStmt) ([]tstree.Stmt, error) {
	l.runtime.needsFileIO = true
	path, err := l.lowerExpr(s.Path)
	if err != nil {
		return nil, fmt.Errorf("ts lower: append file path: %w", err)
	}
	content, err := l.lowerExpr(s.Content)
	if err != nil {
		return nil, fmt.Errorf("ts lower: append file content: %w", err)
	}
	return []tstree.Stmt{
		&tstree.ExprStmt{Expr: &tstree.CallExpr{
			Callee: &tstree.IdentExpr{Name: "mochi_append_file"},
			Args:   []tstree.Expr{path, content},
		}},
	}, nil
}

func (l *lowerer) lowerLinesExpr(e *aotir.LinesExpr) (tstree.Expr, error) {
	l.runtime.needsFileIO = true
	path, err := l.lowerExpr(e.Path)
	if err != nil {
		return nil, fmt.Errorf("ts lower: lines path: %w", err)
	}
	return &tstree.CallExpr{
		Callee: &tstree.IdentExpr{Name: "mochi_lines"},
		Args:   []tstree.Expr{path},
	}, nil
}

// fileIODecls emits the file I/O runtime helpers when needsFileIO is set.
func (l *lowerer) fileIODecls() []tstree.Decl {
	if !l.runtime.needsFileIO {
		return nil
	}
	return []tstree.Decl{&tstree.RawDecl{Text: mochiFileIOHelpers}}
}

const mochiFileIOHelpers = `const __mochi_runtime = typeof Deno !== "undefined" ? "deno"
    : typeof Bun !== "undefined" ? "bun"
    : "node";
function mochi_read_file(path: string): string {
    if (__mochi_runtime === "deno") return (Deno as any).readTextFileSync(path).trimEnd();
    // @ts-ignore
    return require("fs").readFileSync(path, "utf8").trimEnd();
}
function mochi_write_file(path: string, content: string): void {
    if (__mochi_runtime === "deno") { (Deno as any).writeTextFileSync(path, content); return; }
    // @ts-ignore
    require("fs").writeFileSync(path, content, { flag: "w" });
}
function mochi_append_file(path: string, content: string): void {
    if (__mochi_runtime === "deno") { (Deno as any).writeTextFileSync(path, content, { append: true }); return; }
    // @ts-ignore
    require("fs").appendFileSync(path, content);
}
function mochi_lines(path: string): string[] {
    return mochi_read_file(path).split("\n");
}`
