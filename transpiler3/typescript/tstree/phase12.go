// Phase 12 adds TryCatchStmt for Mochi error handling lowering.
package tstree

import "strings"

// TryCatchStmt is `try { TryBody } catch (CatchVar: unknown) { CatchBody }`.
type TryCatchStmt struct {
	TryBody   []Stmt
	CatchVar  string
	CatchBody []Stmt
}

func (s *TryCatchStmt) stmtNode() {}
func (s *TryCatchStmt) TsString(indent int) string {
	pad := strings.Repeat("  ", indent)
	var b strings.Builder
	b.WriteString(pad)
	b.WriteString("try {")
	if len(s.TryBody) > 0 {
		b.WriteByte('\n')
		for _, stmt := range s.TryBody {
			b.WriteString(stmt.TsString(indent + 1))
			b.WriteByte('\n')
		}
		b.WriteString(pad)
	}
	b.WriteString("} catch (")
	b.WriteString(s.CatchVar)
	b.WriteString(": unknown) {")
	if len(s.CatchBody) > 0 {
		b.WriteByte('\n')
		for _, stmt := range s.CatchBody {
			b.WriteString(stmt.TsString(indent + 1))
			b.WriteByte('\n')
		}
		b.WriteString(pad)
	}
	b.WriteByte('}')
	return b.String()
}
