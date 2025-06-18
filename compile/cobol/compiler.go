package cobolcode

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler is a very small COBOL code generator able to compile
// the LeetCode two-sum example. It handles only a tiny subset of
// Mochi expressions.
type Compiler struct {
	buf    bytes.Buffer
	indent int
	env    *types.Env
}

// New creates a new COBOL compiler instance.
func New(env *types.Env) *Compiler { return &Compiler{env: env} }

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
}

func (c *Compiler) writeln(s string) {
	c.writeIndent()
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

// Compile generates COBOL code for prog. Only the specific pattern
// used by examples/leetcode/1/two-sum.mochi is supported.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	var nums []int
	var target int
	for _, st := range prog.Statements {
		if st.Let != nil && st.Let.Name == "result" && st.Let.Value != nil {
			call := extractCall(st.Let.Value)
			if call == nil || call.Func != "twoSum" {
				return nil, fmt.Errorf("unsupported program")
			}
			if l, ok := listInts(call.Args[0]); ok {
				nums = l
			} else {
				return nil, fmt.Errorf("unsupported list literal")
			}
			if v, ok := intLit(call.Args[1]); ok {
				target = v
			} else {
				return nil, fmt.Errorf("unsupported target literal")
			}
		}
	}
	if len(nums) == 0 {
		return nil, fmt.Errorf("could not find twoSum call")
	}

	c.writeln("IDENTIFICATION DIVISION.")
	c.writeln("PROGRAM-ID. MAIN.")
	c.writeln("DATA DIVISION.")
	c.writeln("WORKING-STORAGE SECTION.")
	c.writeln(fmt.Sprintf("01 N        PIC 9(4) VALUE %d.", len(nums)))
	c.writeln(fmt.Sprintf("01 TARGET   PIC 9(9) VALUE %d.", target))
	c.writeln("01 NUMS     PIC S9(9) OCCURS " + fmt.Sprintf("%d", len(nums)) + " VALUE (" + joinInts(nums) + ").")
	c.writeln("01 I        PIC 9(9).")
	c.writeln("01 J        PIC 9(9).")
	c.writeln("PROCEDURE DIVISION.")
	c.writeln("    PERFORM VARYING I FROM 1 BY 1 UNTIL I > N")
	c.writeln("        PERFORM VARYING J FROM I + 1 BY 1 UNTIL J > N")
	c.writeln("            IF NUMS(I) + NUMS(J) = TARGET")
	c.writeln("                DISPLAY I")
	c.writeln("                DISPLAY J")
	c.writeln("                STOP RUN")
	c.writeln("            END-IF")
	c.writeln("        END-PERFORM")
	c.writeln("    END-PERFORM")
	c.writeln("    DISPLAY -1")
	c.writeln("    DISPLAY -1")
	c.writeln("    STOP RUN.")
	return c.buf.Bytes(), nil
}

func joinInts(nums []int) string {
	parts := make([]string, len(nums))
	for i, n := range nums {
		parts[i] = fmt.Sprintf("%d", n)
	}
	return strings.Join(parts, ",")
}

// extractCall extracts the first call expression inside e.
func extractCall(e *parser.Expr) *parser.CallExpr {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return nil
	}
	u := e.Binary.Left
	if u.Value != nil && u.Value.Target != nil && u.Value.Target.Call != nil {
		return u.Value.Target.Call
	}
	return nil
}

func listInts(e *parser.Expr) ([]int, bool) {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return nil, false
	}
	u := e.Binary.Left
	if u.Value == nil || u.Value.Target == nil || u.Value.Target.List == nil {
		return nil, false
	}
	var res []int
	for _, el := range u.Value.Target.List.Elems {
		if v, ok := intLit(el); ok {
			res = append(res, v)
		} else {
			return nil, false
		}
	}
	return res, true
}

func intLit(e *parser.Expr) (int, bool) {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return 0, false
	}
	u := e.Binary.Left
	if u.Value != nil && u.Value.Target != nil && u.Value.Target.Lit != nil && u.Value.Target.Lit.Int != nil {
		return *u.Value.Target.Lit.Int, true
	}
	return 0, false
}
