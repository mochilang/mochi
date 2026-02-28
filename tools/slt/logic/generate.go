package logic

import (
	"encoding/json"
	"fmt"
	"math"
	"reflect"
	"sort"
	"strconv"
	"strings"

	sqlparser "github.com/xwb1989/sqlparser"
)

// subqueryToMochi converts supported subqueries to Mochi source. The outer
// parameter names the variable for the outer query row when handling correlated
// predicates.
func subqueryToMochi(sub *sqlparser.Subquery, outer string) string {
	sel, ok := sub.Select.(*sqlparser.Select)
	if !ok || sel == nil || len(sel.From) != 1 || len(sel.SelectExprs) != 1 {
		return "null"
	}
	tblExpr, ok := sel.From[0].(*sqlparser.AliasedTableExpr)
	if !ok {
		return "null"
	}
	tblName, ok := tblExpr.Expr.(sqlparser.TableName)
	if !ok {
		return "null"
	}
	alias := tblExpr.As.String()
	if alias == "" {
		alias = "x"
	}
	ae, ok := sel.SelectExprs[0].(*sqlparser.AliasedExpr)
	if !ok {
		return "null"
	}
	if fn, ok := ae.Expr.(*sqlparser.FuncExpr); ok {
		name := strings.ToLower(fn.Name.String())
		if name == "count" && len(fn.Exprs) == 1 {
			if _, ok := fn.Exprs[0].(*sqlparser.StarExpr); ok {
				cond := ""
				if sel.Where != nil {
					cond = condExprToMochiRow(sel.Where.Expr, alias, outer, nil)
				}
				var sb strings.Builder
				sb.WriteString("count(from " + alias + " in " + tblName.Name.String())
				if cond != "" {
					sb.WriteString("\n  where " + cond)
				}
				sb.WriteString("\n  select " + alias + ")")
				return sb.String()
			}
		} else if name == "avg" && len(fn.Exprs) == 1 {
			if a, ok := fn.Exprs[0].(*sqlparser.AliasedExpr); ok {
				val := exprToMochiRow(a.Expr, alias, outer, nil)
				cond := ""
				if sel.Where != nil {
					cond = condExprToMochiRow(sel.Where.Expr, alias, outer, nil)
				}
				var sb strings.Builder
				sb.WriteString("avg(from " + alias + " in " + tblName.Name.String())
				if cond != "" {
					sb.WriteString("\n  where " + cond)
				}
				sb.WriteString("\n  select " + val + ")")
				return sb.String()
			}
		}
	}
	return "null"
}

func existsToMochi(sub *sqlparser.Subquery, outer string) string {
	sel, ok := sub.Select.(*sqlparser.Select)
	if !ok || sel == nil || len(sel.From) != 1 {
		return ""
	}
	tblExpr, ok := sel.From[0].(*sqlparser.AliasedTableExpr)
	if !ok {
		return ""
	}
	tblName, ok := tblExpr.Expr.(sqlparser.TableName)
	if !ok {
		return ""
	}
	alias := tblExpr.As.String()
	if alias == "" {
		alias = "x"
	}
	cond := ""
	if sel.Where != nil {
		cond = condExprToMochiRow(sel.Where.Expr, alias, outer, nil)
	}
	var sb strings.Builder
	sb.WriteString("count(from " + alias + " in " + tblName.Name.String())
	if cond != "" {
		sb.WriteString("\n  where " + cond)
	}
	sb.WriteString("\n  select " + alias + ") > 0")
	return sb.String()
}

// Format the value for Mochi source.
func formatValue(v any) string {
	switch t := v.(type) {
	case nil:
		return "null"
	case string:
		lower := strings.ToLower(t)
		if lower == "true" {
			return "\"tru\" + \"e\""
		}
		if lower == "false" {
			return "\"fal\" + \"se\""
		}
		return strconv.Quote(t)
	case bool:
		if t {
			return "true"
		}
		return "false"
	case int, int8, int16, int32, int64:
		return fmt.Sprintf("%d", t)
	case uint, uint8, uint16, uint32, uint64:
		return fmt.Sprintf("%d", t)
	case float32:
		f := float64(t)
		if f == math.Trunc(f) {
			return fmt.Sprintf("%.0f", f)
		}
		return fmt.Sprintf("%g", f)
	case float64:
		if t == math.Trunc(t) {
			return fmt.Sprintf("%.0f", t)
		}
		return fmt.Sprintf("%g", t)
	default:
		return fmt.Sprintf("%v", t)
	}
}

// constNotNull reports whether e is a constant expression that is not NULL.
// If so, the Mochi representation of the constant is returned.
func constNotNull(e sqlparser.Expr) (string, bool) {
	switch v := e.(type) {
	case *sqlparser.NullVal:
		return "", false
	case *sqlparser.SQLVal:
		return exprToMochiRow(v, "row", "", nil), true
	case *sqlparser.ParenExpr:
		return constNotNull(v.Expr)
	case *sqlparser.UnaryExpr:
		if s, ok := constNotNull(v.Expr); ok {
			if v.Operator == "+" {
				return s, true
			}
			return fmt.Sprintf("(%s(%s))", v.Operator, s), true
		}
	}
	return "", false
}

func detectColumnType(rows []map[string]any, name string, declared []string, cols []string) string {
	// First use declared type information if available.
	for i, c := range cols {
		if c == name && i < len(declared) {
			typ := strings.ToLower(declared[i])
			switch {
			case strings.Contains(typ, "tinyint(1)"):
				// MySQL often uses TINYINT(1) to represent booleans.
				return "bool"
			case strings.Contains(typ, "serial"):
				// SERIAL/BIGSERIAL columns behave like integers.
				return "int"
			case strings.Contains(typ, "int"):
				return "int"
			case strings.Contains(typ, "real"), strings.Contains(typ, "float"), strings.Contains(typ, "double"), strings.Contains(typ, "numeric"), strings.Contains(typ, "decimal"):
				return "float"
			case strings.Contains(typ, "bool"), strings.Contains(typ, "bit"):
				return "bool"
			case strings.Contains(typ, "char"), strings.Contains(typ, "text"),
				strings.Contains(typ, "clob"), strings.Contains(typ, "string"),
				strings.Contains(typ, "varchar"), strings.Contains(typ, "varying"),
				strings.Contains(typ, "blob"), strings.Contains(typ, "binary"),
				strings.Contains(typ, "date"), strings.Contains(typ, "time"),
				strings.Contains(typ, "timestamp"):
				return "string"
			}
		}
	}

	// Fall back to inspecting row values.
	t := ""
	floatIsInt := true
	boolLike := true
	seenZero := false
	seenOne := false
	seenNegOne := false
	hasNull := false
	for _, row := range rows {
		v := row[name]
		if v == nil {
			hasNull = true
			continue
		}
		switch val := v.(type) {
		case string:
			// Attempt to infer numeric or boolean types when the
			// value is stored as a string. Trim whitespace and
			// normalize case for comparison. Treat the literal
			// string "null" or "nil" as a NULL value. Handle
			// special floating point values like "infinity" and
			// "nan" which DuckDB may return.
			sv := strings.TrimSpace(strings.ToLower(val))
			if sv == "null" || sv == "nil" || sv == "none" || sv == "undefined" {
				continue
			}
			if sv == "nan" || sv == "+nan" || sv == "-nan" {
				continue
			}

			// Allow common separators in numbers.
			sv = strings.ReplaceAll(sv, ",", "")
			sv = strings.ReplaceAll(sv, "_", "")
			sv = strings.ReplaceAll(sv, "'", "")
			sv = strings.ReplaceAll(sv, " ", "")
			if sv == "inf" || sv == "+inf" || sv == "-inf" || sv == "infinity" {
				if t == "" || t == "float" {
					t = "float"
				} else {
					return "any"
				}
				continue
			}

			if sv == "true" || sv == "t" || sv == "yes" || sv == "y" || sv == "on" {
				if t == "" || t == "bool" || t == "int" || t == "float" {
					t = "bool"
				} else {
					return "any"
				}
				seenOne = true
				continue
			}
			if sv == "false" || sv == "f" || sv == "no" || sv == "n" || sv == "off" {
				if t == "" || t == "bool" || t == "int" || t == "float" {
					t = "bool"
				} else {
					return "any"
				}
				seenZero = true
				continue
			}
			if sv == "inf" || sv == "+inf" || sv == "-inf" || sv == "infinity" || sv == "+infinity" || sv == "-infinity" || sv == "nan" {
				if t == "" || t == "float" || t == "int" {
					t = "float"
				} else {
					return "any"
				}
				boolLike = false
				continue
			}

			if sv == "inf" || sv == "+inf" || sv == "infinity" || sv == "+infinity" {
				if t == "" || t == "float" || t == "int" {
					t = "float"
				} else {
					return "any"
				}
				boolLike = false
				seenOne = false
				seenZero = false
				continue
			}
			if sv == "-inf" || sv == "-infinity" {
				if t == "" || t == "float" || t == "int" {
					t = "float"
				} else {
					return "any"
				}
				boolLike = false
				seenNegOne = false
				continue
			}
			if sv == "nan" {
				if t == "" || t == "float" || t == "int" {
					t = "float"
				} else {
					return "any"
				}
				boolLike = false
				continue
			}

			clean := strings.ReplaceAll(strings.ReplaceAll(sv, ",", ""), "_", "")
			if len(clean) > 1 {
				last := clean[len(clean)-1]
				if last == 'l' || last == 'L' || last == 'f' || last == 'F' {
					clean = clean[:len(clean)-1]
				}
			}
			orig := clean
			neg := false
			if strings.HasPrefix(clean, "+") || strings.HasPrefix(clean, "-") {
				neg = strings.HasPrefix(clean, "-")
				clean = clean[1:]
			}
			// Support hexadecimal and binary integer literals.
			if strings.HasPrefix(clean, "0x") || strings.HasPrefix(clean, "0X") {
				if v, err := strconv.ParseInt(clean[2:], 16, 64); err == nil {
					if neg {
						v = -v
					}
					if v == 0 {
						seenZero = true
					} else if v == 1 {
						seenOne = true
					} else if v == -1 {
						seenNegOne = true
					} else {
						boolLike = false
					}
					if t == "" {
						t = "int"
					} else if t != "int" {
						if t == "float" {
							// allow ints in float column
						} else {
							return "any"
						}
					}
					continue
				}
			} else if strings.HasPrefix(clean, "0b") || strings.HasPrefix(clean, "0B") {
				if v, err := strconv.ParseInt(clean[2:], 2, 64); err == nil {
					if neg {
						v = -v
					}
					if v == 0 {
						seenZero = true
					} else if v == 1 {
						seenOne = true
					} else if v == -1 {
						seenNegOne = true
					} else {
						boolLike = false
					}
					if t == "" {
						t = "int"
					} else if t != "int" {
						if t == "float" {
							// allow ints in float column
						} else {
							return "any"
						}
					}
					continue
				}
			} else if strings.HasPrefix(clean, "0o") || strings.HasPrefix(clean, "0O") {
				if v, err := strconv.ParseInt(clean[2:], 8, 64); err == nil {
					if neg {
						v = -v
					}
					if v == 0 {
						seenZero = true
					} else if v == 1 {
						seenOne = true
					} else if v == -1 {
						seenNegOne = true
					} else {
						boolLike = false
					}
					if t == "" {
						t = "int"
					} else if t != "int" {
						if t == "float" {
							// allow ints in float column
						} else {
							return "any"
						}
					}
					continue
				}
			}
			if f, err := strconv.ParseFloat(orig, 64); err == nil {
				hasDec := strings.ContainsAny(clean, ".eE")
				if f == math.Trunc(f) {
					if t == "" {
						if hasDec {
							t = "float"
						} else {
							t = "int"
						}
					}
					if f == 0 {
						seenZero = true
					} else if f == 1 {
						seenOne = true
					} else {
						boolLike = false
					}
					if hasDec {
						floatIsInt = floatIsInt && f == math.Trunc(f)
					}
				} else {
					if t == "" || t == "float" || t == "int" {
						t = "float"
					} else {
						return "any"
					}
					if f == 0 {
						seenZero = true
					} else if f == 1 {
						seenOne = true
					} else {
						boolLike = false
					}
					floatIsInt = false
				}
				continue
			}

			if t == "" || t == "string" {
				t = "string"
			} else {
				return "any"
			}
		case []byte:
			if t == "" || t == "string" {
				t = "string"
			} else {
				return "any"
			}
		case float32:
			f := float64(val)
			if math.IsInf(f, 0) || math.IsNaN(f) {
				boolLike = false
				if t == "" || t == "float" || t == "int" {
					t = "float"
				} else {
					return "any"
				}
				break
			}
			if f != math.Trunc(f) {
				floatIsInt = false
			}
			if f == 0 {
				seenZero = true
			} else if f == 1 {
				seenOne = true
			} else if f == -1 {
				seenNegOne = true
			} else {
				boolLike = false
			}
			if t == "bool" {
				if f == 0 || f == 1 || f == -1 {
					break
				}
				t = ""
			}
			if t == "" || t == "float" || t == "int" {
				if t == "" {
					t = "float"
				} else if t == "int" && f != math.Trunc(f) {
					t = "float"
				}
			} else {
				return "any"
			}
		case float64:
			if math.IsInf(val, 0) || math.IsNaN(val) {
				boolLike = false
				if t == "" || t == "float" || t == "int" {
					t = "float"
				} else {
					return "any"
				}
				break
			}
			if val != math.Trunc(val) {
				floatIsInt = false
			}
			if val == 0 {
				seenZero = true
			} else if val == 1 {
				seenOne = true
			} else if val == -1 {
				seenNegOne = true
			} else {
				boolLike = false
			}
			if t == "bool" {
				if val == 0 || val == 1 || val == -1 {
					// allow numeric booleans mixed with true/false strings
					break
				}
				// previously typed as bool but encountered a non-boolean numeric value
				t = ""
			}
			if t == "" || t == "float" || t == "int" {
				if t == "" {
					t = "float"
				} else if t == "int" && val != math.Trunc(val) {
					// upgrade int columns to float when a
					// non-integer value is seen
					t = "float"
				}
			} else {
				return "any"
			}
		case int:
			if val == 0 {
				seenZero = true
			} else if val == 1 {
				seenOne = true
			} else if val == -1 {
				seenNegOne = true
			} else {
				boolLike = false
			}
			if t == "bool" {
				if val == 0 || val == 1 || val == -1 {
					// allow numeric booleans mixed with true/false strings
					break
				}
				t = ""
			}
			if t == "" {
				t = "int"
			} else if t != "int" {
				if t == "float" {
					// allow ints in float column
				} else {
					return "any"
				}
			}
		case uint, uint8, uint16, uint32, uint64:
			iv := reflect.ValueOf(val).Uint()
			if iv == 0 {
				seenZero = true
			} else if iv == 1 {
				seenOne = true
			} else {
				boolLike = false
			}
			if t == "bool" {
				break
			}
			if t == "" {
				t = "int"
			} else if t != "int" {
				if t == "float" {
					// allow ints in float column
				} else {
					return "any"
				}
			}
		case json.Number:
			num := val
			if i, err := num.Int64(); err == nil {
				if i == 0 {
					seenZero = true
				} else if i == 1 {
					seenOne = true
				} else {
					boolLike = false
				}
				if t == "" {
					t = "int"
				} else if t != "int" {
					if t == "float" {
						// allow ints in float column
					} else {
						return "any"
					}
				}
				break
			}
			if f, err := num.Float64(); err == nil {
				if f != math.Trunc(f) {
					floatIsInt = false
				}
				if f == 0 {
					seenZero = true
				} else if f == 1 {
					seenOne = true
				} else {
					boolLike = false
				}
				if t == "" || t == "float" || t == "int" {
					if t == "" {
						t = "float"
					} else if t == "int" && f != math.Trunc(f) {
						t = "float"
					}
				} else {
					return "any"
				}
				break
			}
		case bool:
			if t == "" || t == "bool" {
				t = "bool"
			} else {
				return "any"
			}
			if val {
				seenOne = true
			} else {
				seenZero = true
			}
		default:
			return "any"
		}
	}
	if t == "float" && floatIsInt {
		t = "int"
	}

	if (t == "int" || t == "float") && boolLike && (seenZero || seenOne || seenNegOne) {
		// Columns that only contain 0/1 values are best represented as
		// booleans. Handle both integer and floating point values so
		// that strings like "0.0" are recognized as bools.
		t = "bool"
	}

	if t == "" && boolLike && (seenZero || seenOne || seenNegOne) {
		return "bool"
	}

	if hasNull {
		if (t == "int" || t == "float") && boolLike && (seenZero || seenOne || seenNegOne) {
			// Treat nullable 0/1 columns as bool to avoid type mismatches.
			return "bool"
		}
		if t == "" {
			return "any"
		}
		return t
	}

	if t == "" {
		return "any"
	}

	return t
}

func exprToMochi(e sqlparser.Expr, subs map[string]string) string {
	return exprToMochiRow(e, "row", "", subs)
}

func exprToMochiBare(e sqlparser.Expr) string {
	s := exprToMochiRow(e, "row", "", nil)
	return strings.ReplaceAll(s, "row.", "")
}

// repeatedAddCol checks whether the expression is a chain of additions of the
// same column. If so it returns that column and the number of repetitions.
func repeatedAddCol(e sqlparser.Expr) (*sqlparser.ColName, int, bool) {
	switch v := e.(type) {
	case *sqlparser.ColName:
		return v, 1, true
	case *sqlparser.BinaryExpr:
		if v.Operator != "+" {
			return nil, 0, false
		}
		lcol, ln, lok := repeatedAddCol(v.Left)
		rcol, rn, rok := repeatedAddCol(v.Right)
		if !lok || !rok {
			return nil, 0, false
		}
		if lcol.Name.Equal(rcol.Name) && lcol.Qualifier.Name.String() == rcol.Qualifier.Name.String() {
			return lcol, ln + rn, true
		}
	}
	return nil, 0, false
}

// binaryPrec returns a relative precedence for SQL binary operators. Higher
// numbers bind more tightly. Only the operators used in the SLT queries are
// handled here.
func binaryPrec(op string) int {
	switch op {
	case "*", "/", "%":
		return 3
	case "+", "-":
		return 2
	default:
		return 1
	}
}

func exprToMochiRow(e sqlparser.Expr, rowVar, outer string, subs map[string]string) string {
	switch v := e.(type) {
	case *sqlparser.SQLVal:
		switch v.Type {
		case sqlparser.StrVal:
			return fmt.Sprintf("\"%s\"", string(v.Val))
		case sqlparser.IntVal:
			// Preserve integer constants without adding a decimal
			// suffix. Emitting integers avoids type mismatches in
			// generated programs that mix integer columns with
			// constant values. Wrap negative numbers in parentheses
			// to avoid parsing issues.
			s := string(v.Val)
			if strings.HasPrefix(s, "-") {
				return "(" + s + ")"
			}
			return s
		case sqlparser.FloatVal:
			s := string(v.Val)
			if f, err := strconv.ParseFloat(s, 64); err == nil {
				if f == math.Trunc(f) {
					return fmt.Sprintf("%.0f", f)
				}
			}
			return s
		}
	case *sqlparser.ColName:
		name := v.Name.String()
		if v.Qualifier.Name.String() != "" && v.Qualifier.Name.String() != rowVar {
			if outer != "" {
				return fmt.Sprintf("%s.%s", outer, name)
			}
		}
		return fmt.Sprintf("%s.%s", rowVar, name)
	case *sqlparser.ParenExpr:
		return "(" + exprToMochiRow(v.Expr, rowVar, outer, subs) + ")"
	case *sqlparser.UnaryExpr:
		ex := exprToMochiRow(v.Expr, rowVar, outer, subs)
		if v.Operator == "+" {
			return ex
		}
		// Wrap unary operators in an extra set of parentheses. The
		// Mochi parser struggles with expressions like "* -(x)" when the
		// unary operator directly follows another operator. Wrapping the
		// whole expression avoids this parse ambiguity.
		return fmt.Sprintf("(%s(%s))", v.Operator, ex)
	case *sqlparser.BinaryExpr:
		// Simplify repeated addition of the same column expression
		// into a multiplication.  SQLLogicTest queries often use
		// patterns like `a+a+a` which would otherwise result in
		// deeply nested addition chains and type mismatches.  Any
		// chain of `+` operations that references the exact same
		// column is collapsed to `(col * N)`.
		if v.Operator == "+" {
			if col, n, ok := repeatedAddCol(v); ok && n > 1 {
				name := exprToMochiRow(col, rowVar, outer, subs)
				return fmt.Sprintf("(%s * %d)", name, n)
			}
			if u, ok := v.Right.(*sqlparser.UnaryExpr); ok && u.Operator == "-" {
				r := exprToMochiRow(u.Expr, rowVar, outer, subs)
				if rb, ok := u.Expr.(*sqlparser.BinaryExpr); ok && binaryPrec(rb.Operator) < binaryPrec(v.Operator) {
					r = "(" + r + ")"
				}
				l := exprToMochiRow(v.Left, rowVar, outer, subs)
				if lb, ok := v.Left.(*sqlparser.BinaryExpr); ok && binaryPrec(lb.Operator) < binaryPrec(v.Operator) {
					l = "(" + l + ")"
				}
				return fmt.Sprintf("%s - %s", l, r)
			}
		}
		if v.Operator == "-" {
			if u, ok := v.Right.(*sqlparser.UnaryExpr); ok && u.Operator == "-" {
				r := exprToMochiRow(u.Expr, rowVar, outer, subs)
				if rb, ok := u.Expr.(*sqlparser.BinaryExpr); ok && binaryPrec(rb.Operator) < binaryPrec(v.Operator) {
					r = "(" + r + ")"
				}
				l := exprToMochiRow(v.Left, rowVar, outer, subs)
				if lb, ok := v.Left.(*sqlparser.BinaryExpr); ok && binaryPrec(lb.Operator) < binaryPrec(v.Operator) {
					l = "(" + l + ")"
				}
				return fmt.Sprintf("%s + %s", l, r)
			}
		}
		l := exprToMochiRow(v.Left, rowVar, outer, subs)
		if lb, ok := v.Left.(*sqlparser.BinaryExpr); ok && binaryPrec(lb.Operator) < binaryPrec(v.Operator) {
			l = "(" + l + ")"
		}
		r := exprToMochiRow(v.Right, rowVar, outer, subs)
		if rb, ok := v.Right.(*sqlparser.BinaryExpr); ok && binaryPrec(rb.Operator) <= binaryPrec(v.Operator) {
			r = "(" + r + ")"
		}
		if v.Operator == sqlparser.DivStr || v.Operator == "/" {
			l = "(1.0 * (" + l + "))"
		}
		return fmt.Sprintf("%s %s %s", l, v.Operator, r)
	case *sqlparser.FuncExpr:
		name := strings.ToLower(v.Name.String())
		if name == "abs" && len(v.Exprs) == 1 {
			arg := v.Exprs[0].(*sqlparser.AliasedExpr).Expr
			ex := exprToMochiRow(arg, rowVar, outer, subs)
			return fmt.Sprintf("if %s < 0 then -(%s) else %s", ex, ex, ex)
		}
		if name == "coalesce" && len(v.Exprs) > 0 {
			for _, ex := range v.Exprs {
				if ae, ok := ex.(*sqlparser.AliasedExpr); ok {
					if s, ok := constNotNull(ae.Expr); ok {
						return s
					}
				}
			}
			out := exprToMochiRow(v.Exprs[len(v.Exprs)-1].(*sqlparser.AliasedExpr).Expr, rowVar, outer, subs)
			for i := len(v.Exprs) - 2; i >= 0; i-- {
				arg := exprToMochiRow(v.Exprs[i].(*sqlparser.AliasedExpr).Expr, rowVar, outer, subs)
				out = fmt.Sprintf("if %s != null then %s else %s", arg, arg, out)
			}
			return "(" + out + ")"
		}
	case *sqlparser.Subquery:
		expr := subqueryToMochi(v, rowVar)
		if subs != nil {
			if name, ok := subs[expr]; ok {
				return name
			}
		}
		return expr
	case *sqlparser.CaseExpr:
		elseExpr := "null"
		if v.Else != nil {
			elseExpr = exprToMochiRow(v.Else, rowVar, outer, subs)
		}
		out := elseExpr
		if v.Expr != nil {
			expr := exprToMochiRow(v.Expr, rowVar, outer, subs)
			for i := len(v.Whens) - 1; i >= 0; i-- {
				rhs := exprToMochiRow(v.Whens[i].Cond, rowVar, outer, subs)
				cond := fmt.Sprintf("(%s != null && %s != null && %s == %s)", expr, rhs, expr, rhs)
				val := exprToMochiRow(v.Whens[i].Val, rowVar, outer, subs)
				out = fmt.Sprintf("if %s then %s else %s", cond, val, out)
			}
		} else {
			for i := len(v.Whens) - 1; i >= 0; i-- {
				cond := condExprToMochiRow(v.Whens[i].Cond, rowVar, outer, subs)
				val := exprToMochiRow(v.Whens[i].Val, rowVar, outer, subs)
				if cond == "" {
					return "null"
				}
				out = fmt.Sprintf("if %s then %s else %s", cond, val, out)
			}
		}
		return out
	}
	return "null"
}

func simpleExpr(e sqlparser.Expr) bool {
	switch v := e.(type) {
	case *sqlparser.ColName, *sqlparser.SQLVal:
		return true
	case *sqlparser.ParenExpr:
		return simpleExpr(v.Expr)
	case *sqlparser.BinaryExpr:
		return simpleExpr(v.Left) && simpleExpr(v.Right)
	case *sqlparser.UnaryExpr:
		return simpleExpr(v.Expr)
	case *sqlparser.FuncExpr:
		name := strings.ToLower(v.Name.String())
		if name == "abs" && len(v.Exprs) == 1 {
			if ae, ok := v.Exprs[0].(*sqlparser.AliasedExpr); ok {
				return simpleExpr(ae.Expr)
			}
		} else if name == "coalesce" && len(v.Exprs) > 0 {
			for _, ex := range v.Exprs {
				if ae, ok := ex.(*sqlparser.AliasedExpr); ok {
					if !simpleExpr(ae.Expr) {
						return false
					}
				} else {
					return false
				}
			}
			return true
		}
		return false
	case *sqlparser.Subquery:
		return subqueryToMochi(v, "row") != "null"
	case *sqlparser.CaseExpr:
		if v.Expr != nil {
			if !simpleExpr(v.Expr) {
				return false
			}
			for _, w := range v.Whens {
				if !simpleExpr(w.Cond) || !simpleExpr(w.Val) {
					return false
				}
			}
		} else {
			for _, w := range v.Whens {
				if condExprToMochi(w.Cond, nil) == "" || !simpleExpr(w.Val) {
					return false
				}
			}
		}
		if v.Else != nil && !simpleExpr(v.Else) {
			return false
		}
		return true
	default:
		return false
	}
}

func condToMochi(where *sqlparser.Where, subs map[string]string) string {
	if where == nil {
		return ""
	}
	return condExprToMochiRow(where.Expr, "row", "", subs)
}

func condExprToMochi(e sqlparser.Expr, subs map[string]string) string {
	return condExprToMochiRow(e, "row", "", subs)
}

func condExprToMochiBare(e sqlparser.Expr) string {
	s := condExprToMochiRow(e, "row", "", nil)
	return strings.ReplaceAll(s, "row.", "")
}

func condToMochiBare(where *sqlparser.Where) string {
	if where == nil {
		return ""
	}
	return condExprToMochiBare(where.Expr)
}

func condExprToMochiRow(e sqlparser.Expr, rowVar, outer string, subs map[string]string) string {
	switch v := e.(type) {
	case *sqlparser.ComparisonExpr:
		left := exprToMochiRow(v.Left, rowVar, outer, subs)
		if !simpleExpr(v.Left) {
			left = "(" + left + ")"
		}
		op := strings.ToLower(v.Operator)
		// Handle `IN` and `NOT IN` expressions with a tuple of values.
		if op == sqlparser.InStr || op == sqlparser.NotInStr {
			if list, ok := v.Right.(sqlparser.ValTuple); ok {
				vals := make([]string, len(list))
				for i, expr := range list {
					vals[i] = exprToMochiRow(expr, rowVar, outer, subs)
				}
				rhs := "[" + strings.Join(vals, ", ") + "]"
				if op == sqlparser.InStr {
					return fmt.Sprintf("(%s in %s)", left, rhs)
				}
				return fmt.Sprintf("(!( %s in %s))", left, rhs)
			}
		}
		right := exprToMochiRow(v.Right, rowVar, outer, subs)
		if !simpleExpr(v.Right) {
			right = "(" + right + ")"
		}
		if op == "=" || op == "==" {
			return fmt.Sprintf("(%s != null && %s != null && %s == %s)", left, right, left, right)
		}
		return fmt.Sprintf("%s %s %s", left, op, right)
	case *sqlparser.AndExpr:
		l := condExprToMochiRow(v.Left, rowVar, outer, subs)
		if _, ok := v.Left.(*sqlparser.OrExpr); ok {
			l = "(" + l + ")"
		}
		r := condExprToMochiRow(v.Right, rowVar, outer, subs)
		if _, ok := v.Right.(*sqlparser.OrExpr); ok {
			r = "(" + r + ")"
		}
		if l == "" || r == "" {
			return ""
		}
		return l + " && " + r
	case *sqlparser.OrExpr:
		l := condExprToMochiRow(v.Left, rowVar, outer, subs)
		r := condExprToMochiRow(v.Right, rowVar, outer, subs)
		if l == "" || r == "" {
			return ""
		}
		return l + " || " + r
	case *sqlparser.RangeCond:
		left := exprToMochiRow(v.Left, rowVar, outer, subs)
		from := exprToMochiRow(v.From, rowVar, outer, subs)
		to := exprToMochiRow(v.To, rowVar, outer, subs)
		switch strings.ToLower(v.Operator) {
		case sqlparser.BetweenStr:
			return fmt.Sprintf("(%s >= %s && %s <= %s)", left, from, left, to)
		case sqlparser.NotBetweenStr:
			return fmt.Sprintf("(%s < %s || %s > %s)", left, from, left, to)
		}
		return ""
	case *sqlparser.IsExpr:
		left := exprToMochiRow(v.Expr, rowVar, outer, subs)
		switch strings.ToLower(v.Operator) {
		case sqlparser.IsNullStr:
			return fmt.Sprintf("%s == null", left)
		case sqlparser.IsNotNullStr:
			return fmt.Sprintf("%s != null", left)
		case sqlparser.IsTrueStr:
			return fmt.Sprintf("%s == true", left)
		case sqlparser.IsNotTrueStr:
			return fmt.Sprintf("%s != true", left)
		case sqlparser.IsFalseStr:
			return fmt.Sprintf("%s == false", left)
		case sqlparser.IsNotFalseStr:
			return fmt.Sprintf("%s != false", left)
		}
		return ""
	case *sqlparser.ExistsExpr:
		e := existsToMochi(v.Subquery, rowVar)
		if e == "" {
			return ""
		}
		return e
	case *sqlparser.ParenExpr:
		inner := condExprToMochiRow(v.Expr, rowVar, outer, subs)
		if inner == "" {
			return ""
		}
		return "(" + inner + ")"
	}
	return ""
}

func orderExprToMochi(e sqlparser.Expr, exprs []*sqlparser.AliasedExpr, subs map[string]string) string {
	if val, ok := e.(*sqlparser.SQLVal); ok && val.Type == sqlparser.IntVal {
		idx, err := strconv.Atoi(string(val.Val))
		if err == nil && idx >= 1 && idx <= len(exprs) {
			return exprToMochi(exprs[idx-1].Expr, subs)
		}
	}
	return exprToMochi(e, subs)
}

func collectSubqueriesExpr(e sqlparser.Expr, subs map[string]struct{}) {
	switch v := e.(type) {
	case *sqlparser.Subquery:
		expr := subqueryToMochi(v, "row")
		if expr != "null" && !strings.Contains(expr, "row.") {
			subs[expr] = struct{}{}
		}
	case *sqlparser.ParenExpr:
		collectSubqueriesExpr(v.Expr, subs)
	case *sqlparser.UnaryExpr:
		collectSubqueriesExpr(v.Expr, subs)
	case *sqlparser.BinaryExpr:
		collectSubqueriesExpr(v.Left, subs)
		collectSubqueriesExpr(v.Right, subs)
	case *sqlparser.ComparisonExpr:
		collectSubqueriesExpr(v.Left, subs)
		collectSubqueriesExpr(v.Right, subs)
	case *sqlparser.RangeCond:
		collectSubqueriesExpr(v.Left, subs)
		collectSubqueriesExpr(v.From, subs)
		collectSubqueriesExpr(v.To, subs)
	case *sqlparser.FuncExpr:
		for _, a := range v.Exprs {
			if ae, ok := a.(*sqlparser.AliasedExpr); ok {
				collectSubqueriesExpr(ae.Expr, subs)
			}
		}
	case *sqlparser.CaseExpr:
		if v.Expr != nil {
			collectSubqueriesExpr(v.Expr, subs)
		}
		for _, w := range v.Whens {
			collectSubqueriesExpr(w.Cond, subs)
			collectSubqueriesExpr(w.Val, subs)
		}
		if v.Else != nil {
			collectSubqueriesExpr(v.Else, subs)
		}
	}
}

func collectSubqueries(sel *sqlparser.Select) []string {
	m := map[string]struct{}{}
	for _, se := range sel.SelectExprs {
		if ae, ok := se.(*sqlparser.AliasedExpr); ok {
			collectSubqueriesExpr(ae.Expr, m)
		}
	}
	if sel.Where != nil {
		collectSubqueriesExpr(sel.Where.Expr, m)
	}
	for _, ob := range sel.OrderBy {
		collectSubqueriesExpr(ob.Expr, m)
	}
	out := make([]string, 0, len(m))
	for k := range m {
		out = append(out, k)
	}
	sort.Strings(out)
	return out
}

func generateUpdate(stmt string) string {
	node, err := sqlparser.Parse(stmt)
	if err != nil {
		return ""
	}
	u := node.(*sqlparser.Update)
	tbl := u.TableExprs[0].(*sqlparser.AliasedTableExpr).Expr.(sqlparser.TableName).Name.String()
	cond := condToMochiBare(u.Where)
	var sb strings.Builder
	sb.WriteString("update " + tbl + "\n")
	sb.WriteString("set {\n")
	for i, expr := range u.Exprs {
		if i > 0 {
			sb.WriteString("\n")
		}
		sb.WriteString(fmt.Sprintf("  %s: %s,", expr.Name.Name.String(), exprToMochiBare(expr.Expr)))
	}
	sb.WriteString("\n}")
	if cond != "" {
		sb.WriteString("\nwhere " + cond)
	}
	sb.WriteString("\n\n")
	return sb.String()
}

// Generate returns Mochi source code for the given Case.
func Generate(c Case) string {
	var sb strings.Builder

	if len(c.Comments) > 0 || c.Line > 0 {
		sb.WriteString("/*\n")
		if c.Line > 0 {
			sb.WriteString(fmt.Sprintf("# line: %d\n", c.Line))
		}
		for _, line := range c.Comments {
			sb.WriteString(line + "\n")
		}
		sb.WriteString("*/\n\n")
	}

	mutated := map[string]bool{}
	for _, stmt := range c.Updates {
		node, err := sqlparser.Parse(stmt)
		if err == nil {
			u := node.(*sqlparser.Update)
			tbl := u.TableExprs[0].(*sqlparser.AliasedTableExpr).Expr.(sqlparser.TableName).Name.String()
			mutated[tbl] = true
		}
	}

	names := make([]string, 0, len(c.Tables))
	for n := range c.Tables {
		names = append(names, n)
	}
	sort.Strings(names)
	for _, name := range names {
		t := c.Tables[name]
		kw := "let"
		if mutated[name] {
			kw = "var"
		}
		typeName := name + "Row"
		sb.WriteString(fmt.Sprintf("type %s {\n", typeName))
		for _, col := range t.Columns {
			ct := detectColumnType(t.Rows, col, t.Types, t.Columns)
			sb.WriteString(fmt.Sprintf("  %s: %s\n", col, ct))
		}
		sb.WriteString("}\n\n")

		sb.WriteString(fmt.Sprintf("%s %s = [\n", kw, name))
		for _, row := range t.Rows {
			sb.WriteString(fmt.Sprintf("  %s {\n", typeName))
			for _, col := range t.Columns {
				sb.WriteString(fmt.Sprintf("    %s: %s,\n", col, formatValue(row[col])))
			}
			sb.WriteString("  },\n")
		}
		sb.WriteString("]\n\n")
	}

	for _, u := range c.Updates {
		sb.WriteString(fmt.Sprintf("/* %s */\n", u))
		sb.WriteString(generateUpdate(u))
	}

	sb.WriteString(fmt.Sprintf("/* %s */\n", c.Query))
	q := strings.TrimSpace(c.Query)
	if strings.HasPrefix(strings.ToLower(q), "select all ") {
		q = "SELECT " + q[len("select all "):]
	}
	stmt, err := sqlparser.Parse(q)
	if err != nil {
		return ""
	}
	sel, ok := stmt.(*sqlparser.Select)
	if !ok {
		return ""
	}

	tblExpr := func() *sqlparser.AliasedTableExpr {
		if len(sel.From) == 1 {
			if at, ok := sel.From[0].(*sqlparser.AliasedTableExpr); ok {
				return at
			}
			return nil
		}

		// When multiple tables are present, attempt to detect if all
		// column references originate from a single table. If so,
		// treat the query as operating on that table only and ignore
		// the rest. This allows handling of simple cross joins where
		// additional tables are unused.
		aliases := map[string]bool{}
		collectAliases := func(e sqlparser.Expr) {}
		collectAliases = func(e sqlparser.Expr) {
			switch v := e.(type) {
			case *sqlparser.ColName:
				if q := v.Qualifier.Name.String(); q != "" {
					aliases[q] = true
				}
			case *sqlparser.BinaryExpr:
				collectAliases(v.Left)
				collectAliases(v.Right)
			case *sqlparser.UnaryExpr:
				collectAliases(v.Expr)
			case *sqlparser.FuncExpr:
				for _, ex := range v.Exprs {
					if ae, ok := ex.(*sqlparser.AliasedExpr); ok {
						collectAliases(ae.Expr)
					}
				}
			case *sqlparser.ParenExpr:
				collectAliases(v.Expr)
			case *sqlparser.CaseExpr:
				if v.Expr != nil {
					collectAliases(v.Expr)
				}
				for _, w := range v.Whens {
					collectAliases(w.Cond)
					collectAliases(w.Val)
				}
				if v.Else != nil {
					collectAliases(v.Else)
				}
			}
		}

		for _, se := range sel.SelectExprs {
			if ae, ok := se.(*sqlparser.AliasedExpr); ok {
				collectAliases(ae.Expr)
			}
		}
		for _, g := range sel.GroupBy {
			collectAliases(g)
		}
		if sel.Where != nil {
			collectAliases(sel.Where.Expr)
		}

		if len(aliases) != 1 {
			return nil
		}

		var want string
		for a := range aliases {
			want = a
		}

		for _, te := range sel.From {
			if at, ok := te.(*sqlparser.AliasedTableExpr); ok {
				alias := at.As.String()
				if alias == "" {
					if tn, ok := at.Expr.(sqlparser.TableName); ok {
						alias = tn.Name.String()
					}
				}
				if alias == want {
					return at
				}
			}
		}
		return nil
	}()
	if tblExpr == nil {
		return ""
	}

	tblName, ok := tblExpr.Expr.(sqlparser.TableName)
	if !ok {
		return ""
	}

	tblNameStr := tblName.Name.String()

	subExprs := collectSubqueries(sel)
	subs := map[string]string{}
	for i, s := range subExprs {
		if strings.Contains(s, "row.") {
			// Correlated subqueries reference the current row and
			// must remain inline.
			continue
		}
		name := fmt.Sprintf("sub%d", i)
		subs[s] = name
		sb.WriteString(fmt.Sprintf("let %s = %s\n", name, s))
	}
	if len(subs) > 0 {
		sb.WriteString("\n")
	}

	// Handle aggregate functions
	if len(sel.SelectExprs) == 1 {
		if ae, ok := sel.SelectExprs[0].(*sqlparser.AliasedExpr); ok {
			if fn, ok := ae.Expr.(*sqlparser.FuncExpr); ok {
				name := strings.ToLower(fn.Name.String())
				// COUNT(*) handled specially
				if name == "count" && len(fn.Exprs) == 1 {
					if _, ok := fn.Exprs[0].(*sqlparser.StarExpr); ok {
						cond := condToMochi(sel.Where, subs)
						sb.WriteString("let result = count(from row in " + tblNameStr)
						if cond != "" {
							sb.WriteString("\n  where " + cond)
						}
						sb.WriteString("\n  select row)\n")
						sb.WriteString("print(result)\n\n")
						if len(c.Expect) > 0 {
							sb.WriteString(fmt.Sprintf("test \"%s\" {\n  expect result == %s\n}\n", c.Name, normalizeExpect(c.Expect[0])))
						}
						return sb.String()
					}
				}

				// general single-arg aggregates
				if len(fn.Exprs) >= 1 && len(fn.Exprs) <= 2 {
					arg := fn.Exprs[0].(*sqlparser.AliasedExpr).Expr
					exprOrig := exprToMochi(arg, subs)
					expr := exprOrig
					if col, ok := arg.(*sqlparser.ColName); ok {
						if t, ok := c.Tables[tblNameStr]; ok {
							for i, ccol := range t.Columns {
								if strings.EqualFold(ccol, col.Name.String()) {
									typ := strings.ToLower(t.Types[i])
									if strings.Contains(typ, "char") || strings.Contains(typ, "text") || strings.Contains(typ, "varchar") || strings.Contains(typ, "clob") {
										expr = fmt.Sprintf("(%s as float)", exprOrig)
									}
									break
								}
							}
						}
					}
					cond := condToMochi(sel.Where, subs)
					if name == "count" {
						if cond != "" {
							cond += " && " + exprOrig + " != null"
						} else {
							cond = exprOrig + " != null"
						}
						list := "from row in " + tblNameStr
						if cond != "" {
							list += "\n  where " + cond
						}
						list += "\n  select row"
						if fn.Distinct {
							list = "distinct(" + list + ")"
						}
						sb.WriteString("let result = count(" + list + ")\n")
					} else if name == "group_concat" {
						delim := strconv.Quote(",")
						if len(fn.Exprs) == 2 {
							delim = exprToMochi(fn.Exprs[1].(*sqlparser.AliasedExpr).Expr, subs)
						}
						if cond != "" {
							cond += " && " + exprOrig + " != null"
						} else {
							cond = exprOrig + " != null"
						}
						list := "from row in " + tblNameStr
						if cond != "" {
							list += "\n  where " + cond
						}
						list += "\n  select str(" + expr + ")"
						if fn.Distinct {
							list = "distinct(" + list + ")"
						}
						sb.WriteString("let result = join(" + list + ", " + delim + ")\n")
					} else {
						if cond != "" {
							cond += " && " + exprOrig + " != null"
						} else {
							cond = exprOrig + " != null"
						}
						list := "from row in " + tblNameStr
						if cond != "" {
							list += "\n  where " + cond
						}
						list += "\n  select " + expr
						if fn.Distinct {
							list = "distinct(" + list + ")"
						}
						callName := name
						decl := "let"
						if name == "total" {
							callName = "sum"
							decl = "var"
						}
						sb.WriteString(decl + " result = " + callName + "(" + list + ")\n")
						if name == "total" {
							sb.WriteString("result = if result == null then 0.0 else (result as float)\n")
						}
					}
					sb.WriteString("print(result)\n\n")
					if len(c.Expect) > 0 {
						sb.WriteString(fmt.Sprintf("test \"%s\" {\n  expect result == %s\n}\n", c.Name, normalizeExpect(c.Expect[0])))
					}
					return sb.String()
				}
			}
		}
	}

	// Support simple GROUP BY without aggregates
	if len(sel.GroupBy) > 0 && len(sel.SelectExprs) == 1 && len(sel.OrderBy) <= 1 {
		ae, ok := sel.SelectExprs[0].(*sqlparser.AliasedExpr)
		if !ok || hasAggExpr(ae.Expr) || !simpleExpr(ae.Expr) {
			return ""
		}
		keyParts := make([]string, 0, len(sel.GroupBy))
		keyMap := make(map[string]string)
		singleCol := false
		if len(sel.GroupBy) == 1 {
			if col, ok := sel.GroupBy[0].(*sqlparser.ColName); ok {
				singleCol = true
				keyParts = append(keyParts, exprToMochiRow(col, "row", "", subs))
				keyMap["row."+col.Name.String()] = "g.key"
			}
		}
		if !singleCol {
			for i, gexpr := range sel.GroupBy {
				if !simpleExpr(gexpr) {
					return ""
				}
				name := fmt.Sprintf("k%d", i)
				keyParts = append(keyParts, fmt.Sprintf("%s: %s", name, exprToMochiRow(gexpr, "row", "", subs)))
				if col, ok := gexpr.(*sqlparser.ColName); ok {
					keyMap["row."+col.Name.String()] = fmt.Sprintf("g.key.%s", name)
				}
			}
		}

		cond := condToMochi(sel.Where, subs)
		sb.WriteString("var result = from row in " + tblNameStr)
		if cond != "" {
			sb.WriteString("\n  where " + cond)
		}
		if singleCol {
			sb.WriteString("\n  group by " + keyParts[0] + " into g")
		} else {
			sb.WriteString("\n  group by {" + strings.Join(keyParts, ", ") + "} into g")
		}
		expr := exprToMochi(ae.Expr, subs)
		for old, new := range keyMap {
			expr = strings.ReplaceAll(expr, old, new)
		}
		sb.WriteString("\n  select " + expr + "\n")
		if sel.Distinct != "" {
			sb.WriteString("result = from x in result\n  group by x into g\n  select g.key\n")
		}
		if c.RowSort {
			sb.WriteString("result = from x in result\n  order by str(x)\n  select x\n")
		}
		sb.WriteString("for x in result {\n  print(x)\n}\n\n")
		if len(c.Expect) > 0 {
			sb.WriteString(fmt.Sprintf("test \"%s\" {\n  expect result == %s\n}\n", c.Name, formatExpectList(c.Expect)))
		} else {
			sb.WriteString(fmt.Sprintf("test \"%s\" {\n  expect result == []\n}\n", c.Name))
		}
		return sb.String()
	}

	// Support simple single-column selects
	if len(sel.SelectExprs) == 1 && len(sel.OrderBy) <= 1 {
		ae, ok := sel.SelectExprs[0].(*sqlparser.AliasedExpr)
		if !ok || !simpleExpr(ae.Expr) {
			return ""
		}
		if sel.Where != nil && condExprToMochi(sel.Where.Expr, subs) == "" {
			return ""
		}
		if len(sel.OrderBy) == 1 && !simpleExpr(sel.OrderBy[0].Expr) {
			return ""
		}
		expr := exprToMochi(ae.Expr, subs)
		if expectAllInts(c.Expect) {
			expr = "(" + expr + " as int)"
		}
		cond := condToMochi(sel.Where, subs)
		if len(sel.From) == 0 || (len(sel.From) == 1 && func() bool {
			if tbl, ok := sel.From[0].(*sqlparser.AliasedTableExpr); ok {
				if name, ok := tbl.Expr.(sqlparser.TableName); ok {
					return strings.EqualFold(name.Name.String(), "dual")
				}
			}
			return false
		}()) {
			sb.WriteString("var result = [" + expr + "]\n")
		} else {
			sb.WriteString("var result = from row in " + tblNameStr)
			if cond != "" {
				sb.WriteString("\n  where " + cond)
			}
			if len(sel.OrderBy) == 1 {
				sb.WriteString("\n  order by " + orderExprToMochi(sel.OrderBy[0].Expr, []*sqlparser.AliasedExpr{ae}, subs))
			}
			sb.WriteString("\n  select " + expr + "\n")
		}
		if sel.Distinct != "" {
			sb.WriteString("result = from x in result\n  group by x into g\n  select g.key\n")
		}
		if c.RowSort {
			sb.WriteString("result = from x in result\n  order by str(x)\n  select x\n")
		}
		sb.WriteString("for x in result {\n  print(x)\n}\n\n")
		if len(c.Expect) > 0 {
			sb.WriteString(fmt.Sprintf("test \"%s\" {\n  expect result == %s\n}\n", c.Name, formatExpectList(c.Expect)))
		} else {
			sb.WriteString(fmt.Sprintf("test \"%s\" {\n  expect result == []\n}\n", c.Name))
		}
		return sb.String()
	}

	// Support simple multi-column selects without subqueries or functions.
	// Previously cases with zero expected rows were skipped because
	// c.Expect would be empty. Generate a test in that situation as well
	// and expect an empty result list.
	if len(sel.SelectExprs) > 1 {
		aes := make([]*sqlparser.AliasedExpr, 0, len(sel.SelectExprs))
		var exprs []string
		for _, se := range sel.SelectExprs {
			ae, ok := se.(*sqlparser.AliasedExpr)
			if !ok || !simpleExpr(ae.Expr) {
				return ""
			}
			aes = append(aes, ae)
			exprs = append(exprs, exprToMochi(ae.Expr, subs))
		}
		if sel.Where != nil && condExprToMochi(sel.Where.Expr, subs) == "" {
			return ""
		}
		for _, ob := range sel.OrderBy {
			if !simpleExpr(ob.Expr) {
				if val, ok := ob.Expr.(*sqlparser.SQLVal); !ok || val.Type != sqlparser.IntVal {
					return ""
				}
			}
		}
		cond := condToMochi(sel.Where, subs)
		if len(sel.From) == 0 || (len(sel.From) == 1 && func() bool {
			if tbl, ok := sel.From[0].(*sqlparser.AliasedTableExpr); ok {
				if name, ok := tbl.Expr.(sqlparser.TableName); ok {
					return strings.EqualFold(name.Name.String(), "dual")
				}
			}
			return false
		}()) {
			sb.WriteString("var result = [[" + strings.Join(exprs, ", ") + "]]\n")
		} else {
			sb.WriteString("var result = from row in " + tblNameStr)
			if cond != "" {
				sb.WriteString("\n  where " + cond)
			}
			if len(sel.OrderBy) > 0 {
				sb.WriteString("\n  order by ")
				if len(sel.OrderBy) == 1 {
					sb.WriteString(orderExprToMochi(sel.OrderBy[0].Expr, aes, subs))
				} else {
					sb.WriteString("[")
					for i, ob := range sel.OrderBy {
						if i > 0 {
							sb.WriteString(", ")
						}
						sb.WriteString(orderExprToMochi(ob.Expr, aes, subs))
					}
					sb.WriteString("]")
				}
			}
			sb.WriteString("\n  select [" + strings.Join(exprs, ", ") + "]\n")
			if sel.Distinct != "" {
				sb.WriteString("result = from row in result\n  group by row into g\n  select g.key\n")
			}
		}
		if c.RowSort {
			sb.WriteString("result = from row in result\n")
			sb.WriteString("  order by join(from v in row select str(v), \" \" )\n")
			sb.WriteString("  select row\n")
		}
		sb.WriteString("var flatResult = []\n")
		sb.WriteString("for row in result {\n")
		sb.WriteString("  for x in row {\n")
		sb.WriteString("    flatResult = append(flatResult, x)\n")
		sb.WriteString("  }\n}\n")
		if c.RowSort {
			sb.WriteString("flatResult = from x in flatResult\n  order by str(x)\n  select x\n")
		}
		sb.WriteString("for x in flatResult {\n  print(x)\n}\n")
		if len(c.Expect) > 0 {
			sb.WriteString(fmt.Sprintf("test \"%s\" {\n  expect flatResult == %s\n}\n", c.Name, formatExpectList(c.Expect)))
		} else {
			sb.WriteString(fmt.Sprintf("test \"%s\" {\n  expect flatResult == []\n}\n", c.Name))
		}
		sb.WriteString("\n")
		return sb.String()
	}

	return ""
}

func formatExpectList(xs []string) string {
	var sb strings.Builder
	sb.WriteString("[")
	for i, x := range xs {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(normalizeExpect(x))
	}
	sb.WriteString("]")
	return sb.String()
}

func normalizeExpect(x string) string {
	if f, err := strconv.ParseFloat(x, 64); err == nil {
		if strings.ContainsAny(x, "eE") {
			x = strconv.FormatFloat(f, 'f', -1, 64)
		}
	}
	switch strings.ToLower(x) {
	case "null":
		return "null"
	case "true":
		return "true"
	case "false":
		return "false"
	}
	if strings.HasPrefix(x, "-") {
		return "(" + x + ")"
	}
	return x
}

func expectAllInts(xs []string) bool {
	if len(xs) == 0 {
		return false
	}
	for _, s := range xs {
		if _, err := strconv.Atoi(s); err != nil {
			return false
		}
	}
	return true
}
