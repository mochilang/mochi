//go:build slow

package erlang

import "mochi/parser"

func identName(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil || len(u.Value.Ops) != 0 {
		return "", false
	}
	if sel := u.Value.Target.Selector; sel != nil {
		name := sel.Root
		for _, part := range sel.Tail {
			if !identifierRegex.MatchString(part) {
				return "", false
			}
			name += "." + part
		}
		return name, true
	}
	return "", false
}

// groupKeyFields returns the field names of a simple map literal used as a
// group key. It returns nil if the expression is not a suitable map literal.
func groupKeyFields(e *parser.Expr) []string {
	if e == nil || e.Binary == nil {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil || len(u.Value.Ops) != 0 {
		return nil
	}
	if u.Value.Target == nil || u.Value.Target.Map == nil {
		return nil
	}
	fields := make([]string, 0, len(u.Value.Target.Map.Items))
	for _, it := range u.Value.Target.Map.Items {
		if name, ok := identName(it.Key); ok {
			fields = append(fields, name)
		}
	}
	return fields
}

// mapLiteralFields returns the field names of a simple map literal expression.
// The expression must be a literal map or struct literal with identifier keys.
func mapLiteralFields(e *parser.Expr) []string {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil || len(u.Value.Ops) != 0 {
		return nil
	}
	if u.Value.Target.Map != nil {
		fields := make([]string, 0, len(u.Value.Target.Map.Items))
		for _, it := range u.Value.Target.Map.Items {
			if name, ok := identName(it.Key); ok {
				fields = append(fields, name)
			}
		}
		return fields
	}
	if u.Value.Target.Struct != nil {
		fields := make([]string, 0, len(u.Value.Target.Struct.Fields))
		for _, f := range u.Value.Target.Struct.Fields {
			fields = append(fields, f.Name)
		}
		return fields
	}
	return nil
}

// listMapLiteralFields returns field names from a list literal of map literals.
func listMapLiteralFields(e *parser.Expr) []string {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil || len(u.Value.Ops) != 0 {
		return nil
	}
	if l := u.Value.Target.List; l != nil && len(l.Elems) > 0 {
		return mapLiteralFields(l.Elems[0])
	}
	return nil
}

// queryResultFields returns field names of the map returned by q.Select if it
// is a simple map literal.
func queryResultFields(q *parser.QueryExpr) []string {
	if q == nil {
		return nil
	}
	if q.Group != nil && selectIsVar(q.Select, q.Group.Name) {
		return []string{"key", "items"}
	}
	return mapLiteralFields(q.Select)
}

func fieldSet(fields []string) map[string]bool {
	if len(fields) == 0 {
		return nil
	}
	m := make(map[string]bool, len(fields))
	for _, f := range fields {
		m[f] = true
	}
	return m
}
