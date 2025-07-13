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
	if sel := u.Value.Target.Selector; sel != nil && len(sel.Tail) == 0 {
		return sel.Root, true
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
