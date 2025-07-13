//go:build slow

package racket

import "mochi/parser"

// groupKeyNames returns field names used in a GROUP BY map expression.
func groupKeyNames(e *parser.Expr) []string {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil || u.Value.Target == nil || u.Value.Target.Map == nil || len(u.Value.Ops) != 0 {
		return nil
	}
	m := u.Value.Target.Map
	names := make([]string, 0, len(m.Items))
	for _, it := range m.Items {
		if id, ok := identName(it.Key); ok {
			names = append(names, id)
		}
	}
	return names
}
