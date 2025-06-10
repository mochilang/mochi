package interpreter

import (
	"encoding/csv"
	"os"
	"strconv"

	"mochi/parser"
	"mochi/runtime/data"
	"mochi/types"
)

func (i *Interpreter) invokeTool(cl closure, args map[string]any) (any, error) {
	child := types.NewEnv(cl.Env)
	for _, param := range cl.FullParams {
		child.SetValue(param.Name, args[param.Name], true)
	}
	old := i.env
	i.env = child
	defer func() { i.env = old }()
	if cl.Fn.ExprBody != nil {
		return i.evalExpr(cl.Fn.ExprBody)
	}
	for _, stmt := range cl.Fn.BlockBody {
		if err := i.evalStmt(stmt); err != nil {
			if r, ok := err.(returnSignal); ok {
				return r.value, nil
			}
			return nil, err
		}
	}
	return nil, nil
}

func isUnderscoreExpr(e *parser.Expr) bool {
	if e == nil {
		return false
	}
	if len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return false
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return false
	}
	if p.Target.Selector != nil && p.Target.Selector.Root == "_" && len(p.Target.Selector.Tail) == 0 {
		return true
	}
	return false
}

func identName(e *parser.Expr) (string, bool) {
	if e == nil {
		return "", false
	}
	if len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return "", false
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return "", false
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	return "", false
}

func callPattern(e *parser.Expr) (*parser.CallExpr, bool) {
	if e == nil {
		return nil, false
	}
	if len(e.Binary.Right) != 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil, false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target.Call == nil {
		return nil, false
	}
	return p.Target.Call, true
}

func (i *Interpreter) evalQuery(q *parser.QueryExpr) (any, error) {
	child := types.NewEnv(i.env)
	old := i.env
	i.env = child
	defer func() { i.env = old }()
	switch i.dataPlan {
	case "duckdb":
		return data.EvalQueryDuckDB(q, child, func(e *parser.Expr) (any, error) {
			return i.evalExpr(e)
		})
	default:
		return data.EvalQuery(q, child, func(e *parser.Expr) (any, error) {
			return i.evalExpr(e)
		})
	}
}

func (i *Interpreter) evalMatch(m *parser.MatchExpr) (any, error) {
	val, err := i.evalExpr(m.Target)
	if err != nil {
		return nil, err
	}
	for _, c := range m.Cases {
		if isUnderscoreExpr(c.Pattern) {
			return i.evalExpr(c.Result)
		}
		if call, ok := callPattern(c.Pattern); ok {
			obj, ok := val.(map[string]any)
			if !ok {
				continue
			}
			name, _ := obj["__name"].(string)
			if name != call.Func {
				continue
			}
			st, ok := i.types.GetStruct(call.Func)
			if !ok {
				continue
			}
			if len(call.Args) != len(st.Order) {
				continue
			}
			child := types.NewEnv(i.env)
			for idx, arg := range call.Args {
				if n, ok := identName(arg); ok {
					child.SetValue(n, obj[st.Order[idx]], true)
				}
			}
			old := i.env
			i.env = child
			res, err := i.evalExpr(c.Result)
			i.env = old
			return res, err
		}
		if ident, ok := identName(c.Pattern); ok {
			obj, ok := val.(map[string]any)
			if ok {
				name, _ := obj["__name"].(string)
				if name == ident {
					return i.evalExpr(c.Result)
				}
			}
		} else {
			pv, err := i.evalExpr(c.Pattern)
			if err != nil {
				return nil, err
			}
			eq, err := applyBinary(c.Pos, val, "==", pv)
			if err != nil {
				return nil, err
			}
			if b, ok := eq.(bool); ok && b {
				return i.evalExpr(c.Result)
			}
		}
	}
	return nil, nil
}

func loadCSV(path string) ([]map[string]any, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	r := csv.NewReader(f)
	rows, err := r.ReadAll()
	if err != nil {
		return nil, err
	}
	if len(rows) == 0 {
		return nil, nil
	}
	headers := rows[0]
	out := make([]map[string]any, 0, len(rows)-1)
	for _, rec := range rows[1:] {
		m := map[string]any{}
		for idx, h := range headers {
			var val string
			if idx < len(rec) {
				val = rec[idx]
			}
			if iv, err := strconv.Atoi(val); err == nil {
				m[h] = iv
			} else if fv, err := strconv.ParseFloat(val, 64); err == nil {
				m[h] = fv
			} else {
				m[h] = val
			}
		}
		out = append(out, m)
	}
	return out, nil
}
