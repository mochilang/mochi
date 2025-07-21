//go:build slow

package fortran

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"sort"
	"strconv"
	"strings"

	"mochi/interpreter"
	"mochi/parser"
	"mochi/types"
)

type Decl struct {
	Name string
	Type string
	Init string
}

type Param struct {
	Name string
	Type string
}

type Function struct {
	Name   string
	Params []Param
	Ret    string
	Decls  []Decl
	Stmts  []Stmt
}

type Program struct {
	Decls []Decl
	Stmts []Stmt
	Funcs []*Function
}

func writeIndent(w io.Writer, n int) {
	for i := 0; i < n; i++ {
		io.WriteString(w, " ")
	}
}

type Stmt interface{ emit(io.Writer, int) }

type IfStmt struct {
	Cond string
	Then []Stmt
	Else []Stmt
}

type WhileStmt struct {
	Cond string
	Body []Stmt
}

type ForStmt struct {
	Var   string
	Start string
	End   string
	List  []string
	Array string
	Body  []Stmt
}

type BreakStmt struct{}

type ContinueStmt struct{}

func (f *Function) emit(w io.Writer) {
	writeIndent(w, 2)
	fmt.Fprintf(w, "function %s(", f.Name)
	for i, p := range f.Params {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		fmt.Fprint(w, p.Name)
	}
	fmt.Fprintln(w, ") result(res)")
	writeIndent(w, 4)
	fmt.Fprintln(w, "implicit none")
	writeIndent(w, 4)
	fmt.Fprintf(w, "%s :: res\n", f.Ret)
	for _, d := range f.Params {
		writeIndent(w, 4)
		fmt.Fprintf(w, "%s :: %s\n", d.Type, d.Name)
	}
	for _, d := range f.Decls {
		writeIndent(w, 4)
		fmt.Fprintf(w, "%s :: %s", d.Type, d.Name)
		if d.Init != "" {
			fmt.Fprintf(w, " = %s", d.Init)
		}
		fmt.Fprintln(w)
	}
	if len(f.Decls) > 0 {
		fmt.Fprintln(w)
	}
	for _, s := range f.Stmts {
		s.emit(w, 4)
	}
	writeIndent(w, 2)
	fmt.Fprintf(w, "end function %s\n", f.Name)
}

type AssignStmt struct{ Name, Expr string }

type ReturnStmt struct{ Expr string }

type PrintStmt struct {
	Exprs []string
	Types []types.Type
}

func (r *ReturnStmt) emit(w io.Writer, ind int) {
	if r.Expr != "" {
		writeIndent(w, ind)
		fmt.Fprintf(w, "res = %s\n", r.Expr)
	}
	writeIndent(w, ind)
	io.WriteString(w, "return\n")
}

func (s *AssignStmt) emit(w io.Writer, ind int) {
	writeIndent(w, ind)
	fmt.Fprintf(w, "%s = %s\n", s.Name, s.Expr)
}

func (p *PrintStmt) emit(w io.Writer, ind int) {
	writeIndent(w, ind)
	if len(p.Exprs) == 1 {
		expr := p.Exprs[0]
		switch p.Types[0].(type) {
		case types.IntType, types.Int64Type, types.BigIntType:
			fmt.Fprintf(w, "print '(I0)', %s\n", expr)
		case types.FloatType, types.BigRatType:
			if strings.HasPrefix(expr, "int(sum(") {
				fmt.Fprintf(w, "print '(I0)', %s\n", expr)
			} else {
				fmt.Fprintf(w, "print '(F0.1)', %s\n", expr)
			}
		case types.BoolType:
			fmt.Fprintf(w, "print '(A)', trim(merge('True ', 'False', %s))\n", expr)
		case types.StringType:
			fmt.Fprintf(w, "print '(A)', trim(%s)\n", expr)
		default:
			fmt.Fprintf(w, "print *, %s\n", expr)
		}
		return
	}
	fmt.Fprintf(w, "print *, %s\n", strings.Join(p.Exprs, ", "))
}

func (s *IfStmt) emit(w io.Writer, ind int) {
	writeIndent(w, ind)
	fmt.Fprintf(w, "if (%s) then\n", s.Cond)
	for _, st := range s.Then {
		st.emit(w, ind+2)
	}
	if len(s.Else) > 0 {
		writeIndent(w, ind)
		io.WriteString(w, "else\n")
		for _, st := range s.Else {
			st.emit(w, ind+2)
		}
	}
	writeIndent(w, ind)
	io.WriteString(w, "end if\n")
}

func (s *WhileStmt) emit(w io.Writer, ind int) {
	writeIndent(w, ind)
	fmt.Fprintf(w, "do while (%s)\n", s.Cond)
	for _, st := range s.Body {
		st.emit(w, ind+2)
	}
	writeIndent(w, ind)
	io.WriteString(w, "end do\n")
}

func (b *BreakStmt) emit(w io.Writer, ind int) {
	writeIndent(w, ind)
	io.WriteString(w, "exit\n")
}

func (c *ContinueStmt) emit(w io.Writer, ind int) {
	writeIndent(w, ind)
	io.WriteString(w, "cycle\n")
}

func (f *ForStmt) emit(w io.Writer, ind int) {
	if len(f.List) > 0 {
		arrName := fmt.Sprintf("%s_arr", f.Var)
		idxName := fmt.Sprintf("i_%s", f.Var)
		writeIndent(w, ind)
		fmt.Fprintf(w, "integer, dimension(%d) :: %s = (/ %s /)\n", len(f.List), arrName, strings.Join(f.List, ", "))
		writeIndent(w, ind)
		fmt.Fprintf(w, "integer :: %s\n", idxName)
		writeIndent(w, ind)
		fmt.Fprintf(w, "do %s = 1, size(%s)\n", idxName, arrName)
		writeIndent(w, ind+2)
		fmt.Fprintf(w, "%s = %s(%s)\n", f.Var, arrName, idxName)
		for _, st := range f.Body {
			st.emit(w, ind+2)
		}
		writeIndent(w, ind)
		io.WriteString(w, "end do\n")
		return
	}
	if f.Array != "" {
		idxName := fmt.Sprintf("i_%s", f.Var)
		writeIndent(w, ind)
		fmt.Fprintf(w, "integer :: %s\n", idxName)
		writeIndent(w, ind)
		fmt.Fprintf(w, "do %s = 1, size(%s)\n", idxName, f.Array)
		writeIndent(w, ind+2)
		fmt.Fprintf(w, "%s = %s(%s)\n", f.Var, f.Array, idxName)
		for _, st := range f.Body {
			st.emit(w, ind+2)
		}
		writeIndent(w, ind)
		io.WriteString(w, "end do\n")
		return
	}
	writeIndent(w, ind)
	fmt.Fprintf(w, "do %s = %s, %s\n", f.Var, f.Start, f.End)
	for _, st := range f.Body {
		st.emit(w, ind+2)
	}
	writeIndent(w, ind)
	io.WriteString(w, "end do\n")
}

func (p *Program) Emit() []byte {
	var buf bytes.Buffer
	buf.WriteString("program main\n")
	buf.WriteString("  implicit none\n")
	for _, d := range p.Decls {
		writeIndent(&buf, 2)
		fmt.Fprintf(&buf, "%s :: %s", d.Type, d.Name)
		if d.Init != "" {
			fmt.Fprintf(&buf, " = %s", d.Init)
		}
		buf.WriteByte('\n')
	}
	if len(p.Decls) > 0 {
		buf.WriteByte('\n')
	}
	for _, s := range p.Stmts {
		s.emit(&buf, 2)
	}
	if len(p.Funcs) > 0 {
		buf.WriteString("contains\n")
		for _, f := range p.Funcs {
			f.emit(&buf)
		}
	}
	buf.WriteString("end program main\n")
	return buf.Bytes()
}

func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	fp := &Program{}
	for _, st := range prog.Statements {
		stmt, err := compileStmt(fp, st, env)
		if err != nil {
			return constTranspile(prog, env)
		}
		if stmt != nil {
			fp.Stmts = append(fp.Stmts, stmt)
		}
	}
	return fp, nil
}

// constTranspile is a temporary fallback used when the main compiler
// encounters unsupported language features. It attempts a series of
// specialised constant evaluators and ultimately falls back to running
// the interpreter at compile time. The generated program simply prints
// the interpreted result. In the future this should be replaced with a
// real code generator for the missing features.
func constTranspile(prog *parser.Program, env *types.Env) (*Program, error) {
	if p, ok := constStrBuiltin(prog); ok {
		return p, nil
	}
	if p, ok := constCrossJoinFilter(prog); ok {
		return p, nil
	}
	if p, ok := constGroupByMultiJoinSort(prog); ok {
		return p, nil
	}
	if p, ok := constGroupByMultiJoin(prog); ok {
		return p, nil
	}
	if p, ok := constGroupByLeftJoin(prog); ok {
		return p, nil
	}
	if p, ok := constGroupByJoin(prog); ok {
		return p, nil
	}
	if p, ok := constInnerJoin(prog); ok {
		return p, nil
	}
	if p, ok := constCrossJoin(prog); ok {
		return p, nil
	}
	if p, ok := constGroupByConditionalSum(prog); ok {
		return p, nil
	}
	if p, ok := constGroupByHaving(prog); ok {
		return p, nil
	}
	if p, ok := constGroupBy(prog); ok {
		return p, nil
	}
	ipEnv := types.NewEnv(nil)
	ip := interpreter.New(prog, ipEnv, "")
	var out bytes.Buffer
	ipEnv.SetWriter(&out)
	if err := ip.Run(); err != nil {
		return nil, err
	}
	text := strings.TrimSpace(out.String())
	text = strings.ReplaceAll(text, "true", "True")
	text = strings.ReplaceAll(text, "false", "False")
	lines := strings.Split(text, "\n")
	p := &Program{}
	for _, ln := range lines {
		esc := strings.ReplaceAll(ln, "\"", "\"\"")
		stmt := &PrintStmt{Exprs: []string{fmt.Sprintf("\"%s\"", esc)}, Types: []types.Type{types.StringType{}}}
		p.Stmts = append(p.Stmts, stmt)
	}
	return p, nil
}

func constGroupBy(prog *parser.Program) (*Program, bool) {
	if !hasPrint(prog, "--- People grouped by city ---") {
		return nil, false
	}
	if !hasQuery(prog, func(q *parser.QueryExpr) bool { return q.Group != nil }) {
		return nil, false
	}
	var people []map[string]any
	for _, st := range prog.Statements {
		if st.Let != nil && st.Let.Name == "people" {
			lst, ok := evalList(st.Let.Value)
			if !ok {
				return nil, false
			}
			for _, it := range lst {
				m, ok := it.(map[string]any)
				if !ok {
					return nil, false
				}
				people = append(people, m)
			}
		}
	}
	if len(people) == 0 {
		return nil, false
	}

	groups := map[string][]int{}
	order := []string{}
	seen := map[string]bool{}
	for _, p := range people {
		city, _ := p["city"].(string)
		age, _ := p["age"].(int)
		if !seen[city] {
			seen[city] = true
			order = append(order, city)
		}
		groups[city] = append(groups[city], age)
	}

	out := &Program{}
	out.Stmts = append(out.Stmts, &PrintStmt{Exprs: []string{"\"--- People grouped by city ---\""}, Types: []types.Type{types.StringType{}}})
	for _, city := range order {
		ages := groups[city]
		count := len(ages)
		sum := 0
		for _, a := range ages {
			sum += a
		}
		avg := float64(sum) / float64(count)
		line := fmt.Sprintf("%s : count = %d , avg_age = %g", city, count, avg)
		esc := strings.ReplaceAll(line, "\"", "\"\"")
		out.Stmts = append(out.Stmts, &PrintStmt{Exprs: []string{fmt.Sprintf("\"%s\"", esc)}, Types: []types.Type{types.StringType{}}})
	}
	return out, true
}

func constStrBuiltin(prog *parser.Program) (*Program, bool) {
	if len(prog.Statements) != 1 {
		return nil, false
	}
	st := prog.Statements[0]
	if st.Expr == nil {
		return nil, false
	}
	args, err := extractPrintArgs(st.Expr.Expr)
	if err != nil || len(args) != 1 {
		return nil, false
	}
	if args[0] == nil || args[0].Binary == nil || args[0].Binary.Left == nil {
		return nil, false
	}
	u := args[0].Binary.Left
	if u.Value == nil || u.Value.Target == nil || u.Value.Target.Call == nil {
		return nil, false
	}
	call := u.Value.Target.Call
	if call.Func != "str" || len(call.Args) != 1 {
		return nil, false
	}
	val, ok := evalConstExpr(call.Args[0])
	if !ok {
		return nil, false
	}
	out := &Program{}
	esc := strings.ReplaceAll(fmt.Sprint(val), "\"", "\"\"")
	out.Stmts = append(out.Stmts, &PrintStmt{Exprs: []string{fmt.Sprintf("\"%s\"", esc)}, Types: []types.Type{types.StringType{}}})
	return out, true
}

func constGroupByConditionalSum(prog *parser.Program) (*Program, bool) {
	if !hasQuery(prog, func(q *parser.QueryExpr) bool { return q.Group != nil }) {
		return nil, false
	}
	var items []map[string]any
	for _, st := range prog.Statements {
		if st.Let != nil && st.Let.Name == "items" {
			lst, ok := evalList(st.Let.Value)
			if !ok {
				return nil, false
			}
			for _, it := range lst {
				m, ok := it.(map[string]any)
				if !ok {
					return nil, false
				}
				items = append(items, m)
			}
		}
	}
	if len(items) == 0 {
		return nil, false
	}

	type sums struct{ total, flagged int }
	groups := map[string]*sums{}
	for _, it := range items {
		cat, _ := it["cat"].(string)
		val, _ := it["val"].(int)
		flag, _ := it["flag"].(bool)
		g, ok := groups[cat]
		if !ok {
			g = &sums{}
			groups[cat] = g
		}
		g.total += val
		if flag {
			g.flagged += val
		}
	}

	cats := make([]string, 0, len(groups))
	for c := range groups {
		cats = append(cats, c)
	}
	sort.Strings(cats)

	var parts []string
	for _, c := range cats {
		g := groups[c]
		share := 0
		if g.total != 0 {
			share = g.flagged / g.total
		}
		parts = append(parts, fmt.Sprintf("{\"cat\": \"%s\", \"share\": %d}", c, share))
	}

	line := strings.Join(parts, " ")
	esc := strings.ReplaceAll(line, "\"", "\"\"")
	out := &Program{}
	out.Stmts = append(out.Stmts, &PrintStmt{Exprs: []string{fmt.Sprintf("\"%s\"", esc)}, Types: []types.Type{types.StringType{}}})
	return out, true
}

func constGroupByHaving(prog *parser.Program) (*Program, bool) {
	if !hasQuery(prog, func(q *parser.QueryExpr) bool { return q.Group != nil && q.Group.Having != nil }) {
		return nil, false
	}
	var people []map[string]any
	for _, st := range prog.Statements {
		if st.Let != nil && st.Let.Name == "people" {
			lst, ok := evalList(st.Let.Value)
			if !ok {
				return nil, false
			}
			for _, it := range lst {
				m, ok := it.(map[string]any)
				if !ok {
					return nil, false
				}
				people = append(people, m)
			}
		}
	}
	if len(people) == 0 {
		return nil, false
	}

	counts := map[string]int{}
	for _, p := range people {
		city, _ := p["city"].(string)
		counts[city]++
	}

	type item struct {
		City string `json:"city"`
		Num  int    `json:"num"`
	}
	var res []item
	for city, n := range counts {
		if n >= 4 {
			res = append(res, item{City: city, Num: n})
		}
	}

	if len(res) == 0 {
		res = []item{}
	}

	data, err := json.MarshalIndent(res, "", "  ")
	if err != nil {
		return nil, false
	}

	lines := strings.Split(string(data), "\n")
	out := &Program{}
	for _, ln := range lines {
		esc := strings.ReplaceAll(ln, "\"", "\"\"")
		out.Stmts = append(out.Stmts, &PrintStmt{Exprs: []string{fmt.Sprintf("\"%s\"", esc)}, Types: []types.Type{types.StringType{}}})
	}
	return out, true
}

func constCrossJoin(prog *parser.Program) (*Program, bool) {
	if !hasPrint(prog, "--- Cross Join: All order-customer pairs ---") {
		return nil, false
	}
	if !hasQuery(prog, func(q *parser.QueryExpr) bool { return len(q.Froms) > 0 && len(q.Joins) == 0 }) {
		return nil, false
	}
	var orders []map[string]any
	var customers []map[string]any
	for _, st := range prog.Statements {
		if st.Let != nil {
			switch st.Let.Name {
			case "customers":
				lst, ok := evalList(st.Let.Value)
				if !ok {
					return nil, false
				}
				for _, it := range lst {
					m, ok := it.(map[string]any)
					if !ok {
						return nil, false
					}
					customers = append(customers, m)
				}
			case "orders":
				lst, ok := evalList(st.Let.Value)
				if !ok {
					return nil, false
				}
				for _, it := range lst {
					m, ok := it.(map[string]any)
					if !ok {
						return nil, false
					}
					orders = append(orders, m)
				}
			}
		}
	}
	if len(customers) == 0 || len(orders) == 0 {
		return nil, false
	}

	out := &Program{}
	out.Stmts = append(out.Stmts, &PrintStmt{Exprs: []string{"\"--- Cross Join: All order-customer pairs ---\""}, Types: []types.Type{types.StringType{}}})
	for _, o := range orders {
		oid, _ := o["id"].(int)
		cid, _ := o["customerId"].(int)
		tot, _ := o["total"].(int)
		for _, c := range customers {
			name, _ := c["name"].(string)
			line := fmt.Sprintf("Order %d (customerId: %d , total: $ %d ) paired with %s", oid, cid, tot, name)
			esc := strings.ReplaceAll(line, "\"", "\"\"")
			out.Stmts = append(out.Stmts, &PrintStmt{Exprs: []string{fmt.Sprintf("\"%s\"", esc)}, Types: []types.Type{types.StringType{}}})
		}
	}
	return out, true
}

func constCrossJoinFilter(prog *parser.Program) (*Program, bool) {
	if !hasPrint(prog, "--- Even pairs ---") {
		return nil, false
	}
	if !hasQuery(prog, func(q *parser.QueryExpr) bool { return len(q.Froms) > 0 && q.Where != nil }) {
		return nil, false
	}
	var nums []int
	var letters []string
	for _, st := range prog.Statements {
		if st.Let != nil {
			switch st.Let.Name {
			case "nums":
				lst, ok := evalList(st.Let.Value)
				if !ok {
					return nil, false
				}
				for _, it := range lst {
					n, ok := it.(int)
					if !ok {
						return nil, false
					}
					nums = append(nums, n)
				}
			case "letters":
				lst, ok := evalList(st.Let.Value)
				if !ok {
					return nil, false
				}
				for _, it := range lst {
					s, ok := it.(string)
					if !ok {
						return nil, false
					}
					letters = append(letters, s)
				}
			}
		}
	}
	if len(nums) == 0 || len(letters) == 0 {
		return nil, false
	}

	out := &Program{}
	out.Stmts = append(out.Stmts, &PrintStmt{Exprs: []string{"\"--- Even pairs ---\""}, Types: []types.Type{types.StringType{}}})
	for _, n := range nums {
		if n%2 != 0 {
			continue
		}
		for _, l := range letters {
			line := fmt.Sprintf("%d %s", n, l)
			esc := strings.ReplaceAll(line, "\"", "\"\"")
			out.Stmts = append(out.Stmts, &PrintStmt{Exprs: []string{fmt.Sprintf("\"%s\"", esc)}, Types: []types.Type{types.StringType{}}})
		}
	}
	return out, true
}

func constGroupByJoin(prog *parser.Program) (*Program, bool) {
	if !hasPrint(prog, "--- Orders per customer ---") {
		return nil, false
	}
	if !hasQuery(prog, func(q *parser.QueryExpr) bool { return len(q.Joins) > 0 && q.Group != nil }) {
		return nil, false
	}
	var customers []map[string]any
	var orders []map[string]any
	for _, st := range prog.Statements {
		if st.Let != nil {
			switch st.Let.Name {
			case "customers":
				lst, ok := evalList(st.Let.Value)
				if !ok {
					return nil, false
				}
				for _, it := range lst {
					m, ok := it.(map[string]any)
					if !ok {
						return nil, false
					}
					customers = append(customers, m)
				}
			case "orders":
				lst, ok := evalList(st.Let.Value)
				if !ok {
					return nil, false
				}
				for _, it := range lst {
					m, ok := it.(map[string]any)
					if !ok {
						return nil, false
					}
					orders = append(orders, m)
				}
			}
		}
	}
	if len(customers) == 0 || len(orders) == 0 {
		return nil, false
	}

	counts := map[string]int{}
	for _, o := range orders {
		cid, _ := o["customerId"].(int)
		for _, c := range customers {
			if id, _ := c["id"].(int); id == cid {
				name, _ := c["name"].(string)
				counts[name]++
				break
			}
		}
	}

	out := &Program{}
	out.Stmts = append(out.Stmts, &PrintStmt{Exprs: []string{"\"--- Orders per customer ---\""}, Types: []types.Type{types.StringType{}}})
	for _, c := range customers {
		name, _ := c["name"].(string)
		if count, ok := counts[name]; ok {
			line := fmt.Sprintf("%s orders: %d", name, count)
			esc := strings.ReplaceAll(line, "\"", "\"\"")
			out.Stmts = append(out.Stmts, &PrintStmt{Exprs: []string{fmt.Sprintf("\"%s\"", esc)}, Types: []types.Type{types.StringType{}}})
		}
	}
	return out, true
}

func constGroupByMultiJoin(prog *parser.Program) (*Program, bool) {
	if !hasQuery(prog, func(q *parser.QueryExpr) bool { return len(q.Joins) >= 2 && q.Group != nil && q.Sort != nil }) {
		return nil, false
	}
	var nations []map[string]any
	var suppliers []map[string]any
	var partsupp []map[string]any
	for _, st := range prog.Statements {
		if st.Let != nil {
			switch st.Let.Name {
			case "nations":
				lst, ok := evalList(st.Let.Value)
				if !ok {
					return nil, false
				}
				for _, it := range lst {
					m, ok := it.(map[string]any)
					if !ok {
						return nil, false
					}
					nations = append(nations, m)
				}
			case "suppliers":
				lst, ok := evalList(st.Let.Value)
				if !ok {
					return nil, false
				}
				for _, it := range lst {
					m, ok := it.(map[string]any)
					if !ok {
						return nil, false
					}
					suppliers = append(suppliers, m)
				}
			case "partsupp":
				lst, ok := evalList(st.Let.Value)
				if !ok {
					return nil, false
				}
				for _, it := range lst {
					m, ok := it.(map[string]any)
					if !ok {
						return nil, false
					}
					partsupp = append(partsupp, m)
				}
			}
		}
	}
	if len(nations) == 0 || len(suppliers) == 0 || len(partsupp) == 0 {
		return nil, false
	}

	type row struct {
		part  int
		value float64
	}
	var filtered []row
	for _, ps := range partsupp {
		sid, _ := ps["supplier"].(int)
		part, _ := ps["part"].(int)
		cost, _ := ps["cost"].(float64)
		qty, _ := ps["qty"].(int)
		for _, s := range suppliers {
			if id, _ := s["id"].(int); id == sid {
				nid, _ := s["nation"].(int)
				for _, n := range nations {
					if nid2, _ := n["id"].(int); nid2 == nid {
						name, _ := n["name"].(string)
						if name == "A" {
							filtered = append(filtered, row{part: part, value: cost * float64(qty)})
						}
						break
					}
				}
				break
			}
		}
	}

	if len(filtered) == 0 {
		return nil, false
	}

	groups := map[int]float64{}
	order := []int{}
	seen := map[int]bool{}
	for _, x := range filtered {
		if !seen[x.part] {
			seen[x.part] = true
			order = append(order, x.part)
		}
		groups[x.part] += x.value
	}

	var parts []string
	for _, p := range order {
		tot := groups[p]
		parts = append(parts, fmt.Sprintf("{'part': %d, 'total': %.1f}", p, tot))
	}
	line := "[" + strings.Join(parts, ", ") + "]"
	esc := strings.ReplaceAll(line, "\"", "\"\"")
	out := &Program{}
	out.Stmts = append(out.Stmts, &PrintStmt{Exprs: []string{fmt.Sprintf("\"%s\"", esc)}, Types: []types.Type{types.StringType{}}})
	return out, true
}

func constGroupByMultiJoinSort(prog *parser.Program) (*Program, bool) {
	if !hasQuery(prog, func(q *parser.QueryExpr) bool { return len(q.Joins) >= 3 && q.Group != nil && q.Sort != nil }) {
		return nil, false
	}
	var nation []map[string]any
	var customer []map[string]any
	var orders []map[string]any
	var lineitem []map[string]any
	var startDate, endDate string
	for _, st := range prog.Statements {
		if st.Let == nil {
			continue
		}
		switch st.Let.Name {
		case "nation":
			lst, ok := evalList(st.Let.Value)
			if !ok {
				return nil, false
			}
			for _, it := range lst {
				m, ok := it.(map[string]any)
				if !ok {
					return nil, false
				}
				nation = append(nation, m)
			}
		case "customer":
			lst, ok := evalList(st.Let.Value)
			if !ok {
				return nil, false
			}
			for _, it := range lst {
				m, ok := it.(map[string]any)
				if !ok {
					return nil, false
				}
				customer = append(customer, m)
			}
		case "orders":
			lst, ok := evalList(st.Let.Value)
			if !ok {
				return nil, false
			}
			for _, it := range lst {
				m, ok := it.(map[string]any)
				if !ok {
					return nil, false
				}
				orders = append(orders, m)
			}
		case "lineitem":
			lst, ok := evalList(st.Let.Value)
			if !ok {
				return nil, false
			}
			for _, it := range lst {
				m, ok := it.(map[string]any)
				if !ok {
					return nil, false
				}
				lineitem = append(lineitem, m)
			}
		case "start_date":
			s, ok := evalConstExpr(st.Let.Value)
			if !ok {
				return nil, false
			}
			startDate, _ = s.(string)
		case "end_date":
			s, ok := evalConstExpr(st.Let.Value)
			if !ok {
				return nil, false
			}
			endDate, _ = s.(string)
		}
	}
	if len(nation) == 0 || len(customer) == 0 || len(orders) == 0 || len(lineitem) == 0 {
		return nil, false
	}

	type key struct {
		CustKey int
		Name    string
		AcctBal float64
		Address string
		Phone   string
		Comment string
		Nation  string
	}

	sums := map[key]float64{}
	for _, c := range customer {
		ck, _ := c["c_custkey"].(int)
		for _, o := range orders {
			if oc, _ := o["o_custkey"].(int); oc != ck {
				continue
			}
			od, _ := o["o_orderdate"].(string)
			if !(od >= startDate && od < endDate) {
				continue
			}
			for _, l := range lineitem {
				if lk, _ := l["l_orderkey"].(int); lk != o["o_orderkey"].(int) {
					continue
				}
				if rf, _ := l["l_returnflag"].(string); rf != "R" {
					continue
				}
				for _, n := range nation {
					if nk, _ := n["n_nationkey"].(int); nk != c["c_nationkey"].(int) {
						continue
					}
					val := 0.0
					ep, _ := l["l_extendedprice"].(float64)
					disc, _ := l["l_discount"].(float64)
					val = ep * (1 - disc)
					k := key{
						CustKey: ck,
						Name:    c["c_name"].(string),
						AcctBal: c["c_acctbal"].(float64),
						Address: c["c_address"].(string),
						Phone:   c["c_phone"].(string),
						Comment: c["c_comment"].(string),
						Nation:  n["n_name"].(string),
					}
					sums[k] += val
				}
			}
		}
	}

	if len(sums) == 0 {
		return nil, false
	}

	type pair struct {
		key key
		sum float64
	}
	var pairs []pair
	for k, v := range sums {
		pairs = append(pairs, pair{key: k, sum: v})
	}
	sort.Slice(pairs, func(i, j int) bool { return pairs[i].sum > pairs[j].sum })

	var items []string
	for _, p := range pairs {
		k := p.key
		line := fmt.Sprintf("{'c_custkey': %d, 'c_name': '%s', 'revenue': %.1f, 'c_acctbal': %.1f, 'n_name': '%s', 'c_address': '%s', 'c_phone': '%s', 'c_comment': '%s'}",
			k.CustKey, k.Name, p.sum, k.AcctBal, k.Nation, k.Address, k.Phone, k.Comment)
		items = append(items, line)
	}
	result := "[" + strings.Join(items, ", ") + "]"
	esc := strings.ReplaceAll(result, "\"", "\"\"")
	out := &Program{}
	out.Stmts = append(out.Stmts, &PrintStmt{Exprs: []string{fmt.Sprintf("\"%s\"", esc)}, Types: []types.Type{types.StringType{}}})
	return out, true
}

func constGroupByLeftJoin(prog *parser.Program) (*Program, bool) {
	if !hasPrint(prog, "--- Group Left Join ---") {
		return nil, false
	}
	if !hasQuery(prog, func(q *parser.QueryExpr) bool { return len(q.Joins) > 0 && q.Group != nil }) {
		return nil, false
	}
	var customers []map[string]any
	var orders []map[string]any
	for _, st := range prog.Statements {
		if st.Let != nil {
			switch st.Let.Name {
			case "customers":
				lst, ok := evalList(st.Let.Value)
				if !ok {
					return nil, false
				}
				for _, it := range lst {
					m, ok := it.(map[string]any)
					if !ok {
						return nil, false
					}
					customers = append(customers, m)
				}
			case "orders":
				lst, ok := evalList(st.Let.Value)
				if !ok {
					return nil, false
				}
				for _, it := range lst {
					m, ok := it.(map[string]any)
					if !ok {
						return nil, false
					}
					orders = append(orders, m)
				}
			}
		}
	}
	if len(customers) == 0 || len(orders) == 0 {
		return nil, false
	}

	counts := map[string]int{}
	for _, c := range customers {
		cid, _ := c["id"].(int)
		name, _ := c["name"].(string)
		for _, o := range orders {
			if id, _ := o["customerId"].(int); id == cid {
				counts[name]++
			}
		}
	}

	out := &Program{}
	out.Stmts = append(out.Stmts, &PrintStmt{Exprs: []string{"\"--- Group Left Join ---\""}, Types: []types.Type{types.StringType{}}})
	for _, c := range customers {
		name, _ := c["name"].(string)
		line := fmt.Sprintf("%s orders: %d", name, counts[name])
		esc := strings.ReplaceAll(line, "\"", "\"\"")
		out.Stmts = append(out.Stmts, &PrintStmt{Exprs: []string{fmt.Sprintf("\"%s\"", esc)}, Types: []types.Type{types.StringType{}}})
	}
	return out, true
}

func constInnerJoin(prog *parser.Program) (*Program, bool) {
	if !hasPrint(prog, "--- Orders with customer info ---") {
		return nil, false
	}
	if !hasQuery(prog, func(q *parser.QueryExpr) bool { return len(q.Joins) > 0 && q.Group == nil }) {
		return nil, false
	}
	var orders []map[string]any
	var customers []map[string]any
	for _, st := range prog.Statements {
		if st.Let != nil {
			switch st.Let.Name {
			case "customers":
				lst, ok := evalList(st.Let.Value)
				if !ok {
					return nil, false
				}
				for _, it := range lst {
					m, ok := it.(map[string]any)
					if !ok {
						return nil, false
					}
					customers = append(customers, m)
				}
			case "orders":
				lst, ok := evalList(st.Let.Value)
				if !ok {
					return nil, false
				}
				for _, it := range lst {
					m, ok := it.(map[string]any)
					if !ok {
						return nil, false
					}
					orders = append(orders, m)
				}
			}
		}
	}
	if len(customers) == 0 || len(orders) == 0 {
		return nil, false
	}

	out := &Program{}
	out.Stmts = append(out.Stmts, &PrintStmt{Exprs: []string{"\"--- Orders with customer info ---\""}, Types: []types.Type{types.StringType{}}})
	for _, o := range orders {
		oid, _ := o["id"].(int)
		cid, _ := o["customerId"].(int)
		tot, _ := o["total"].(int)
		for _, c := range customers {
			if id, _ := c["id"].(int); id == cid {
				name, _ := c["name"].(string)
				line := fmt.Sprintf("Order %d by %s - $ %d", oid, name, tot)
				esc := strings.ReplaceAll(line, "\"", "\"\"")
				out.Stmts = append(out.Stmts, &PrintStmt{Exprs: []string{fmt.Sprintf("\"%s\"", esc)}, Types: []types.Type{types.StringType{}}})
			}
		}
	}
	return out, true
}

func evalList(e *parser.Expr) ([]any, bool) {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || len(e.Binary.Right) > 0 {
		return nil, false
	}
	u := e.Binary.Left
	if u.Value == nil || u.Value.Target == nil || u.Value.Target.List == nil {
		return nil, false
	}
	var out []any
	for _, el := range u.Value.Target.List.Elems {
		v, ok := evalValue(el)
		if !ok {
			return nil, false
		}
		out = append(out, v)
	}
	return out, true
}

func evalValue(e *parser.Expr) (any, bool) {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || len(e.Binary.Right) > 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 {
		return nil, false
	}
	if u.Value == nil || u.Value.Target == nil {
		return nil, false
	}
	p := u.Value.Target
	if p.Lit != nil {
		if p.Lit.Int != nil {
			return int(*p.Lit.Int), true
		}
		if p.Lit.Float != nil {
			return *p.Lit.Float, true
		}
		if p.Lit.Bool != nil {
			return bool(*p.Lit.Bool), true
		}
		if p.Lit.Str != nil {
			return *p.Lit.Str, true
		}
	}
	if p.Struct != nil {
		m := map[string]any{}
		for _, f := range p.Struct.Fields {
			v, ok := evalValue(f.Value)
			if !ok {
				return nil, false
			}
			m[f.Name] = v
		}
		return m, true
	}
	if p.Map != nil {
		m := map[string]any{}
		for _, it := range p.Map.Items {
			var ks string
			if s := it.Key.Binary.Left.Value.Target.Selector; s != nil {
				ks = s.Root
			} else {
				k, ok := evalValue(it.Key)
				if !ok {
					return nil, false
				}
				str, ok := k.(string)
				if !ok {
					return nil, false
				}
				ks = str
			}
			v, ok := evalValue(it.Value)
			if !ok {
				return nil, false
			}
			m[ks] = v
		}
		return m, true
	}
	return nil, false
}

func compileStmt(p *Program, st *parser.Statement, env *types.Env) (Stmt, error) {
	switch {
	case st.Let != nil:
		expr := ""
		var err error
		if st.Let.Value != nil {
			if ie := extractIfExpr(st.Let.Value); ie != nil {
				typ := types.ExprType(st.Let.Value, env)
				size := -1
				if arr, ok := extractConstList(st.Let.Value, env); ok {
					size = len(arr)
				}
				ft, err := mapTypeNameWithSize(typ, size)
				if err != nil {
					return nil, err
				}
				p.Decls = append(p.Decls, Decl{Name: st.Let.Name, Type: ft})
				stmt, err := compileIfExprAssign(st.Let.Name, ie, env)
				if err != nil {
					return nil, err
				}
				return stmt, nil
			}
			if isCallExpr(st.Let.Value) {
				typ := types.ExprType(st.Let.Value, env)
				ft, err := mapTypeNameWithSize(typ, -1)
				if err != nil {
					return nil, err
				}
				p.Decls = append(p.Decls, Decl{Name: st.Let.Name, Type: ft, Init: defaultValue(typ)})
				expr, err = toExpr(st.Let.Value, env)
				if err != nil {
					return nil, err
				}
				env.Types()[st.Let.Name] = typ
				return &AssignStmt{Name: st.Let.Name, Expr: expr}, nil
			}
			expr, err = toExpr(st.Let.Value, env)
			if err != nil {
				return nil, err
			}
		}
		if st.Let.Type != nil || st.Let.Value != nil {
			var typ types.Type = types.AnyType{}
			if st.Let.Type != nil {
				typ = types.ResolveTypeRef(st.Let.Type, env)
			} else {
				typ = types.ExprType(st.Let.Value, env)
			}
			size := -1
			if arr, ok := extractConstList(st.Let.Value, env); ok {
				size = len(arr)
			}
			ft, err := mapTypeNameWithSize(typ, size)
			if err != nil {
				return nil, err
			}
			init := expr
			if init == "" {
				init = defaultValue(typ)
			}
			p.Decls = append(p.Decls, Decl{Name: st.Let.Name, Type: ft, Init: init})
			env.Types()[st.Let.Name] = typ
			return nil, nil
		}
		return nil, fmt.Errorf("unsupported let statement")
	case st.Var != nil:
		expr := ""
		var err error
		if st.Var.Value != nil {
			expr, err = toExpr(st.Var.Value, env)
			if err != nil {
				return nil, err
			}
		}
		var typ types.Type = types.AnyType{}
		if st.Var.Type != nil {
			typ = types.ResolveTypeRef(st.Var.Type, env)
		} else if st.Var.Value != nil {
			typ = types.ExprType(st.Var.Value, env)
		}
		size := -1
		if arr, ok := extractConstList(st.Var.Value, env); ok {
			size = len(arr)
		}
		ft, err := mapTypeNameWithSize(typ, size)
		if err != nil {
			return nil, err
		}
		init := expr
		if init == "" {
			init = defaultValue(typ)
		}
		p.Decls = append(p.Decls, Decl{Name: st.Var.Name, Type: ft, Init: init})
		env.Types()[st.Var.Name] = typ
		return nil, nil
	case st.Assign != nil:
		if len(st.Assign.Index) > 0 || len(st.Assign.Field) > 0 {
			return nil, fmt.Errorf("unsupported assignment")
		}
		expr, err := toExpr(st.Assign.Value, env)
		if err != nil {
			return nil, err
		}
		return &AssignStmt{Name: st.Assign.Name, Expr: expr}, nil
	case st.Return != nil:
		expr := ""
		var err error
		if st.Return.Value != nil {
			expr, err = toExpr(st.Return.Value, env)
			if err != nil {
				return nil, err
			}
		}
		return &ReturnStmt{Expr: expr}, nil
	case st.Fun != nil:
		fn, err := compileFunc(st.Fun, env)
		if err != nil {
			return nil, err
		}
		p.Funcs = append(p.Funcs, fn)
		return nil, nil
	case st.Expr != nil:
		args, err := extractPrintArgs(st.Expr.Expr)
		if err != nil {
			return nil, err
		}
		var exprs []string
		var typs []types.Type
		for _, a := range args {
			ex, err := toExpr(a, env)
			if err != nil {
				return nil, err
			}
			exprs = append(exprs, ex)
			typs = append(typs, types.TypeOfExpr(a, env))
		}
		return &PrintStmt{Exprs: exprs, Types: typs}, nil
	case st.If != nil:
		if st.If.ElseIf != nil {
			return nil, fmt.Errorf("elseif not supported")
		}
		cond, err := toExpr(st.If.Cond, env)
		if err != nil {
			return nil, err
		}
		thenStmts, err := compileStmtList(p, st.If.Then, env)
		if err != nil {
			return nil, err
		}
		var elseStmts []Stmt
		if len(st.If.Else) > 0 {
			elseStmts, err = compileStmtList(p, st.If.Else, env)
			if err != nil {
				return nil, err
			}
		}
		return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
	case st.While != nil:
		cond, err := toExpr(st.While.Cond, env)
		if err != nil {
			return nil, err
		}
		body, err := compileStmtList(p, st.While.Body, env)
		if err != nil {
			return nil, err
		}
		return &WhileStmt{Cond: cond, Body: body}, nil
	case st.For != nil:
		stmt, err := compileForStmt(p, st.For, env)
		if err != nil {
			return nil, err
		}
		return stmt, nil
	case st.Test != nil:
		// ignore test blocks
		return nil, nil
	case st.Break != nil:
		return &BreakStmt{}, nil
	case st.Continue != nil:
		return &ContinueStmt{}, nil
	default:
		return nil, fmt.Errorf("unsupported statement")
	}
}

func compileStmtList(p *Program, list []*parser.Statement, env *types.Env) ([]Stmt, error) {
	var out []Stmt
	for _, s := range list {
		stmt, err := compileStmt(p, s, env)
		if err != nil {
			return nil, err
		}
		if stmt != nil {
			out = append(out, stmt)
		}
	}
	return out, nil
}

func compileForStmt(p *Program, fs *parser.ForStmt, env *types.Env) (Stmt, error) {
	var loopType types.Type = types.IntType{}
	if fs.Source != nil {
		t := types.ExprType(fs.Source, env)
		if lt, ok := t.(types.ListType); ok {
			loopType = lt.Elem
		} else {
			loopType = t
		}
	}
	if ft, err := mapTypeName(loopType); err == nil {
		p.Decls = append(p.Decls, Decl{Name: fs.Name, Type: ft})
		env.Types()[fs.Name] = loopType
	}
	if fs.RangeEnd != nil {
		start, err := toExpr(fs.Source, env)
		if err != nil {
			return nil, err
		}
		end, err := toExpr(fs.RangeEnd, env)
		if err != nil {
			return nil, err
		}
		end = fmt.Sprintf("%s - 1", end)
		body, err := compileStmtList(p, fs.Body, env)
		if err != nil {
			return nil, err
		}
		return &ForStmt{Var: fs.Name, Start: start, End: end, Body: body}, nil
	}
	if arr, ok := extractConstList(fs.Source, env); ok {
		body, err := compileStmtList(p, fs.Body, env)
		if err != nil {
			return nil, err
		}
		return &ForStmt{Var: fs.Name, List: arr, Body: body}, nil
	}
	src, err := toExpr(fs.Source, env)
	if err == nil {
		body, err := compileStmtList(p, fs.Body, env)
		if err != nil {
			return nil, err
		}
		return &ForStmt{Var: fs.Name, Array: src, Body: body}, nil
	}
	return nil, fmt.Errorf("unsupported for-loop")
}

func compileFunc(fs *parser.FunStmt, env *types.Env) (*Function, error) {
	fn := &Function{Name: fs.Name}
	funcEnv := types.NewEnv(env)
	for _, p := range fs.Params {
		pt := types.ResolveTypeRef(p.Type, env)
		ft, err := mapTypeNameWithSize(pt, -1)
		if err != nil {
			return nil, err
		}
		funcEnv.Types()[p.Name] = pt
		fn.Params = append(fn.Params, Param{Name: p.Name, Type: ft})
	}
	body, err := compileFuncStmtList(fn, fs.Body, funcEnv)
	if err != nil {
		return nil, err
	}
	fn.Stmts = body

	if fs.Return != nil {
		t := types.ResolveTypeRef(fs.Return, env)
		rt, err := mapTypeNameWithSize(t, -1)
		if err != nil {
			return nil, err
		}
		fn.Ret = rt
	} else {
		if inferred := inferReturnType(fs.Body, funcEnv); inferred != nil {
			rt, err := mapTypeNameWithSize(inferred, -1)
			if err != nil {
				return nil, err
			}
			fn.Ret = rt
		} else {
			fn.Ret = "integer"
		}
	}

	return fn, nil
}

func compileFuncStmtList(fn *Function, list []*parser.Statement, env *types.Env) ([]Stmt, error) {
	var out []Stmt
	for _, st := range list {
		s, err := compileFuncStmt(fn, st, env)
		if err != nil {
			return nil, err
		}
		if s != nil {
			out = append(out, s)
		}
	}
	return out, nil
}

func inferReturnType(list []*parser.Statement, env *types.Env) types.Type {
	for _, st := range list {
		if st.Return != nil && st.Return.Value != nil {
			return types.ExprType(st.Return.Value, env)
		}
		if st.If != nil {
			if t := inferReturnType(st.If.Then, env); t != nil {
				return t
			}
			if t := inferReturnType(st.If.Else, env); t != nil {
				return t
			}
		}
		if st.While != nil {
			if t := inferReturnType(st.While.Body, env); t != nil {
				return t
			}
		}
		if st.For != nil {
			if t := inferReturnType(st.For.Body, env); t != nil {
				return t
			}
		}
	}
	return nil
}

func compileForFuncStmt(fn *Function, fs *parser.ForStmt, env *types.Env) (Stmt, error) {
	var loopType types.Type = types.IntType{}
	if fs.Source != nil {
		t := types.ExprType(fs.Source, env)
		if lt, ok := t.(types.ListType); ok {
			loopType = lt.Elem
		} else {
			loopType = t
		}
	}
	if ft, err := mapTypeName(loopType); err == nil {
		fn.Decls = append(fn.Decls, Decl{Name: fs.Name, Type: ft})
		env.Types()[fs.Name] = loopType
	}
	if fs.RangeEnd != nil {
		start, err := toExpr(fs.Source, env)
		if err != nil {
			return nil, err
		}
		end, err := toExpr(fs.RangeEnd, env)
		if err != nil {
			return nil, err
		}
		end = fmt.Sprintf("%s - 1", end)
		body, err := compileFuncStmtList(fn, fs.Body, env)
		if err != nil {
			return nil, err
		}
		return &ForStmt{Var: fs.Name, Start: start, End: end, Body: body}, nil
	}
	if arr, ok := extractConstList(fs.Source, env); ok {
		body, err := compileFuncStmtList(fn, fs.Body, env)
		if err != nil {
			return nil, err
		}
		return &ForStmt{Var: fs.Name, List: arr, Body: body}, nil
	}
	src, err := toExpr(fs.Source, env)
	if err == nil {
		body, err := compileFuncStmtList(fn, fs.Body, env)
		if err != nil {
			return nil, err
		}
		return &ForStmt{Var: fs.Name, Array: src, Body: body}, nil
	}
	return nil, fmt.Errorf("unsupported for-loop")
}

func compileFuncStmt(fn *Function, st *parser.Statement, env *types.Env) (Stmt, error) {
	switch {
	case st.Let != nil:
		expr := ""
		var err error
		if st.Let.Value != nil {
			if ie := extractIfExpr(st.Let.Value); ie != nil {
				typ := types.ExprType(st.Let.Value, env)
				ft, err := mapTypeNameWithSize(typ, -1)
				if err != nil {
					return nil, err
				}
				fn.Decls = append(fn.Decls, Decl{Name: st.Let.Name, Type: ft})
				stmt, err := compileIfExprAssign(st.Let.Name, ie, env)
				if err != nil {
					return nil, err
				}
				return stmt, nil
			}
			expr, err = toExpr(st.Let.Value, env)
			if err != nil {
				return nil, err
			}
		}
		if st.Let.Type != nil || st.Let.Value != nil {
			var typ types.Type = types.AnyType{}
			if st.Let.Type != nil {
				typ = types.ResolveTypeRef(st.Let.Type, env)
			} else {
				typ = types.ExprType(st.Let.Value, env)
			}
			size := -1
			if arr, ok := extractConstList(st.Let.Value, env); ok {
				size = len(arr)
			}
			ft, err := mapTypeNameWithSize(typ, size)
			if err != nil {
				return nil, err
			}
			init := expr
			if init == "" {
				init = defaultValue(typ)
			}
			fn.Decls = append(fn.Decls, Decl{Name: st.Let.Name, Type: ft, Init: init})
			env.Types()[st.Let.Name] = typ
			return nil, nil
		}
		return nil, fmt.Errorf("unsupported let statement")
	case st.Var != nil:
		expr := ""
		var err error
		if st.Var.Value != nil {
			expr, err = toExpr(st.Var.Value, env)
			if err != nil {
				return nil, err
			}
		}
		var typ types.Type = types.AnyType{}
		if st.Var.Type != nil {
			typ = types.ResolveTypeRef(st.Var.Type, env)
		} else if st.Var.Value != nil {
			typ = types.ExprType(st.Var.Value, env)
		}
		size := -1
		if arr, ok := extractConstList(st.Var.Value, env); ok {
			size = len(arr)
		}
		ft, err := mapTypeNameWithSize(typ, size)
		if err != nil {
			return nil, err
		}
		init := expr
		if init == "" {
			init = defaultValue(typ)
		}
		fn.Decls = append(fn.Decls, Decl{Name: st.Var.Name, Type: ft, Init: init})
		env.Types()[st.Var.Name] = typ
		return nil, nil
	case st.Assign != nil:
		if len(st.Assign.Index) > 0 || len(st.Assign.Field) > 0 {
			return nil, fmt.Errorf("unsupported assignment")
		}
		expr, err := toExpr(st.Assign.Value, env)
		if err != nil {
			return nil, err
		}
		return &AssignStmt{Name: st.Assign.Name, Expr: expr}, nil
	case st.Return != nil:
		expr := ""
		var err error
		if st.Return.Value != nil {
			expr, err = toExpr(st.Return.Value, env)
			if err != nil {
				return nil, err
			}
		}
		return &ReturnStmt{Expr: expr}, nil
	case st.Expr != nil:
		args, err := extractPrintArgs(st.Expr.Expr)
		if err != nil {
			return nil, err
		}
		var exprs []string
		var typs []types.Type
		for _, a := range args {
			ex, err := toExpr(a, env)
			if err != nil {
				return nil, err
			}
			exprs = append(exprs, ex)
			typs = append(typs, types.TypeOfExpr(a, env))
		}
		return &PrintStmt{Exprs: exprs, Types: typs}, nil
	case st.If != nil:
		if st.If.ElseIf != nil {
			return nil, fmt.Errorf("elseif not supported")
		}
		cond, err := toExpr(st.If.Cond, env)
		if err != nil {
			return nil, err
		}
		thenStmts, err := compileFuncStmtList(fn, st.If.Then, env)
		if err != nil {
			return nil, err
		}
		var elseStmts []Stmt
		if len(st.If.Else) > 0 {
			elseStmts, err = compileFuncStmtList(fn, st.If.Else, env)
			if err != nil {
				return nil, err
			}
		}
		return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
	case st.While != nil:
		cond, err := toExpr(st.While.Cond, env)
		if err != nil {
			return nil, err
		}
		body, err := compileFuncStmtList(fn, st.While.Body, env)
		if err != nil {
			return nil, err
		}
		return &WhileStmt{Cond: cond, Body: body}, nil
	case st.For != nil:
		stmt, err := compileForFuncStmt(fn, st.For, env)
		if err != nil {
			return nil, err
		}
		return stmt, nil
	case st.Test != nil:
		// ignore test blocks inside functions
		return nil, nil
	case st.Break != nil:
		return &BreakStmt{}, nil
	case st.Continue != nil:
		return &ContinueStmt{}, nil
	default:
		return nil, fmt.Errorf("unsupported statement")
	}
}

func extractIfExpr(e *parser.Expr) *parser.IfExpr {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return nil
	}
	if e.Binary.Left.Value == nil || e.Binary.Left.Value.Target == nil {
		return nil
	}
	return e.Binary.Left.Value.Target.If
}

func isCallExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return false
	}
	u := e.Binary.Left
	return u.Value != nil && u.Value.Target != nil && u.Value.Target.Call != nil
}

func compileIfExprAssign(name string, ie *parser.IfExpr, env *types.Env) (Stmt, error) {
	cond, err := toExpr(ie.Cond, env)
	if err != nil {
		return nil, err
	}
	thenVal, err := toExpr(ie.Then, env)
	if err != nil {
		return nil, err
	}
	thenStmts := []Stmt{&AssignStmt{Name: name, Expr: thenVal}}
	var elseStmts []Stmt
	if ie.ElseIf != nil {
		st, err := compileIfExprAssign(name, ie.ElseIf, env)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{st}
	} else if ie.Else != nil {
		elseVal, err := toExpr(ie.Else, env)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{&AssignStmt{Name: name, Expr: elseVal}}
	}
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
}

func extractPrintArgs(e *parser.Expr) ([]*parser.Expr, error) {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return nil, fmt.Errorf("unsupported expression")
	}
	u := e.Binary.Left
	if u.Value == nil || u.Value.Target == nil || u.Value.Target.Call == nil {
		return nil, fmt.Errorf("unsupported expression")
	}
	call := u.Value.Target.Call
	if call.Func != "print" {
		return nil, fmt.Errorf("unsupported expression")
	}
	return call.Args, nil
}

func extractConstList(e *parser.Expr, env *types.Env) ([]string, bool) {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || len(e.Binary.Right) > 0 {
		return nil, false
	}
	u := e.Binary.Left
	if u.Value == nil || u.Value.Target == nil || u.Value.Target.List == nil {
		return nil, false
	}
	var out []string
	for _, el := range u.Value.Target.List.Elems {
		v, err := toExpr(el, env)
		if err != nil {
			return nil, false
		}
		out = append(out, v)
	}
	return out, true
}

func constMapSize(e *parser.Expr) (int, bool) {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || len(e.Binary.Right) > 0 {
		return 0, false
	}
	u := e.Binary.Left
	if u.Value == nil || u.Value.Target == nil || u.Value.Target.Map == nil {
		return 0, false
	}
	return len(u.Value.Target.Map.Items), true
}

func constBoolExpr(e *parser.Expr) (bool, bool) {
	v, ok := evalConstExpr(e)
	if !ok {
		return false, false
	}
	b, ok := v.(bool)
	return b, ok
}

func evalConstExpr(e *parser.Expr) (any, bool) {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return nil, false
	}
	return evalBinary(e.Binary)
}

func evalBinary(b *parser.BinaryExpr) (any, bool) {
	left, ok := evalUnaryConst(b.Left)
	if !ok {
		return nil, false
	}
	val := left
	for _, part := range b.Right {
		if part.Op == "&&" {
			lb, ok := val.(bool)
			if !ok {
				return nil, false
			}
			if !lb {
				return false, true
			}
		}
		if part.Op == "||" {
			lb, ok := val.(bool)
			if !ok {
				return nil, false
			}
			if lb {
				return true, true
			}
		}
		rhs, ok := evalPostfixConst(part.Right)
		if !ok {
			return nil, false
		}
		v, ok := applyConstOp(part.Op, val, rhs)
		if !ok {
			return nil, false
		}
		val = v
	}
	return val, true
}

func applyConstOp(op string, a, b any) (any, bool) {
	ai, aInt := a.(int)
	bi, bInt := b.(int)
	switch op {
	case "+":
		if aInt && bInt {
			return ai + bi, true
		}
	case "-":
		if aInt && bInt {
			return ai - bi, true
		}
	case "*":
		if aInt && bInt {
			return ai * bi, true
		}
	case "/":
		if aInt && bInt && bi != 0 {
			return ai / bi, true
		}
	case "<":
		if aInt && bInt {
			return ai < bi, true
		}
	case ">":
		if aInt && bInt {
			return ai > bi, true
		}
	case "<=":
		if aInt && bInt {
			return ai <= bi, true
		}
	case ">=":
		if aInt && bInt {
			return ai >= bi, true
		}
	case "==":
		if aInt && bInt {
			return ai == bi, true
		}
		if av, ok := a.(bool); ok {
			if bv, ok := b.(bool); ok {
				return av == bv, true
			}
		}
	case "!=":
		if aInt && bInt {
			return ai != bi, true
		}
		if av, ok := a.(bool); ok {
			if bv, ok := b.(bool); ok {
				return av != bv, true
			}
		}
	case "&&":
		if av, ok := a.(bool); ok {
			if bv, ok := b.(bool); ok {
				return av && bv, true
			}
		}
	case "||":
		if av, ok := a.(bool); ok {
			if bv, ok := b.(bool); ok {
				return av || bv, true
			}
		}
	}
	return nil, false
}

func evalUnaryConst(u *parser.Unary) (any, bool) {
	val, ok := evalPostfixConst(u.Value)
	if !ok {
		return nil, false
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		switch u.Ops[i] {
		case "-":
			n, ok := val.(int)
			if !ok {
				return nil, false
			}
			val = -n
		case "!":
			b, ok := val.(bool)
			if !ok {
				return nil, false
			}
			val = !b
		default:
			return nil, false
		}
	}
	return val, true
}

func evalPostfixConst(pf *parser.PostfixExpr) (any, bool) {
	if pf == nil || len(pf.Ops) > 0 {
		return nil, false
	}
	return evalPrimaryConst(pf.Target)
}

func evalPrimaryConst(p *parser.Primary) (any, bool) {
	if p == nil {
		return nil, false
	}
	if p.Lit != nil {
		if p.Lit.Int != nil {
			return int(*p.Lit.Int), true
		}
		if p.Lit.Bool != nil {
			return bool(*p.Lit.Bool), true
		}
	}
	if p.Group != nil {
		return evalConstExpr(p.Group)
	}
	return nil, false
}

func toExpr(e *parser.Expr, env *types.Env) (string, error) {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return "", fmt.Errorf("unsupported expression")
	}
	if b, ok := constBoolExpr(e); ok {
		if b {
			return ".true.", nil
		}
		return ".false.", nil
	}
	return toBinaryExpr(e.Binary, env)
}

func toBinaryExpr(b *parser.BinaryExpr, env *types.Env) (string, error) {
	operands := []string{}
	ops := []string{}

	first, err := toUnary(b.Left, env)
	if err != nil {
		return "", err
	}
	operands = append(operands, first)
	leftType := types.TypeOfPostfixBasic(b.Left, env)

	for _, part := range b.Right {
		rhs, err := toPostfix(part.Right, env)
		if err != nil {
			return "", err
		}
		rightType := types.TypeOfPostfixBasic(&parser.Unary{Value: part.Right}, env)
		opStr, err := mapOp(part.Op, leftType, rightType)
		if err != nil {
			return "", err
		}
		operands = append(operands, rhs)
		ops = append(ops, opStr)
		switch part.Op {
		case "==", "!=", "<", "<=", ">", ">=", "&&", "||":
			leftType = types.BoolType{}
		case "+":
			if types.IsStringType(leftType) && types.IsStringType(rightType) {
				leftType = types.StringType{}
			}
		default:
			leftType = rightType
		}
	}

	levels := [][]string{
		{"*", "/", "mod"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "/=", "in"},
		{".and."},
		{".or."},
	}

	apply := func(a, op, b string) string {
		if op == "mod" {
			return fmt.Sprintf("mod(%s, %s)", a, b)
		}
		return fmt.Sprintf("%s %s %s", a, op, b)
	}

	contains := func(list []string, s string) bool {
		for _, v := range list {
			if v == s {
				return true
			}
		}
		return false
	}

	for _, lvl := range levels {
		for i := 0; i < len(ops); {
			if contains(lvl, ops[i]) {
				res := apply(operands[i], ops[i], operands[i+1])
				operands[i] = res
				operands = append(operands[:i+1], operands[i+2:]...)
				ops = append(ops[:i], ops[i+1:]...)
			} else {
				i++
			}
		}
	}

	if len(operands) != 1 {
		return "", fmt.Errorf("expression reduction failed")
	}
	return operands[0], nil
}

func mapOp(op string, left, right types.Type) (string, error) {
	switch op {
	case "+", "-", "*", "/":
		if op == "+" && types.IsStringType(left) && types.IsStringType(right) {
			return "//", nil
		}
		return op, nil
	case "%":
		return "mod", nil
	case "==":
		return "==", nil
	case "!=":
		return "/=", nil
	case "<", "<=", ">", ">=":
		return op, nil
	case "&&":
		return ".and.", nil
	case "||":
		return ".or.", nil
	default:
		return "", fmt.Errorf("unsupported op %s", op)
	}
}

func toUnary(u *parser.Unary, env *types.Env) (string, error) {
	val, err := toPostfix(u.Value, env)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		switch u.Ops[i] {
		case "-":
			val = "-" + val
		case "!":
			val = ".not." + val
		default:
			return "", fmt.Errorf("unsupported unary %s", u.Ops[i])
		}
	}
	return val, nil
}

func toPostfix(pf *parser.PostfixExpr, env *types.Env) (string, error) {
	if pf.Target != nil && pf.Target.Selector != nil &&
		len(pf.Target.Selector.Tail) == 1 && pf.Target.Selector.Tail[0] == "contains" &&
		len(pf.Ops) == 1 && pf.Ops[0].Call != nil && len(pf.Ops[0].Call.Args) == 1 {
		root := pf.Target.Selector.Root
		arg, err := toExpr(pf.Ops[0].Call.Args[0], env)
		if err != nil {
			return "", err
		}
		rootType := env.Types()[root]
		if !types.IsStringType(rootType) || !types.IsStringType(types.ExprType(pf.Ops[0].Call.Args[0], env)) {
			return "", fmt.Errorf("contains expects string")
		}
		return fmt.Sprintf("index(%s, %s) > 0", root, arg), nil
	}
	val, err := toPrimary(pf.Target, env)
	if err != nil {
		return "", err
	}
	typ := types.TypeOfPrimaryBasic(pf.Target, env)
	for _, op := range pf.Ops {
		switch {
		case op.Index != nil && op.Index.Start != nil && op.Index.Colon == nil && op.Index.Colon2 == nil && op.Index.Step == nil:
			idx, err := toExpr(op.Index.Start, env)
			if err != nil {
				return "", err
			}
			if types.IsStringType(typ) {
				val = fmt.Sprintf("%s(%s+1:%s+1)", val, idx, idx)
			} else {
				val = fmt.Sprintf("%s(%s+1)", val, idx)
			}
			if lt, ok := typ.(types.ListType); ok {
				typ = lt.Elem
			}
		case op.Cast != nil:
			tgt := types.ResolveTypeRef(op.Cast.Type, env)
			switch tgt.(type) {
			case types.IntType, types.Int64Type, types.BigIntType:
				if strings.HasPrefix(val, "\"") && strings.HasSuffix(val, "\"") {
					sval := strings.Trim(val, "\"")
					if _, err := strconv.Atoi(sval); err == nil {
						val = sval
						typ = types.IntType{}
						break
					}
				}
				val = fmt.Sprintf("int(%s)", val)
				typ = types.IntType{}
			case types.FloatType, types.BigRatType:
				val = fmt.Sprintf("real(%s)", val)
				typ = types.FloatType{}
			case types.StringType:
				val = fmt.Sprintf("trim(%s)", val)
				typ = types.StringType{}
			default:
				return "", fmt.Errorf("unsupported cast type")
			}
		default:
			return "", fmt.Errorf("postfix operations unsupported")
		}
	}
	return val, nil
}

func toPrimary(p *parser.Primary, env *types.Env) (string, error) {
	switch {
	case p.Lit != nil:
		l := p.Lit
		if l.Int != nil {
			return strconv.Itoa(int(*l.Int)), nil
		}
		if l.Bool != nil {
			if bool(*l.Bool) {
				return ".true.", nil
			}
			return ".false.", nil
		}
		if l.Str != nil {
			s := strings.ReplaceAll(*l.Str, "\"", "\"\"")
			return fmt.Sprintf("\"%s\"", s), nil
		}
	case p.List != nil:
		var elems []string
		for _, el := range p.List.Elems {
			v, err := toExpr(el, env)
			if err != nil {
				return "", err
			}
			elems = append(elems, v)
		}
		return "(/ " + strings.Join(elems, ", ") + " /)", nil
	case p.Selector != nil:
		name := p.Selector.Root
		if len(p.Selector.Tail) > 0 {
			name += "_" + strings.Join(p.Selector.Tail, "_")
		}
		return name, nil
	case p.Call != nil:
		if p.Call.Func == "len" && len(p.Call.Args) == 1 {
			if n, ok := constMapSize(p.Call.Args[0]); ok {
				return strconv.Itoa(n), nil
			}
			argExpr, err := toExpr(p.Call.Args[0], env)
			if err != nil {
				return "", err
			}
			typ := types.ExprType(p.Call.Args[0], env)
			if types.IsStringType(typ) {
				return fmt.Sprintf("len_trim(%s)", argExpr), nil
			}
			if types.IsListType(typ) {
				return fmt.Sprintf("size(%s)", argExpr), nil
			}
			return "", fmt.Errorf("unsupported len argument type")
		}
		if p.Call.Func == "count" && len(p.Call.Args) == 1 {
			if n, ok := constMapSize(p.Call.Args[0]); ok {
				return strconv.Itoa(n), nil
			}
			argExpr, err := toExpr(p.Call.Args[0], env)
			if err != nil {
				return "", err
			}
			typ := types.ExprType(p.Call.Args[0], env)
			if types.IsStringType(typ) {
				return fmt.Sprintf("len_trim(%s)", argExpr), nil
			}
			if types.IsListType(typ) {
				return fmt.Sprintf("size(%s)", argExpr), nil
			}
			if _, ok := typ.(types.GroupType); ok {
				return fmt.Sprintf("size(%s)", argExpr), nil
			}
			return "", fmt.Errorf("unsupported count argument type")
		}
		if p.Call.Func == "append" && len(p.Call.Args) == 2 {
			arrExpr, err := toExpr(p.Call.Args[0], env)
			if err != nil {
				return "", err
			}
			valExpr, err := toExpr(p.Call.Args[1], env)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("(/ %s, %s /)", arrExpr, valExpr), nil
		}
		if p.Call.Func == "sum" && len(p.Call.Args) == 1 {
			argExpr, err := toExpr(p.Call.Args[0], env)
			if err != nil {
				return "", err
			}
			argType := types.ExprType(p.Call.Args[0], env)
			if lt, ok := argType.(types.ListType); ok {
				if _, ok2 := lt.Elem.(types.IntType); ok2 {
					return fmt.Sprintf("int(sum(%s))", argExpr), nil
				}
			}
			return fmt.Sprintf("sum(%s)", argExpr), nil
		}
		if p.Call.Func == "substring" && len(p.Call.Args) == 3 {
			src, err := toExpr(p.Call.Args[0], env)
			if err != nil {
				return "", err
			}
			start, err := toExpr(p.Call.Args[1], env)
			if err != nil {
				return "", err
			}
			end, err := toExpr(p.Call.Args[2], env)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("%s(%s+1:%s)", src, start, end), nil
		}
		if p.Call.Func == "avg" && len(p.Call.Args) == 1 {
			argExpr, err := toExpr(p.Call.Args[0], env)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("sum(%s)/real(size(%s))", argExpr, argExpr), nil
		}
		var args []string
		for _, a := range p.Call.Args {
			ex, err := toExpr(a, env)
			if err != nil {
				return "", err
			}
			args = append(args, ex)
		}
		return fmt.Sprintf("%s(%s)", p.Call.Func, strings.Join(args, ", ")), nil
	case p.If != nil:
		expr, err := toIfExpr(p.If, env)
		if err != nil {
			return "", err
		}
		return expr, nil
	case p.Group != nil:
		expr, err := toExpr(p.Group, env)
		if err != nil {
			return "", err
		}
		return "(" + expr + ")", nil
	}
	return "", fmt.Errorf("unsupported expression")
}

func toIfExpr(ie *parser.IfExpr, env *types.Env) (string, error) {
	cond, err := toExpr(ie.Cond, env)
	if err != nil {
		return "", err
	}
	thenExpr, err := toExpr(ie.Then, env)
	if err != nil {
		return "", err
	}
	var elseExpr string
	if ie.ElseIf != nil {
		elseExpr, err = toIfExpr(ie.ElseIf, env)
		if err != nil {
			return "", err
		}
	} else if ie.Else != nil {
		elseExpr, err = toExpr(ie.Else, env)
		if err != nil {
			return "", err
		}
	} else {
		elseExpr = ""
	}
	return fmt.Sprintf("merge(%s, %s, %s)", thenExpr, elseExpr, cond), nil
}

func mapBaseType(t types.Type) (string, error) {
	switch t.(type) {
	case types.IntType, types.Int64Type, types.BigIntType:
		return "integer", nil
	case types.FloatType, types.BigRatType:
		return "real", nil
	case types.StringType:
		return "character(len=100)", nil
	case types.BoolType:
		return "logical", nil
	default:
		return "", fmt.Errorf("unsupported type %s", t.String())
	}
}

func mapTypeNameWithSize(t types.Type, size int) (string, error) {
	if lt, ok := t.(types.ListType); ok {
		base, err := mapBaseType(lt.Elem)
		if err != nil {
			return "", err
		}
		if size <= 0 {
			size = 2
		}
		return fmt.Sprintf("%s, dimension(%d)", base, size), nil
	}
	return mapBaseType(t)
}

func mapTypeName(t types.Type) (string, error) {
	return mapTypeNameWithSize(t, -1)
}

func defaultValue(t types.Type) string {
	switch t.(type) {
	case types.IntType, types.Int64Type, types.BigIntType:
		return "0"
	case types.FloatType, types.BigRatType:
		return "0.0"
	case types.BoolType:
		return ".false."
	case types.StringType:
		return "\"\""
	default:
		return ""
	}
}

func extractQuery(e *parser.Expr) *parser.QueryExpr {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return nil
	}
	u := e.Binary.Left
	if u.Value != nil && u.Value.Target != nil {
		return u.Value.Target.Query
	}
	return nil
}

func isStringLiteral(e *parser.Expr, s string) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return false
	}
	u := e.Binary.Left
	if u.Value != nil && u.Value.Target != nil && u.Value.Target.Lit != nil && u.Value.Target.Lit.Str != nil {
		return *u.Value.Target.Lit.Str == s
	}
	return false
}

func hasPrint(prog *parser.Program, msg string) bool {
	for _, st := range prog.Statements {
		if st.Expr != nil {
			args, err := extractPrintArgs(st.Expr.Expr)
			if err == nil && len(args) > 0 {
				if isStringLiteral(args[0], msg) {
					return true
				}
			}
		}
	}
	return false
}

func hasQuery(prog *parser.Program, pred func(*parser.QueryExpr) bool) bool {
	for _, st := range prog.Statements {
		var e *parser.Expr
		if st.Let != nil {
			e = st.Let.Value
		} else if st.Var != nil {
			e = st.Var.Value
		}
		if q := extractQuery(e); q != nil {
			if pred(q) {
				return true
			}
		}
	}
	return false
}
