// Package lower translates an aotir.Program into a ptree.PhpFile.
// Entry point: Lower(prog, colours) → *ptree.PhpFile.
//
// Phase 0 ships an empty `mochi_main()` no-op plus a trailing call.
// Phase 1 wires CallStmt for the four print runtime entries by emitting
// matching `mochi_print_*` PHP helpers inline (Phase 15 will switch
// these to a Composer-autoloaded \Mochi\Runtime\IO). Phase 2 lands
// scalars: literals, let/var, binary/unary ops, comparisons, str
// concat, str.contains, int() cast, if/else, while + for-range +
// break/continue.
package lower

import (
	"fmt"
	"path/filepath"
	"strings"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/php/colour"
	"mochi/transpiler3/php/ptree"
)

// runtimeFlags tracks which inline runtime helpers the lowered program
// needs so the emit pass only includes the ones that are actually used.
type runtimeFlags struct {
	printStr    bool
	printInt    bool
	printBool   bool
	printF64    bool
	strContains bool
	strCat      bool
	setMake     bool // mochi_set_make([1,2,1]) → [1=>true, 2=>true]
	setAdd      bool // mochi_set_add($s, 4) → $s with 4 added
}

type lowerer struct {
	runtime runtimeFlags
	prog    *aotir.Program
}

// Lower translates an aotir.Program into a ptree.PhpFile. The returned
// file represents a single .php source file (named "main.php" by the
// emit pass) containing the inline runtime helpers the lowered body
// needs, one `mochi_main()` function, and one trailing call site.
func Lower(prog *aotir.Program, _ colour.ColourMap) (*ptree.PhpFile, error) {
	if prog == nil {
		return nil, fmt.Errorf("php lower: nil program")
	}
	if prog.Main < 0 || prog.Main >= len(prog.Functions) {
		return nil, fmt.Errorf("php lower: invalid Main index %d", prog.Main)
	}
	l := &lowerer{prog: prog}

	// Emit one final readonly class per record declaration. Source
	// order matters for Phase 16 reproducibility, so we walk
	// prog.Records directly.
	var classDecls []ptree.Decl
	for _, r := range prog.Records {
		d, err := l.lowerRecord(r)
		if err != nil {
			return nil, err
		}
		classDecls = append(classDecls, d)
	}

	// Lower non-main user functions in source order so the emitted
	// file preserves declaration ordering across runs (Phase 16
	// reproducibility relies on this).
	var userDecls []ptree.Decl
	for i, fn := range prog.Functions {
		if i == prog.Main {
			continue
		}
		d, err := l.lowerFunction(fn)
		if err != nil {
			return nil, err
		}
		userDecls = append(userDecls, d)
	}

	mainFn := prog.Functions[prog.Main]
	body, err := l.lowerBlock(mainFn.Body)
	if err != nil {
		return nil, err
	}
	mainDecl := &ptree.FuncDecl{
		PhpDoc:     []string{"Generated Mochi entry point. Do not edit by hand."},
		Name:       "mochi_main",
		ReturnType: "void",
		Body:       body,
	}

	decls := l.runtimeDecls()
	decls = append(decls, classDecls...)
	decls = append(decls, userDecls...)
	decls = append(decls, mainDecl)

	file := &ptree.PhpFile{
		// Phase 0/1/2 keep a global-namespace file. Phase 15 will add
		// a real PSR-4 namespace when the Composer package lands.
		Namespace: "",
		Uses:      nil,
		Decls:     decls,
		TrailingExec: []ptree.Stmt{
			&ptree.ExprStmt{
				Expr: &ptree.CallExpr{
					Callee: &ptree.IdentExpr{Name: "mochi_main"},
				},
			},
		},
	}
	return file, nil
}

// runtimeDecls emits FuncDecl entries for each inline runtime helper
// the lowered body requested.
func (l *lowerer) runtimeDecls() []ptree.Decl {
	var out []ptree.Decl
	if l.runtime.printStr {
		out = append(out, &ptree.FuncDecl{
			PhpDoc:     []string{"Print a string followed by a newline (vm3 println contract)."},
			Name:       "mochi_print_str",
			Params:     []ptree.FuncParam{{TypeName: "string", Name: "value"}},
			ReturnType: "void",
			Body: []ptree.Stmt{
				&ptree.RawStmt{Text: `echo $value, "\n";`},
			},
		})
	}
	if l.runtime.printInt {
		out = append(out, &ptree.FuncDecl{
			PhpDoc:     []string{"Print a 64-bit signed integer followed by a newline."},
			Name:       "mochi_print_i64",
			Params:     []ptree.FuncParam{{TypeName: "int", Name: "value"}},
			ReturnType: "void",
			Body: []ptree.Stmt{
				&ptree.RawStmt{Text: `echo $value, "\n";`},
			},
		})
	}
	if l.runtime.printF64 {
		out = append(out, &ptree.FuncDecl{
			PhpDoc:     []string{"Print a 64-bit float followed by a newline (Go 'g' -1 64 format)."},
			Name:       "mochi_print_f64",
			Params:     []ptree.FuncParam{{TypeName: "float", Name: "value"}},
			ReturnType: "void",
			Body: []ptree.Stmt{
				&ptree.RawStmt{Text: `if (is_nan($value)) { echo "NaN\n"; return; }`},
				&ptree.RawStmt{Text: `if (is_infinite($value)) { echo $value < 0 ? "-Inf\n" : "+Inf\n"; return; }`},
				&ptree.RawStmt{Text: `if ((float) (int) $value === $value && abs($value) < 1.0e15) { echo (int) $value, "\n"; return; }`},
				&ptree.RawStmt{Text: `echo $value, "\n";`},
			},
		})
	}
	if l.runtime.printBool {
		out = append(out, &ptree.FuncDecl{
			PhpDoc:     []string{"Print a bool as the lowercase literal (vm3 contract), followed by a newline."},
			Name:       "mochi_print_bool",
			Params:     []ptree.FuncParam{{TypeName: "bool", Name: "value"}},
			ReturnType: "void",
			Body: []ptree.Stmt{
				&ptree.RawStmt{Text: `echo $value ? "true\n" : "false\n";`},
			},
		})
	}
	if l.runtime.strContains {
		out = append(out, &ptree.FuncDecl{
			PhpDoc:     []string{"Return true when $needle is a substring of $haystack (vm3 str.contains)."},
			Name:       "mochi_str_contains",
			Params:     []ptree.FuncParam{{TypeName: "string", Name: "haystack"}, {TypeName: "string", Name: "needle"}},
			ReturnType: "bool",
			Body: []ptree.Stmt{
				&ptree.RawStmt{Text: `return $needle === "" || str_contains($haystack, $needle);`},
			},
		})
	}
	if l.runtime.setMake {
		out = append(out, &ptree.FuncDecl{
			PhpDoc: []string{
				"Build a Mochi set from a list. Sets are PHP assoc arrays",
				"keyed by element with `true` values, preserving insertion",
				"order. Duplicates are dropped on first occurrence.",
			},
			Name:       "mochi_set_make",
			Params:     []ptree.FuncParam{{TypeName: "array", Name: "elems"}},
			ReturnType: "array",
			Body: []ptree.Stmt{
				&ptree.RawStmt{Text: `$out = [];`},
				&ptree.RawStmt{Text: `foreach ($elems as $e) { $out[$e] = true; }`},
				&ptree.RawStmt{Text: `return $out;`},
			},
		})
	}
	if l.runtime.setAdd {
		out = append(out, &ptree.FuncDecl{
			PhpDoc: []string{
				"Return a copy of $s with $e added. Mochi semantics are",
				"non-mutating; PHP's array copy-on-write makes this cheap.",
			},
			Name:       "mochi_set_add",
			Params:     []ptree.FuncParam{{TypeName: "array", Name: "s"}, {Name: "e"}},
			ReturnType: "array",
			Body: []ptree.Stmt{
				&ptree.RawStmt{Text: `$s[$e] = true;`},
				&ptree.RawStmt{Text: `return $s;`},
			},
		})
	}
	return out
}

// lowerBlock translates an aotir.Block to a list of PHP statements.
func (l *lowerer) lowerBlock(b *aotir.Block) ([]ptree.Stmt, error) {
	if b == nil {
		return nil, nil
	}
	var out []ptree.Stmt
	for _, st := range b.Statements {
		stmts, err := l.lowerStmt(st)
		if err != nil {
			return nil, err
		}
		out = append(out, stmts...)
	}
	return out, nil
}

// lowerStmt translates one aotir statement.
func (l *lowerer) lowerStmt(s aotir.Stmt) ([]ptree.Stmt, error) {
	switch v := s.(type) {
	case *aotir.CallStmt:
		return l.lowerCallStmt(v)
	case *aotir.LetStmt:
		return l.lowerLetStmt(v)
	case *aotir.AssignStmt:
		return l.lowerAssignStmt(v)
	case *aotir.IfStmt:
		return l.lowerIfStmt(v)
	case *aotir.WhileStmt:
		return l.lowerWhileStmt(v)
	case *aotir.ForRangeStmt:
		return l.lowerForRangeStmt(v)
	case *aotir.ForEachStmt:
		return l.lowerForEachStmt(v)
	case *aotir.MapPutStmt:
		key, err := l.lowerExpr(v.Key)
		if err != nil {
			return nil, err
		}
		val, err := l.lowerExpr(v.Value)
		if err != nil {
			return nil, err
		}
		return []ptree.Stmt{&ptree.IndexAssignStmt{Name: v.Name, Key: key, Value: val}}, nil
	case *aotir.BreakStmt:
		return []ptree.Stmt{&ptree.BreakStmt{}}, nil
	case *aotir.ContinueStmt:
		return []ptree.Stmt{&ptree.ContinueStmt{}}, nil
	case *aotir.ReturnStmt:
		if v.Value == nil {
			return []ptree.Stmt{&ptree.ReturnStmt{}}, nil
		}
		e, err := l.lowerExpr(v.Value)
		if err != nil {
			return nil, err
		}
		return []ptree.Stmt{&ptree.ReturnStmt{Value: e}}, nil
	default:
		return nil, fmt.Errorf("php lower: phase 2 cannot lower %T", s)
	}
}

func (l *lowerer) lowerCallStmt(s *aotir.CallStmt) ([]ptree.Stmt, error) {
	switch s.Func {
	case "mochi_print_str":
		l.runtime.printStr = true
	case "mochi_print_i64":
		l.runtime.printInt = true
	case "mochi_print_f64":
		l.runtime.printF64 = true
	case "mochi_print_bool":
		l.runtime.printBool = true
	default:
		return nil, fmt.Errorf("php lower: unsupported builtin call %q", s.Func)
	}
	if len(s.Args) != 1 {
		return nil, fmt.Errorf("php lower: %s wants 1 arg, got %d", s.Func, len(s.Args))
	}
	arg, err := l.lowerExpr(s.Args[0])
	if err != nil {
		return nil, err
	}
	return []ptree.Stmt{
		&ptree.ExprStmt{Expr: &ptree.CallExpr{Callee: &ptree.IdentExpr{Name: s.Func}, Args: []ptree.Expr{arg}}},
	}, nil
}

func (l *lowerer) lowerLetStmt(s *aotir.LetStmt) ([]ptree.Stmt, error) {
	if s.Init == nil {
		// Uninitialised binding: emit a null seed so PHP doesn't pick
		// up a notice on first read. Mochi requires initialisation
		// in source so this branch is defensive.
		return []ptree.Stmt{&ptree.AssignStmt{Name: s.Name, Value: &ptree.NullLit{}}}, nil
	}
	init, err := l.lowerExpr(s.Init)
	if err != nil {
		return nil, err
	}
	return []ptree.Stmt{&ptree.AssignStmt{Name: s.Name, Value: init}}, nil
}

func (l *lowerer) lowerAssignStmt(s *aotir.AssignStmt) ([]ptree.Stmt, error) {
	v, err := l.lowerExpr(s.Value)
	if err != nil {
		return nil, err
	}
	return []ptree.Stmt{&ptree.AssignStmt{Name: s.Name, Value: v}}, nil
}

func (l *lowerer) lowerIfStmt(s *aotir.IfStmt) ([]ptree.Stmt, error) {
	cond, err := l.lowerExpr(s.Cond)
	if err != nil {
		return nil, err
	}
	thenBody, err := l.lowerBlock(s.Then)
	if err != nil {
		return nil, err
	}
	var elseBody []ptree.Stmt
	if s.Else != nil {
		elseBody, err = l.lowerBlock(s.Else)
		if err != nil {
			return nil, err
		}
	}
	return []ptree.Stmt{&ptree.IfStmt{Cond: cond, Then: thenBody, Else: elseBody}}, nil
}

func (l *lowerer) lowerWhileStmt(s *aotir.WhileStmt) ([]ptree.Stmt, error) {
	cond, err := l.lowerExpr(s.Cond)
	if err != nil {
		return nil, err
	}
	body, err := l.lowerBlock(s.Body)
	if err != nil {
		return nil, err
	}
	return []ptree.Stmt{&ptree.WhileStmt{Cond: cond, Body: body}}, nil
}

func (l *lowerer) lowerForRangeStmt(s *aotir.ForRangeStmt) ([]ptree.Stmt, error) {
	start, err := l.lowerExpr(s.Start)
	if err != nil {
		return nil, err
	}
	end, err := l.lowerExpr(s.End)
	if err != nil {
		return nil, err
	}
	body, err := l.lowerBlock(s.Body)
	if err != nil {
		return nil, err
	}
	return []ptree.Stmt{&ptree.ForRangeStmt{Var: s.Var, Start: start, End: end, Body: body}}, nil
}

func (l *lowerer) lowerForEachStmt(s *aotir.ForEachStmt) ([]ptree.Stmt, error) {
	src, err := l.lowerExpr(s.List)
	if err != nil {
		return nil, err
	}
	body, err := l.lowerBlock(s.Body)
	if err != nil {
		return nil, err
	}
	return []ptree.Stmt{&ptree.ForEachStmt{Var: s.Var, Source: src, Body: body}}, nil
}

// lowerFunction translates one non-main aotir Function to a PHP FuncDecl.
// Phase 2 only sees scalar parameter and return types; later phases extend
// phpType to cover records, lists, maps, sums, and closures.
func (l *lowerer) lowerFunction(fn *aotir.Function) (*ptree.FuncDecl, error) {
	body, err := l.lowerBlock(fn.Body)
	if err != nil {
		return nil, fmt.Errorf("php lower: in function %q: %w", fn.Name, err)
	}
	params := make([]ptree.FuncParam, 0, len(fn.Params))
	for _, p := range fn.Params {
		typeName, err := phpParamType(p.Type, p.RecordName)
		if err != nil {
			return nil, fmt.Errorf("php lower: param %q of %q: %w", p.Name, fn.Name, err)
		}
		params = append(params, ptree.FuncParam{TypeName: typeName, Name: p.Name})
	}
	ret, err := phpParamType(fn.ReturnType, fn.ReturnRecordName)
	if err != nil {
		return nil, fmt.Errorf("php lower: return type of %q: %w", fn.Name, err)
	}
	return &ptree.FuncDecl{
		Name:       fn.Name,
		Params:     params,
		ReturnType: ret,
		Body:       body,
	}, nil
}

// phpScalarType is the subset of phpType used where a record name is
// unavailable. Callers that may see TypeRecord/TypeList/TypeMap should
// use phpParamType instead.
func phpScalarType(t aotir.Type) (string, error) {
	switch t {
	case aotir.TypeInt:
		return "int", nil
	case aotir.TypeFloat:
		return "float", nil
	case aotir.TypeString:
		return "string", nil
	case aotir.TypeBool:
		return "bool", nil
	case aotir.TypeUnit:
		return "void", nil
	default:
		return "", fmt.Errorf("cannot map aotir scalar type %v to PHP", t)
	}
}

// phpType is the legacy name; keep for callsites that handle only
// scalars + unit. New callers should prefer phpParamType.
func phpType(t aotir.Type) (string, error) { return phpScalarType(t) }

// phpParamType maps a parameter type to its PHP type declaration,
// including record class names and collection types (Phase 3+).
func phpParamType(t aotir.Type, recordName string) (string, error) {
	switch t {
	case aotir.TypeRecord:
		if recordName == "" {
			return "", fmt.Errorf("TypeRecord needs a RecordName")
		}
		return recordName, nil
	case aotir.TypeList, aotir.TypeMap, aotir.TypeSet:
		return "array", nil
	default:
		return phpScalarType(t)
	}
}

func (l *lowerer) lowerRecord(r *aotir.RecordDecl) (*ptree.ClassDecl, error) {
	fields := make([]ptree.ClassField, 0, len(r.Fields))
	for _, f := range r.Fields {
		typeName, err := phpParamType(f.Type, f.RecordName)
		if err != nil {
			return nil, fmt.Errorf("record %q field %q: %w", r.Name, f.Name, err)
		}
		fields = append(fields, ptree.ClassField{TypeName: typeName, Name: f.Name})
	}
	return &ptree.ClassDecl{
		Name:   r.Name,
		Fields: fields,
		PhpDoc: []string{"Mochi record `" + r.Name + "`. Generated; do not edit by hand."},
	}, nil
}

// lowerExpr translates one aotir expression.
func (l *lowerer) lowerExpr(e aotir.Expr) (ptree.Expr, error) {
	switch v := e.(type) {
	case *aotir.StringLit:
		return &ptree.StringLit{Value: v.Value}, nil
	case *aotir.IntLit:
		return &ptree.IntLit{Value: v.Value}, nil
	case *aotir.FloatLit:
		return &ptree.FloatLit{Value: v.Value}, nil
	case *aotir.BoolLit:
		return &ptree.BoolLit{Value: v.Value}, nil
	case *aotir.VarRef:
		return &ptree.VarExpr{Name: v.Name}, nil
	case *aotir.BinaryExpr:
		return l.lowerBinaryExpr(v)
	case *aotir.UnaryExpr:
		return l.lowerUnaryExpr(v)
	case *aotir.NumCastExpr:
		op, err := l.lowerExpr(v.Operand)
		if err != nil {
			return nil, err
		}
		return &ptree.CastExpr{TargetType: "int", Operand: op}, nil
	case *aotir.StrContainsExpr:
		l.runtime.strContains = true
		hay, err := l.lowerExpr(v.Receiver)
		if err != nil {
			return nil, err
		}
		needle, err := l.lowerExpr(v.Sub)
		if err != nil {
			return nil, err
		}
		return &ptree.CallExpr{
			Callee: &ptree.IdentExpr{Name: "mochi_str_contains"},
			Args:   []ptree.Expr{hay, needle},
		}, nil
	case *aotir.StrLenExpr:
		// Phase 2 strings are UTF-8 byte strings; vm3's len(str) reports
		// byte length, so PHP's strlen() is the correct primitive (and
		// matches the swift transpiler's String.utf8.count behaviour).
		recv, err := l.lowerExpr(v.Receiver)
		if err != nil {
			return nil, err
		}
		return &ptree.CallExpr{
			Callee: &ptree.IdentExpr{Name: "strlen"},
			Args:   []ptree.Expr{recv},
		}, nil
	case *aotir.StrIndexExpr:
		// s[i] returns the i'th byte as a one-character string; PHP's
		// substr($s, $i, 1) matches vm3's byte-indexed semantics.
		recv, err := l.lowerExpr(v.Receiver)
		if err != nil {
			return nil, err
		}
		idx, err := l.lowerExpr(v.Index)
		if err != nil {
			return nil, err
		}
		return &ptree.CallExpr{
			Callee: &ptree.IdentExpr{Name: "substr"},
			Args:   []ptree.Expr{recv, idx, &ptree.IntLit{Value: 1}},
		}, nil
	case *aotir.CallExpr:
		args := make([]ptree.Expr, 0, len(v.Args))
		for _, a := range v.Args {
			lo, err := l.lowerExpr(a)
			if err != nil {
				return nil, err
			}
			args = append(args, lo)
		}
		return &ptree.CallExpr{
			Callee: &ptree.IdentExpr{Name: v.Func},
			Args:   args,
		}, nil
	case *aotir.ListLit:
		elems := make([]ptree.Expr, 0, len(v.Elems))
		for _, el := range v.Elems {
			lo, err := l.lowerExpr(el)
			if err != nil {
				return nil, err
			}
			elems = append(elems, lo)
		}
		return &ptree.ArrayLit{Elems: elems}, nil
	case *aotir.AppendExpr:
		recv, err := l.lowerExpr(v.Receiver)
		if err != nil {
			return nil, err
		}
		val, err := l.lowerExpr(v.Value)
		if err != nil {
			return nil, err
		}
		return &ptree.ArrayAppendExpr{Inner: recv, Tail: val}, nil
	case *aotir.IndexExpr:
		recv, err := l.lowerExpr(v.Receiver)
		if err != nil {
			return nil, err
		}
		idx, err := l.lowerExpr(v.Index)
		if err != nil {
			return nil, err
		}
		return &ptree.IndexExpr{Receiver: recv, Index: idx}, nil
	case *aotir.LenExpr:
		recv, err := l.lowerExpr(v.Receiver)
		if err != nil {
			return nil, err
		}
		return &ptree.CallExpr{
			Callee: &ptree.IdentExpr{Name: "count"},
			Args:   []ptree.Expr{recv},
		}, nil
	case *aotir.MapLit:
		keys := make([]ptree.Expr, 0, len(v.Keys))
		vals := make([]ptree.Expr, 0, len(v.Values))
		for i := range v.Keys {
			k, err := l.lowerExpr(v.Keys[i])
			if err != nil {
				return nil, err
			}
			vv, err := l.lowerExpr(v.Values[i])
			if err != nil {
				return nil, err
			}
			keys = append(keys, k)
			vals = append(vals, vv)
		}
		return &ptree.ArrayLit{Keys: keys, Values: vals}, nil
	case *aotir.MapGetExpr:
		recv, err := l.lowerExpr(v.Receiver)
		if err != nil {
			return nil, err
		}
		key, err := l.lowerExpr(v.Key)
		if err != nil {
			return nil, err
		}
		return &ptree.IndexExpr{Receiver: recv, Index: key}, nil
	case *aotir.MapHasExpr:
		recv, err := l.lowerExpr(v.Receiver)
		if err != nil {
			return nil, err
		}
		key, err := l.lowerExpr(v.Key)
		if err != nil {
			return nil, err
		}
		return &ptree.CallExpr{
			Callee: &ptree.IdentExpr{Name: "array_key_exists"},
			Args:   []ptree.Expr{key, recv},
		}, nil
	case *aotir.MapLenExpr:
		recv, err := l.lowerExpr(v.Receiver)
		if err != nil {
			return nil, err
		}
		return &ptree.CallExpr{
			Callee: &ptree.IdentExpr{Name: "count"},
			Args:   []ptree.Expr{recv},
		}, nil
	case *aotir.MapKeysExpr:
		recv, err := l.lowerExpr(v.Receiver)
		if err != nil {
			return nil, err
		}
		return &ptree.CallExpr{
			Callee: &ptree.IdentExpr{Name: "array_keys"},
			Args:   []ptree.Expr{recv},
		}, nil
	case *aotir.SetLiteralExpr:
		l.runtime.setMake = true
		elems := make([]ptree.Expr, 0, len(v.Elems))
		for _, el := range v.Elems {
			lo, err := l.lowerExpr(el)
			if err != nil {
				return nil, err
			}
			elems = append(elems, lo)
		}
		return &ptree.CallExpr{
			Callee: &ptree.IdentExpr{Name: "mochi_set_make"},
			Args:   []ptree.Expr{&ptree.ArrayLit{Elems: elems}},
		}, nil
	case *aotir.SetAddExpr:
		l.runtime.setAdd = true
		recv, err := l.lowerExpr(v.Receiver)
		if err != nil {
			return nil, err
		}
		el, err := l.lowerExpr(v.Elem)
		if err != nil {
			return nil, err
		}
		return &ptree.CallExpr{
			Callee: &ptree.IdentExpr{Name: "mochi_set_add"},
			Args:   []ptree.Expr{recv, el},
		}, nil
	case *aotir.SetHasExpr:
		recv, err := l.lowerExpr(v.Receiver)
		if err != nil {
			return nil, err
		}
		el, err := l.lowerExpr(v.Elem)
		if err != nil {
			return nil, err
		}
		return &ptree.CallExpr{
			Callee: &ptree.IdentExpr{Name: "array_key_exists"},
			Args:   []ptree.Expr{el, recv},
		}, nil
	case *aotir.SetLenExpr:
		recv, err := l.lowerExpr(v.Receiver)
		if err != nil {
			return nil, err
		}
		return &ptree.CallExpr{
			Callee: &ptree.IdentExpr{Name: "count"},
			Args:   []ptree.Expr{recv},
		}, nil
	case *aotir.RecordLit:
		args := make([]ptree.NamedArg, 0, len(v.Fields))
		for _, f := range v.Fields {
			val, err := l.lowerExpr(f.Value)
			if err != nil {
				return nil, err
			}
			args = append(args, ptree.NamedArg{Name: f.Name, Value: val})
		}
		return &ptree.NewExpr{Class: v.TypeName, Args: args}, nil
	case *aotir.FieldAccess:
		recv, err := l.lowerExpr(v.Receiver)
		if err != nil {
			return nil, err
		}
		return &ptree.PropAccessExpr{Receiver: recv, Field: v.FieldName}, nil
	default:
		return nil, fmt.Errorf("php lower: phase 4 cannot lower %T", e)
	}
}

func (l *lowerer) lowerBinaryExpr(b *aotir.BinaryExpr) (ptree.Expr, error) {
	left, err := l.lowerExpr(b.Left)
	if err != nil {
		return nil, err
	}
	right, err := l.lowerExpr(b.Right)
	if err != nil {
		return nil, err
	}
	switch b.Op {
	case aotir.BinAddI64, aotir.BinAddF64:
		return &ptree.BinaryExpr{Op: "+", Left: left, Right: right}, nil
	case aotir.BinSubI64, aotir.BinSubF64:
		return &ptree.BinaryExpr{Op: "-", Left: left, Right: right}, nil
	case aotir.BinMulI64, aotir.BinMulF64:
		return &ptree.BinaryExpr{Op: "*", Left: left, Right: right}, nil
	case aotir.BinDivI64:
		// PHP `/` between two ints yields a float when the division
		// is not exact. Mochi int/int is truncating, so use intdiv.
		return &ptree.BinaryExpr{Op: "intdiv", IsCall: true, Left: left, Right: right}, nil
	case aotir.BinDivF64:
		// PHP 8 throws DivisionByZeroError on `/` when the divisor is
		// 0.0; fdiv() returns IEEE 754 +Inf/-Inf/NaN, which is what
		// Mochi expects (see the float_nan_inf fixture).
		return &ptree.BinaryExpr{Op: "fdiv", IsCall: true, Left: left, Right: right}, nil
	case aotir.BinModI64:
		return &ptree.BinaryExpr{Op: "%", Left: left, Right: right}, nil
	case aotir.BinEqI64, aotir.BinEqF64, aotir.BinEqBool, aotir.BinEqStr:
		return &ptree.BinaryExpr{Op: "===", Left: left, Right: right}, nil
	case aotir.BinNeI64, aotir.BinNeF64, aotir.BinNeBool, aotir.BinNeStr:
		return &ptree.BinaryExpr{Op: "!==", Left: left, Right: right}, nil
	case aotir.BinEqRec, aotir.BinEqList, aotir.BinEqMap:
		// PHP `==` compares same-class objects field-by-field and
		// indexed/assoc arrays element-by-element, matching Mochi's
		// structural value-equality semantics. `===` would compare
		// object identity / array reference instead.
		return &ptree.BinaryExpr{Op: "==", Left: left, Right: right}, nil
	case aotir.BinNeRec, aotir.BinNeList, aotir.BinNeMap:
		return &ptree.BinaryExpr{Op: "!=", Left: left, Right: right}, nil
	case aotir.BinLtI64, aotir.BinLtF64:
		return &ptree.BinaryExpr{Op: "<", Left: left, Right: right}, nil
	case aotir.BinLeI64, aotir.BinLeF64:
		return &ptree.BinaryExpr{Op: "<=", Left: left, Right: right}, nil
	case aotir.BinGtI64, aotir.BinGtF64:
		return &ptree.BinaryExpr{Op: ">", Left: left, Right: right}, nil
	case aotir.BinGeI64, aotir.BinGeF64:
		return &ptree.BinaryExpr{Op: ">=", Left: left, Right: right}, nil
	case aotir.BinAndBool:
		return &ptree.BinaryExpr{Op: "&&", Left: left, Right: right}, nil
	case aotir.BinOrBool:
		return &ptree.BinaryExpr{Op: "||", Left: left, Right: right}, nil
	case aotir.BinStrCat:
		return &ptree.BinaryExpr{Op: ".", Left: left, Right: right}, nil
	default:
		return nil, fmt.Errorf("php lower: unsupported BinOp %v", b.Op)
	}
}

func (l *lowerer) lowerUnaryExpr(u *aotir.UnaryExpr) (ptree.Expr, error) {
	op, err := l.lowerExpr(u.Operand)
	if err != nil {
		return nil, err
	}
	switch u.Op {
	case aotir.UnNegI64, aotir.UnNegF64:
		return &ptree.UnaryExpr{Op: "-", Operand: op}, nil
	case aotir.UnNotBool:
		return &ptree.UnaryExpr{Op: "!", Operand: op}, nil
	default:
		return nil, fmt.Errorf("php lower: unsupported UnOp %v", u.Op)
	}
}

// ModuleName converts a Mochi source filename to a PSR-4 module name.
// Phase 0/1/2 return the base name without the .mochi suffix; Phase
// 15 will mangle this further when the Composer namespace is wired.
//
//	"hello.mochi"       -> "Hello"
//	"hello_world.mochi" -> "HelloWorld"
func ModuleName(src string) string {
	src = filepath.Base(src)
	src = strings.TrimSuffix(src, ".mochi")
	parts := strings.FieldsFunc(src, func(r rune) bool {
		return r == '_' || r == '-'
	})
	var sb strings.Builder
	for _, p := range parts {
		if len(p) == 0 {
			continue
		}
		sb.WriteString(strings.ToUpper(p[:1]))
		if len(p) > 1 {
			sb.WriteString(p[1:])
		}
	}
	if sb.Len() == 0 {
		return "Main"
	}
	return sb.String()
}
