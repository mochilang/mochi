//go:build slow

package dartt

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"unicode"

	yaml "gopkg.in/yaml.v3"

	"mochi/ast"
	"mochi/parser"
	testpkg "mochi/runtime/ffi/go/testpkg"
	"mochi/types"
)

const helperSHA256 = `
List<int> _sha256(dynamic data) {
  List<int> bs;
  if (data is String) {
    bs = utf8.encode(data);
  } else if (data is List<int>) {
    bs = data;
  } else {
    bs = utf8.encode(data.toString());
  }
  final tmp = File('${Directory.systemTemp.path}/sha256_${DateTime.now().microsecondsSinceEpoch}.bin');
  tmp.writeAsBytesSync(bs.map((e) => e & 0xff).toList());
  final result = Process.runSync('sha256sum', [tmp.path]);
  tmp.deleteSync();
  var hex = '';
  if (result.stdout is String) {
    hex = (result.stdout as String).split(' ')[0];
  }
  final out = <int>[];
  for (var i = 0; i < hex.length; i += 2) {
    out.add(int.parse(hex.substring(i, i + 2), radix: 16));
  }
  return out;
}
`

const helperMD5 = `
String _md5hex(String s) {
  final tmp = File('${Directory.systemTemp.path}/md5_${DateTime.now().microsecondsSinceEpoch}.txt');
  tmp.writeAsStringSync(s);
  final result = Process.runSync('md5sum', [tmp.path]);
  tmp.deleteSync();
  if (result.stdout is String) {
    return (result.stdout as String).split(' ')[0];
  }
  return '';
}
`

const helperFetch = `
dynamic _fetch(String url) {
  if (url.startsWith('file://')) {
    final path = url.substring(7);
    final text = File(path).readAsStringSync();
    try {
      return jsonDecode(text);
    } catch (_) {
      return text;
    }
  }
  final result = Process.runSync('curl', ['-s', url]);
  if (result.exitCode != 0) {
    throw Exception('curl failed');
  }
  var text = '';
  if (result.stdout is String) {
    text = result.stdout as String;
  }
  try {
    return jsonDecode(text);
  } catch (_) {
    return text;
  }
}
`

const helperJSONPrint = `
void _json(dynamic v) {
  print(jsonEncode(v));
}
`

const helperError = `
Never _error(String msg) {
  throw Exception(msg);
}
`

// --- Struct tracking for generated classes ---
type StructField struct {
	Name string
	Type string
}

var (
	structSeq         int
	structSig         map[string]string
	structFields      map[string][]StructField
	mapLitStructName  map[*MapLit]string
	structOrder       []string
	compVarTypes      map[string]string
	localVarTypes     map[string]string
	funcReturnTypes   map[string]string
	structNameCount   map[string]int
	nextStructHint    string
	usesJSON          bool
	benchMain         bool
	renameMain        bool
	useNow            bool
	useInput          bool
	useLookupHost     bool
	useBigRat         bool
	useSHA256         bool
	useMD5            bool
	useFetch          bool
	useError          bool
	useJSONPrint      bool
	useEnv            bool
	useSubprocess     bool
	useSubstrClamp    bool
	useRepeat         bool
	useStr            bool
	useParseIntStr    bool
	imports           []string
	testpkgAliases    map[string]struct{}
	netAliases        map[string]struct{}
	osAliases         map[string]struct{}
	subprocessAliases map[string]struct{}
	structMutable     map[string]bool
	currentRetType    string
	inFunc            bool
	convInFunc        bool
	methodDefs        []Stmt
	structMethods     map[string]map[string]bool
	targetMapValType  string
)

// GetStructOrder returns the generated struct names (for testing).
func GetStructOrder() []string { return structOrder }

// GetStructFields exposes struct field definitions (for testing).
func GetStructFields() map[string][]StructField { return structFields }

// DebugLocalVarTypes exposes local variable type map (for testing).
func DebugLocalVarTypes() map[string]string { return localVarTypes }

// DebugFuncReturnTypes exposes function return type map (for testing).
func DebugFuncReturnTypes() map[string]string { return funcReturnTypes }

// DebugStructMethods exposes struct method map (for testing).
func DebugStructMethods() map[string]map[string]bool { return structMethods }

func capitalize(name string) string {
	parts := strings.Split(name, "_")
	for i, p := range parts {
		if len(p) == 0 {
			continue
		}
		parts[i] = strings.ToUpper(p[:1]) + p[1:]
	}
	return strings.Join(parts, "")
}

var dartKeywords = map[string]struct{}{
	// Core language keywords
	"abstract": {}, "else": {}, "enum": {}, "false": {}, "final": {},
	"for": {}, "if": {}, "in": {}, "new": {}, "null": {}, "super": {},
	"switch": {}, "case": {}, "var": {}, "void": {}, "while": {},
	"return": {}, "this": {}, "true": {}, "is": {}, "with": {},
	"num": {},
	// Additional reserved words
	"assert": {}, "async": {}, "await": {}, "break": {}, "catch": {},
	"class": {}, "const": {}, "continue": {}, "default": {}, "do": {},
	"dynamic": {}, "extends": {}, "factory": {}, "finally": {},
	"import": {}, "interface": {}, "library": {}, "mixin": {},
	"operator": {}, "part": {}, "rethrow": {}, "static": {},
	"typedef": {}, "yield": {}, "try": {}, "throw": {},
}

func sanitize(name string) string {
	if name == "_" {
		return "__"
	}
	if _, ok := dartKeywords[name]; ok {
		return "_" + name
	}
	return name
}

func validIdent(name string) bool {
	if name == "" {
		return false
	}
	for i, r := range name {
		if i == 0 {
			if !(unicode.IsLetter(r) || r == '_') {
				return false
			}
		} else {
			if !(unicode.IsLetter(r) || unicode.IsDigit(r) || r == '_') {
				return false
			}
		}
	}
	return true
}

func zeroValue(typ string) string {
	switch typ {
	case "int", "num", "double":
		return "0"
	case "BigInt":
		return "BigInt.zero"
	case "bool":
		return "false"
	case "String":
		return "\"\""
	default:
		if strings.HasPrefix(typ, "List<") {
			elem := strings.TrimSuffix(strings.TrimPrefix(typ, "List<"), ">")
			return "<" + elem + ">[]"
		}
		if strings.HasPrefix(typ, "Map<") {
			kv := strings.TrimSuffix(strings.TrimPrefix(typ, "Map<"), ">")
			return "<" + kv + ">{}"
		}
	}
	return "null"
}

func emitWithBigIntCast(w io.Writer, e Expr, from, to string) error {
	if to == "BigInt" && from == "int" {
		if lit, ok := e.(*IntLit); ok {
			_, err := fmt.Fprintf(w, "BigInt.from(%d)", lit.Value)
			return err
		}
		if _, err := io.WriteString(w, "BigInt.from("); err != nil {
			return err
		}
		if err := e.emit(w); err != nil {
			return err
		}
		_, err := io.WriteString(w, ")")
		return err
	}
	if to == "int" && from == "BigInt" {
		if _, err := io.WriteString(w, "("); err != nil {
			return err
		}
		if err := e.emit(w); err != nil {
			return err
		}
		_, err := io.WriteString(w, ").toInt()")
		return err
	}
	return e.emit(w)
}

// emitListConversion converts expr to the target list type recursively.
func emitListConversion(w io.Writer, expr Expr, target string) error {
	elem := strings.TrimSuffix(strings.TrimPrefix(target, "List<"), ">")
	exprType := inferType(expr)
	if exprType == target {
		if err := expr.emit(w); err != nil {
			return err
		}
		if strings.HasSuffix(exprType, "?") && !strings.HasSuffix(target, "?") {
			if _, err := io.WriteString(w, "!"); err != nil {
				return err
			}
		}
		return nil
	}
	if strings.HasPrefix(elem, "List<") {
		if _, err := io.WriteString(w, "("); err != nil {
			return err
		}
		if err := expr.emit(w); err != nil {
			return err
		}
		if strings.HasSuffix(exprType, "?") {
			if _, err := io.WriteString(w, "!"); err != nil {
				return err
			}
		}
		if _, err := io.WriteString(w, " as List).map((e) => ("); err != nil {
			return err
		}
		if err := emitListConversion(w, &Name{Name: "e"}, elem); err != nil {
			return err
		}
		if _, err := io.WriteString(w, " as "+elem+")).toList()"); err != nil {
			return err
		}
		return nil
	}
	if elem == "int" {
		if _, err := io.WriteString(w, "("); err != nil {
			return err
		}
		if err := expr.emit(w); err != nil {
			return err
		}
		if strings.HasSuffix(exprType, "?") {
			if _, err := io.WriteString(w, "!"); err != nil {
				return err
			}
		}
		if _, err := io.WriteString(w, " as List).map((e) => (e is BigInt ? e.toInt() : (e as int))).toList()"); err != nil {
			return err
		}
		return nil
	}
	if _, err := io.WriteString(w, "List<"+elem+">.from("); err != nil {
		return err
	}
	if err := expr.emit(w); err != nil {
		return err
	}
	if strings.HasSuffix(exprType, "?") {
		if _, err := io.WriteString(w, "!"); err != nil {
			return err
		}
	}
	_, err := io.WriteString(w, ")")
	return err
}

// --- Simple Dart AST ---

// Program represents a sequence of statements.
type Program struct {
	Imports   []string
	Stmts     []Stmt
	BenchMain bool
	WrapMain  bool
}

type Stmt interface{ emit(io.Writer) error }

// WhileStmt represents a simple while loop.
type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

func (w *WhileStmt) emit(out io.Writer) error {
	if _, err := io.WriteString(out, "while ("); err != nil {
		return err
	}
	if err := w.Cond.emit(out); err != nil {
		return err
	}
	if _, err := io.WriteString(out, ") {\n"); err != nil {
		return err
	}
	for _, st := range w.Body {
		if _, err := io.WriteString(out, "    "); err != nil {
			return err
		}
		if err := st.emit(out); err != nil {
			return err
		}
		if _, ok := st.(*IfStmt); ok {
			if _, err := io.WriteString(out, "\n"); err != nil {
				return err
			}
		} else if _, ok := st.(*WhileStmt); ok {
			if _, err := io.WriteString(out, "\n"); err != nil {
				return err
			}
		} else if _, ok := st.(*ForRangeStmt); ok {
			if _, err := io.WriteString(out, "\n"); err != nil {
				return err
			}
		} else if _, ok := st.(*ForInStmt); ok {
			if _, err := io.WriteString(out, "\n"); err != nil {
				return err
			}
		} else {
			if _, err := io.WriteString(out, ";\n"); err != nil {
				return err
			}
		}
	}
	if _, err := io.WriteString(out, "  }"); err != nil {
		return err
	}
	return nil
}

// ForRangeStmt represents a numeric for-loop like `for i in 0..10 {}`.
type ForRangeStmt struct {
	Name  string
	Start Expr
	End   Expr
	Body  []Stmt
}

func (f *ForRangeStmt) emit(out io.Writer) error {
	if _, err := io.WriteString(out, "for (int "+f.Name+" = "); err != nil {
		return err
	}
	if f.Start != nil {
		if err := f.Start.emit(out); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(out, "; "+f.Name+" < "); err != nil {
		return err
	}
	if f.End != nil {
		if err := f.End.emit(out); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(out, "; "+f.Name+"++) {\n"); err != nil {
		return err
	}
	for _, st := range f.Body {
		if _, err := io.WriteString(out, "    "); err != nil {
			return err
		}
		if err := st.emit(out); err != nil {
			return err
		}
		switch st.(type) {
		case *IfStmt, *WhileStmt, *ForRangeStmt, *ForInStmt:
			if _, err := io.WriteString(out, "\n"); err != nil {
				return err
			}
		default:
			if _, err := io.WriteString(out, ";\n"); err != nil {
				return err
			}
		}
	}
	if _, err := io.WriteString(out, "  }"); err != nil {
		return err
	}
	return nil
}

// ForInStmt represents iteration over an iterable expression.
type ForInStmt struct {
	Name     string
	Iterable Expr
	Body     []Stmt
	ElemType string
}

func (f *ForInStmt) emit(out io.Writer) error {
	decl := "var"
	if f.ElemType != "" && f.ElemType != "dynamic" {
		decl = f.ElemType
	}
	saved := localVarTypes[f.Name]
	if f.ElemType != "" {
		localVarTypes[f.Name] = f.ElemType
	}
	if _, err := io.WriteString(out, "for ("+decl+" "+f.Name+" in "); err != nil {
		return err
	}
	if f.Iterable != nil {
		typ := inferType(f.Iterable)
		if strings.HasPrefix(typ, "Map<") {
			if err := f.Iterable.emit(out); err != nil {
				return err
			}
			if _, err := io.WriteString(out, ".keys"); err != nil {
				return err
			}
		} else if typ == "String" || typ == "String?" {
			if err := f.Iterable.emit(out); err != nil {
				return err
			}
			if typ == "String?" {
				if _, err := io.WriteString(out, "!"); err != nil {
					return err
				}
			}
			if _, err := io.WriteString(out, ".split('')"); err != nil {
				return err
			}
		} else {
			if err := f.Iterable.emit(out); err != nil {
				return err
			}
		}
	}
	if _, err := io.WriteString(out, ") {\n"); err != nil {
		return err
	}
	for _, st := range f.Body {
		if _, err := io.WriteString(out, "    "); err != nil {
			return err
		}
		if err := st.emit(out); err != nil {
			return err
		}
		switch st.(type) {
		case *IfStmt, *WhileStmt, *ForRangeStmt, *ForInStmt:
			if _, err := io.WriteString(out, "\n"); err != nil {
				return err
			}
		default:
			if _, err := io.WriteString(out, ";\n"); err != nil {
				return err
			}
		}
	}
	if _, err := io.WriteString(out, "  }"); err != nil {
		return err
	}
	if saved != "" {
		localVarTypes[f.Name] = saved
	} else {
		delete(localVarTypes, f.Name)
	}
	return nil
}

// IfStmt represents a conditional statement with an optional else branch.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

func (s *IfStmt) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "if ("); err != nil {
		return err
	}
	if err := s.Cond.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ") {\n"); err != nil {
		return err
	}
	for _, st := range s.Then {
		if _, err := io.WriteString(w, "    "); err != nil {
			return err
		}
		if err := st.emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ";\n"); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, "  }"); err != nil {
		return err
	}
	if len(s.Else) > 0 {
		if _, err := io.WriteString(w, " else {\n"); err != nil {
			return err
		}
		for _, st := range s.Else {
			if _, err := io.WriteString(w, "    "); err != nil {
				return err
			}
			if err := st.emit(w); err != nil {
				return err
			}
			if _, err := io.WriteString(w, ";\n"); err != nil {
				return err
			}
		}
		if _, err := io.WriteString(w, "  }"); err != nil {
			return err
		}
	}
	return nil
}

type VarStmt struct {
	Name  string
	Type  string
	Value Expr
}

func (s *VarStmt) emit(w io.Writer) error {
	nextStructHint = s.Name
	typ := s.Type
	if typ == "" {
		// Try to infer type from the assigned value for untyped variables
		typ = inferType(s.Value)
		if typ == "" {
			typ = "dynamic"
		}
	}
	if inFunc && s.Type == "" && typ != "dynamic" {
		valType := inferType(s.Value)
		if valType != "" && valType != typ {
			if strings.TrimSuffix(valType, "?") == typ {
				// keep non-null type
			} else {
				typ = valType
			}
		}
	}
	if _, ok := s.Value.(*IndexExpr); ok && strings.HasSuffix(typ, "?") {
		typ = strings.TrimSuffix(typ, "?")
	}
	nextStructHint = ""
	if n, ok := s.Value.(*Name); ok && n.Name == "null" && !strings.HasSuffix(typ, "?") {
		typ += "?"
	}
	localVarTypes[s.Name] = typ
	if typ == "dynamic" {
		if _, err := io.WriteString(w, "dynamic "+s.Name); err != nil {
			return err
		}
	} else {
		if _, err := io.WriteString(w, typ+" "+s.Name); err != nil {
			return err
		}
	}
	if s.Value != nil {
		baseType := strings.TrimSuffix(typ, "?")
		if ll, ok := s.Value.(*ListLit); ok && len(ll.Elems) == 0 && strings.HasPrefix(baseType, "List<") {
			elem := strings.TrimSuffix(strings.TrimPrefix(baseType, "List<"), ">")
			_, err := io.WriteString(w, " = <"+elem+">[]")
			return err
		}
		if ml, ok := s.Value.(*MapLit); ok && len(ml.Entries) == 0 && strings.HasPrefix(baseType, "Map<") {
			kv := strings.TrimSuffix(strings.TrimPrefix(baseType, "Map<"), ">")
			_, err := io.WriteString(w, " = <"+kv+">{}")
			return err
		}
		if n, ok := s.Value.(*Name); ok {
			_ = n
		}
		if iex, ok := s.Value.(*IndexExpr); ok {
			oldBang := iex.NoBang
			oldSuf := iex.NoSuffix
			iex.NoBang = true
			iex.NoSuffix = strings.HasSuffix(typ, "?")
			defer func() { iex.NoBang = oldBang; iex.NoSuffix = oldSuf }()
		}
		valType := inferType(s.Value)
		if typ == "BigInt" && valType == "int" {
			if lit, ok := s.Value.(*IntLit); ok {
				_, err := fmt.Fprintf(w, " = BigInt.from(%d)", lit.Value)
				return err
			}
			if _, err := io.WriteString(w, " = BigInt.from("); err != nil {
				return err
			}
			if err := s.Value.emit(w); err != nil {
				return err
			}
			_, err := io.WriteString(w, ")")
			return err
		}
		if typ == "int" && valType == "BigInt" {
			if _, err := io.WriteString(w, " = ("); err != nil {
				return err
			}
			if err := s.Value.emit(w); err != nil {
				return err
			}
			_, err := io.WriteString(w, ").toInt()")
			return err
		}
		if strings.HasPrefix(baseType, "Map<") {
			kv := strings.TrimSuffix(strings.TrimPrefix(baseType, "Map<"), ">")
			parts := strings.SplitN(kv, ",", 2)
			if len(parts) == 2 {
				targetMapValType = strings.TrimSpace(parts[1])
				defer func() { targetMapValType = "" }()
			}
		}
		if _, err := io.WriteString(w, " = "); err != nil {
			return err
		}
		return s.Value.emit(w)
	}
	baseType := strings.TrimSuffix(typ, "?")
	if strings.HasPrefix(baseType, "List<") {
		elem := strings.TrimSuffix(strings.TrimPrefix(baseType, "List<"), ">")
		_, err := io.WriteString(w, " = <"+elem+">[]")
		return err
	}
	if strings.HasPrefix(baseType, "Map<") {
		kv := strings.TrimSuffix(strings.TrimPrefix(baseType, "Map<"), ">")
		_, err := io.WriteString(w, " = <"+kv+">{}")
		return err
	}
	if typ == "String" {
		_, err := io.WriteString(w, " = \"\"")
		return err
	}
	if typ == "bool" {
		_, err := io.WriteString(w, " = false")
		return err
	}
	if typ == "int" || typ == "num" || typ == "double" || typ == "BigInt" {
		_, err := io.WriteString(w, " = 0")
		return err
	}
	return nil
}

type AssignStmt struct {
	Target Expr
	Value  Expr
}

func (s *AssignStmt) emit(w io.Writer) error {
	fullTargetType := inferType(s.Target)
	targetType := strings.TrimSuffix(fullTargetType, "?")
	if n, ok := s.Value.(*Name); ok && n.Name == "null" && !strings.HasSuffix(targetType, "?") {
		// assigning null to a non-null variable is a no-op
		return nil
	}
	if iex, ok := s.Target.(*IndexExpr); ok {
		base := strings.TrimSuffix(inferType(iex.Target), "?")
		if strings.HasPrefix(base, "List<") {
			elem := strings.TrimSuffix(strings.TrimPrefix(base, "List<"), ">")
			if _, err := io.WriteString(w, "while ("); err != nil {
				return err
			}
			if err := iex.Target.emit(w); err != nil {
				return err
			}
			if _, err := io.WriteString(w, ".length <= "); err != nil {
				return err
			}
			if err := iex.Index.emit(w); err != nil {
				return err
			}
			if _, err := io.WriteString(w, ") { "); err != nil {
				return err
			}
			if err := iex.Target.emit(w); err != nil {
				return err
			}
			if _, err := io.WriteString(w, ".add("); err != nil {
				return err
			}
			zv := zeroValue(elem)
			if zv == "null" {
				if _, err := io.WriteString(w, "null as dynamic"); err != nil {
					return err
				}
			} else {
				if _, err := io.WriteString(w, zv); err != nil {
					return err
				}
			}
			if _, err := io.WriteString(w, "); } "); err != nil {
				return err
			}
		}
		oldBang := iex.NoBang
		oldSuf := iex.NoSuffix
		iex.NoBang = true
		if strings.HasPrefix(strings.TrimSuffix(inferType(iex.Target), "?"), "Map<") {
			iex.NoSuffix = true
		}
		if idxIE, ok := iex.Index.(*IndexExpr); ok {
			oldIdxBang := idxIE.NoBang
			oldIdxSuf := idxIE.NoSuffix
			idxIE.NoBang = true
			idxIE.NoSuffix = false
			defer func() { idxIE.NoBang = oldIdxBang; idxIE.NoSuffix = oldIdxSuf }()
		}
		if err := iex.emit(w); err != nil {
			return err
		}
		iex.NoBang = oldBang
		iex.NoSuffix = oldSuf
	} else {
		if err := s.Target.emit(w); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, " = "); err != nil {
		return err
	}

	valType := inferType(s.Value)
	if targetType == "BigInt" && valType == "int" {
		if lit, ok := s.Value.(*IntLit); ok {
			_, err := fmt.Fprintf(w, "BigInt.from(%d)", lit.Value)
			return err
		}
		if _, err := io.WriteString(w, "BigInt.from("); err != nil {
			return err
		}
		if err := s.Value.emit(w); err != nil {
			return err
		}
		_, err := io.WriteString(w, ")")
		return err
	}
	if targetType == "int" && valType == "BigInt" {
		if _, err := io.WriteString(w, "("); err != nil {
			return err
		}
		if err := s.Value.emit(w); err != nil {
			return err
		}
		_, err := io.WriteString(w, ").toInt()")
		return err
	}
	if targetType == "int" && valType == "num" {
		if _, err := io.WriteString(w, "("); err != nil {
			return err
		}
		if err := s.Value.emit(w); err != nil {
			return err
		}
		_, err := io.WriteString(w, ").toInt()")
		return err
	}
	if strings.HasPrefix(targetType, "List<") && valType != targetType {
		return emitListConversion(w, s.Value, targetType)
	}
	baseType := strings.TrimSuffix(targetType, "?")
	if ll, ok := s.Value.(*ListLit); ok && len(ll.Elems) == 0 && strings.HasPrefix(baseType, "List<") {
		elem := strings.TrimSuffix(strings.TrimPrefix(baseType, "List<"), ">")
		_, err := io.WriteString(w, "<"+elem+">[]")
		return err
	}
	if ml, ok := s.Value.(*MapLit); ok && len(ml.Entries) == 0 && strings.HasPrefix(baseType, "Map<") {
		kv := strings.TrimSuffix(strings.TrimPrefix(baseType, "Map<"), ">")
		_, err := io.WriteString(w, "<"+kv+">{}")
		return err
	}
	if iex, ok := s.Value.(*IndexExpr); ok {
		oldBang := iex.NoBang
		oldSuf := iex.NoSuffix
		iex.NoBang = true
		needBang := false
		if t := inferType(iex.Target); strings.HasPrefix(strings.TrimSuffix(t, "?"), "Map<") {
			kv := strings.TrimSuffix(strings.TrimPrefix(strings.TrimSuffix(t, "?"), "Map<"), ">")
			parts := strings.SplitN(kv, ",", 2)
			if len(parts) == 2 {
				vt := strings.TrimSpace(parts[1])
				if !strings.HasSuffix(vt, "?") {
					needBang = true
				}
			}
		}
		iex.NoSuffix = !needBang
		defer func() { iex.NoBang = oldBang; iex.NoSuffix = oldSuf }()
		return iex.emit(w)
	}
	return s.Value.emit(w)
}

type LetStmt struct {
	Name  string
	Type  string
	Value Expr
}

func (s *LetStmt) emit(w io.Writer) error {
	nextStructHint = s.Name
	typ := s.Type
	if typ == "" {
		typ = "dynamic"
		if s.Value != nil {
			if vt := inferType(s.Value); vt != "" && vt != "dynamic" {
				typ = vt
			}
		}
	}
	if s.Value == nil {
		nextStructHint = ""
		localVarTypes[s.Name] = typ
		decl := typ + " " + s.Name + ";"
		if typ != "dynamic" && !strings.HasSuffix(typ, "?") {
			decl = "late " + typ + " " + s.Name + ";"
		}
		_, err := io.WriteString(w, decl)
		return err
	}
	if _, ok := s.Value.(*IndexExpr); ok && strings.HasSuffix(typ, "?") {
		typ = strings.TrimSuffix(typ, "?")
	}
	nextStructHint = ""
	localVarTypes[s.Name] = typ
	if typ == "dynamic" {
		if _, err := io.WriteString(w, "dynamic "+s.Name+" = "); err != nil {
			return err
		}
	} else {
		if _, err := io.WriteString(w, typ+" "+s.Name+" = "); err != nil {
			return err
		}
	}
	if iex, ok := s.Value.(*IndexExpr); ok {
		oldBang := iex.NoBang
		oldSuf := iex.NoSuffix
		iex.NoBang = true
		iex.NoSuffix = strings.HasSuffix(typ, "?")
		defer func() { iex.NoBang = oldBang; iex.NoSuffix = oldSuf }()
	}
	valType := inferType(s.Value)
	if typ == "BigInt" && valType == "int" {
		if lit, ok := s.Value.(*IntLit); ok {
			_, err := fmt.Fprintf(w, "BigInt.from(%d)", lit.Value)
			return err
		}
		if _, err := io.WriteString(w, "BigInt.from("); err != nil {
			return err
		}
		if err := s.Value.emit(w); err != nil {
			return err
		}
		_, err := io.WriteString(w, ")")
		return err
	}
	if typ == "int" && valType == "BigInt" {
		if _, err := io.WriteString(w, "("); err != nil {
			return err
		}
		if err := s.Value.emit(w); err != nil {
			return err
		}
		_, err := io.WriteString(w, ").toInt()")
		return err
	}
	baseType := strings.TrimSuffix(typ, "?")
	if strings.HasPrefix(baseType, "Map<") {
		kv := strings.TrimSuffix(strings.TrimPrefix(baseType, "Map<"), ">")
		parts := strings.SplitN(kv, ",", 2)
		if len(parts) == 2 {
			targetMapValType = strings.TrimSpace(parts[1])
			defer func() { targetMapValType = "" }()
		}
	}
	return s.Value.emit(w)
}

// ReturnStmt represents a `return` statement.
type ReturnStmt struct {
	Value Expr
}

func (s *ReturnStmt) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "return"); err != nil {
		return err
	}
	if s.Value != nil {
		if _, err := io.WriteString(w, " "); err != nil {
			return err
		}
		valType := inferType(s.Value)
		if currentRetType == "int" && (valType == "num" || valType == "BigInt") {
			if _, err := io.WriteString(w, "("); err != nil {
				return err
			}
			if err := s.Value.emit(w); err != nil {
				return err
			}
			if _, err := io.WriteString(w, ").toInt()"); err != nil {
				return err
			}
		} else if !strings.HasSuffix(currentRetType, "?") && strings.HasSuffix(valType, "?") {
			if _, err := io.WriteString(w, "("); err != nil {
				return err
			}
			if err := s.Value.emit(w); err != nil {
				return err
			}
			if _, err := io.WriteString(w, ")!"); err != nil {
				return err
			}
		} else if strings.HasPrefix(currentRetType, "List<") && valType != currentRetType {
			if err := emitListConversion(w, s.Value, currentRetType); err != nil {
				return err
			}
		} else {
			if err := s.Value.emit(w); err != nil {
				return err
			}
		}
	}
	return nil
}

// BreakStmt represents a `break` statement.
type BreakStmt struct{}

func (s *BreakStmt) emit(w io.Writer) error {
	_, err := io.WriteString(w, "break")
	return err
}

// ContinueStmt represents a `continue` statement.
type ContinueStmt struct{}

func (s *ContinueStmt) emit(w io.Writer) error {
	_, err := io.WriteString(w, "continue")
	return err
}

// BenchStmt measures execution time and memory usage of a statement block.
type BenchStmt struct {
	Name string
	Body []Stmt
}

// stmtCallsMain reports whether the statement or any of its nested statements
// contains a call to the `main` function with no arguments.
func stmtCallsMain(st Stmt) bool {
	switch s := st.(type) {
	case *ExprStmt:
		if call, ok := s.Expr.(*CallExpr); ok {
			if name, ok := call.Func.(*Name); ok && name.Name == "main" && len(call.Args) == 0 {
				return true
			}
		}
	case *BenchStmt:
		for _, b := range s.Body {
			if stmtCallsMain(b) {
				return true
			}
		}
	case *IfStmt:
		for _, st := range s.Then {
			if stmtCallsMain(st) {
				return true
			}
		}
		for _, st := range s.Else {
			if stmtCallsMain(st) {
				return true
			}
		}
	case *WhileStmt:
		for _, st := range s.Body {
			if stmtCallsMain(st) {
				return true
			}
		}
	case *ForRangeStmt:
		for _, st := range s.Body {
			if stmtCallsMain(st) {
				return true
			}
		}
	case *ForInStmt:
		for _, st := range s.Body {
			if stmtCallsMain(st) {
				return true
			}
		}
	}
	return false
}

func (b *BenchStmt) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "{\n"); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "  var _benchMem0 = ProcessInfo.currentRss;\n"); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "  var _benchSw = Stopwatch()..start();\n"); err != nil {
		return err
	}
	for _, st := range b.Body {
		if _, err := io.WriteString(w, "  "); err != nil {
			return err
		}
		if err := st.emit(w); err != nil {
			return err
		}
		if isBlockStmt(st) {
			if _, err := io.WriteString(w, "\n"); err != nil {
				return err
			}
		} else {
			if _, err := io.WriteString(w, ";\n"); err != nil {
				return err
			}
		}
	}
	if _, err := io.WriteString(w, "  _benchSw.stop();\n"); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "  var _benchMem1 = ProcessInfo.currentRss;\n"); err != nil {
		return err
	}
	if _, err := fmt.Fprintf(w, "  print(jsonEncode({\"duration_us\": _benchSw.elapsedMicroseconds, \"memory_bytes\": (_benchMem1 - _benchMem0).abs(), \"name\": %q}));\n", b.Name); err != nil {
		return err
	}
	_, err := io.WriteString(w, "}")
	return err
}

// SaveStmt saves a list of maps to stdout in JSONL format.
type SaveStmt struct {
	Src    Expr
	Path   string
	Format string
}

func (s *SaveStmt) emit(w io.Writer) error {
	if s.Format == "jsonl" && (s.Path == "" || s.Path == "-") {
		if _, err := io.WriteString(w, "for (var _row in "); err != nil {
			return err
		}
		if err := s.Src.emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ") {\n  "); err != nil {
			return err
		}
		elemType := inferType(s.Src)
		if strings.HasPrefix(elemType, "List<") && strings.HasSuffix(elemType, ">") {
			elemType = strings.TrimSuffix(strings.TrimPrefix(elemType, "List<"), ">")
		}
		if fields, ok := structFields[elemType]; ok {
			if _, err := io.WriteString(w, "var _tmp = {"); err != nil {
				return err
			}
			for i, f := range fields {
				if i > 0 {
					if _, err := io.WriteString(w, ", "); err != nil {
						return err
					}
				}
				if _, err := fmt.Fprintf(w, "%q: _row.%s", f.Name, f.Name); err != nil {
					return err
				}
			}
			if _, err := io.WriteString(w, "};\n  print(jsonEncode(_tmp));\n}"); err != nil {
				return err
			}
		} else {
			if _, err := io.WriteString(w, "print(jsonEncode(_row));\n}"); err != nil {
				return err
			}
		}
		return nil
	}
	_, err := io.WriteString(w, "// unsupported save")
	return err
}

// UpdateStmt represents an `update` statement on a list of structs.
type UpdateStmt struct {
	Target string
	Fields []string
	Values []Expr
	Cond   Expr
}

func (u *UpdateStmt) emit(w io.Writer) error {
	fmt.Fprintf(w, "for (var i = 0; i < %s.length; i++) {\n", u.Target)
	fmt.Fprintf(w, "  var item = %s[i];\n", u.Target)
	pad := "  "
	if u.Cond != nil {
		fmt.Fprintf(w, "  if (")
		if err := u.Cond.emit(w); err != nil {
			return err
		}
		fmt.Fprintf(w, ") {\n")
		pad = "    "
	}
	for i, f := range u.Fields {
		fmt.Fprintf(w, "%sitem[%q] = ", pad, f)
		if err := u.Values[i].emit(w); err != nil {
			return err
		}
		fmt.Fprintf(w, ";\n")
	}
	if u.Cond != nil {
		fmt.Fprintf(w, "  }\n")
	}
	fmt.Fprintf(w, "  %s[i] = item;\n", u.Target)
	fmt.Fprintf(w, "}\n")
	return nil
}

// FuncDecl represents a function definition.
type FuncDecl struct {
	Name       string
	Params     []string
	ParamTypes map[string]string
	Body       []Stmt
}

func (f *FuncDecl) emit(w io.Writer) error {
	retType := funcReturnTypes[f.Name]
	if retType == "" {
		retType = inferReturnType(f.Body)
	}
	savedRet := currentRetType
	currentRetType = retType
	inFunc = true
	// Save current variable type map to avoid leaking local definitions
	savedVars := map[string]string{}
	for k, v := range localVarTypes {
		savedVars[k] = v
	}
	if _, err := io.WriteString(w, retType+" "+f.Name+"("); err != nil {
		return err
	}
	for i, p := range f.Params {
		if i > 0 {
			if _, err := io.WriteString(w, ", "); err != nil {
				return err
			}
		}
		if _, err := io.WriteString(w, p); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, ") {\n"); err != nil {
		return err
	}
	saved := map[string]string{}
	for n, t := range f.ParamTypes {
		if old, ok := localVarTypes[n]; ok {
			saved[n] = old
		}
		localVarTypes[n] = t
	}
	for _, st := range f.Body {
		if _, err := io.WriteString(w, "  "); err != nil {
			return err
		}
		if err := st.emit(w); err != nil {
			return err
		}
		switch st.(type) {
		case *IfStmt, *WhileStmt, *ForRangeStmt, *ForInStmt, *FuncDecl:
			if _, err := io.WriteString(w, "\n"); err != nil {
				return err
			}
		default:
			if _, err := io.WriteString(w, ";\n"); err != nil {
				return err
			}
		}
	}
	for n := range f.ParamTypes {
		if v, ok := saved[n]; ok {
			localVarTypes[n] = v
		} else {
			delete(localVarTypes, n)
		}
	}
	// Restore variable type map to state before entering the function
	for k := range localVarTypes {
		if _, ok := savedVars[k]; !ok {
			delete(localVarTypes, k)
		}
	}
	for k, v := range savedVars {
		localVarTypes[k] = v
	}
	inFunc = false
	currentRetType = savedRet
	_, err := io.WriteString(w, "}")
	return err
}

type ExprStmt struct{ Expr Expr }

func (s *ExprStmt) emit(w io.Writer) error { return s.Expr.emit(w) }

type Expr interface{ emit(io.Writer) error }

type UnaryExpr struct {
	Op string
	X  Expr
}

func (u *UnaryExpr) emit(w io.Writer) error {
	if u.Op == "-" {
		t := inferType(u.X)
		if t == "BigRat" {
			useBigRat = true
			if _, err := io.WriteString(w, "_neg("); err != nil {
				return err
			}
			if err := u.X.emit(w); err != nil {
				return err
			}
			_, err := io.WriteString(w, ")")
			return err
		}
	}
	if _, err := io.WriteString(w, u.Op); err != nil {
		return err
	}
	// Wrap the operand in parentheses when it is a binary expression to
	// preserve the intended precedence of the original Mochi expression.
	if _, ok := u.X.(*BinaryExpr); ok {
		if _, err := io.WriteString(w, "("); err != nil {
			return err
		}
		if err := u.X.emit(w); err != nil {
			return err
		}
		_, err := io.WriteString(w, ")")
		return err
	}
	return u.X.emit(w)
}

type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (b *BinaryExpr) emit(w io.Writer) error {
	if b.Op == "in" {
		if err := b.Right.emit(w); err != nil {
			return err
		}
		method := ".contains("
		if strings.HasPrefix(inferType(b.Right), "Map<") {
			method = ".containsKey("
		} else if sel, ok := b.Right.(*SelectorExpr); ok {
			if fields, ok := structFields[inferType(sel.Receiver)]; ok {
				for _, f := range fields {
					if f.Name == sel.Field && strings.HasPrefix(f.Type, "Map<") {
						method = ".containsKey("
						break
					}
				}
			}
		}
		if _, err := io.WriteString(w, method); err != nil {
			return err
		}
		if err := b.Left.emit(w); err != nil {
			return err
		}
		_, err := io.WriteString(w, ")")
		return err
	}
	lt := inferType(b.Left)
	rt := inferType(b.Right)
	if iex, ok := b.Left.(*IndexExpr); ok && strings.HasSuffix(lt, "?") {
		oldBang := iex.NoBang
		oldSuf := iex.NoSuffix
		iex.NoSuffix = false
		defer func() { iex.NoBang = oldBang; iex.NoSuffix = oldSuf }()
	}
	if iex, ok := b.Right.(*IndexExpr); ok && strings.HasSuffix(rt, "?") {
		oldBang := iex.NoBang
		oldSuf := iex.NoSuffix
		iex.NoSuffix = false
		defer func() { iex.NoBang = oldBang; iex.NoSuffix = oldSuf }()
	}
	if (b.Op == "==" || b.Op == "!=") && strings.HasPrefix(lt, "List<") && strings.HasPrefix(rt, "List<") {
		usesJSON = true
		if _, err := io.WriteString(w, "jsonEncode("); err != nil {
			return err
		}
		if err := b.Left.emit(w); err != nil {
			return err
		}
		if b.Op == "==" {
			if _, err := io.WriteString(w, ") == jsonEncode("); err != nil {
				return err
			}
		} else {
			if _, err := io.WriteString(w, ") != jsonEncode("); err != nil {
				return err
			}
		}
		if err := b.Right.emit(w); err != nil {
			return err
		}
		_, err := io.WriteString(w, ")")
		return err
	}
	if lt == "BigRat" || rt == "BigRat" {
		useBigRat = true
		var fn string
		switch b.Op {
		case "+":
			fn = "_add"
		case "-":
			fn = "_sub"
		case "*":
			fn = "_mul"
		case "/":
			fn = "_div"
		}
		if fn != "" {
			io.WriteString(w, fn+"(")
			if err := b.Left.emit(w); err != nil {
				return err
			}
			io.WriteString(w, ", ")
			if err := b.Right.emit(w); err != nil {
				return err
			}
			_, err := io.WriteString(w, ")")
			return err
		}
	}
	target := lt
	if lt == "BigInt" || rt == "BigInt" {
		target = "BigInt"
	} else if rt != lt {
		target = lt
	}
	if b.Op == "+" && ((lt == "String" && rt == "List<String>") || (lt == "List<String>" && rt == "String")) {
		if lt == "List<String>" {
			if _, err := io.WriteString(w, "("); err != nil {
				return err
			}
			if err := b.Left.emit(w); err != nil {
				return err
			}
			if _, err := io.WriteString(w, ").join()"); err != nil {
				return err
			}
		} else {
			if err := b.Left.emit(w); err != nil {
				return err
			}
		}
		if _, err := io.WriteString(w, " + "); err != nil {
			return err
		}
		if rt == "List<String>" {
			if _, err := io.WriteString(w, "("); err != nil {
				return err
			}
			if err := b.Right.emit(w); err != nil {
				return err
			}
			if _, err := io.WriteString(w, ").join()"); err != nil {
				return err
			}
		} else {
			if err := b.Right.emit(w); err != nil {
				return err
			}
		}
		return nil
	}
	left := func() error { return emitWithBigIntCast(w, b.Left, lt, target) }
	right := func() error { return emitWithBigIntCast(w, b.Right, rt, target) }
	if (b.Op == "<" || b.Op == "<=" || b.Op == ">" || b.Op == ">=") && (lt == "String" || isMaybeString(b.Left)) && (rt == "String" || isMaybeString(b.Right)) {
		if err := left(); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ".compareTo("); err != nil {
			return err
		}
		if err := right(); err != nil {
			return err
		}
		var cmp string
		switch b.Op {
		case "<":
			cmp = " < 0"
		case "<=":
			cmp = " <= 0"
		case ">":
			cmp = " > 0"
		case ">=":
			cmp = " >= 0"
		}
		_, err := io.WriteString(w, ")"+cmp)
		return err
	}
	if (b.Op == "<" || b.Op == "<=" || b.Op == ">" || b.Op == ">=") && (lt == "dynamic" || rt == "dynamic") {
		// When types are dynamic we can't rely on the comparison
		// operators being defined directly. Instead use Comparable
		// which works for strings and numbers.
		if err := b.Left.emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ".compareTo("); err != nil {
			return err
		}
		if err := b.Right.emit(w); err != nil {
			return err
		}
		var cmp string
		switch b.Op {
		case "<":
			cmp = " < 0"
		case "<=":
			cmp = " <= 0"
		case ">":
			cmp = " > 0"
		case ">=":
			cmp = " >= 0"
		}
		_, err := io.WriteString(w, ")"+cmp)
		return err
	}
	lp := precedence(b.Left)
	bp := precedence(b)
	if lp > bp || ((b.Op == "==" || b.Op == "!=") && isComparisonExpr(b.Left)) {
		if _, err := io.WriteString(w, "("); err != nil {
			return err
		}
		if err := left(); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ")"); err != nil {
			return err
		}
	} else {
		if err := left(); err != nil {
			return err
		}
	}
	op := b.Op
	if b.Op == "/" {
		if (lt == "int" && rt == "int") || lt == "BigInt" || rt == "BigInt" ||
			(lt == "dynamic" && rt != "num") || (rt == "dynamic" && lt != "num") {
			// Use integer division when operands are integers or dynamic values
			// that are likely integers.
			op = "~/"
		}
	}
	if _, err := io.WriteString(w, " "+op+" "); err != nil {
		return err
	}
	rp := precedence(b.Right)
	// Ensure correct evaluation order for expressions like `a - (b + c)`
	// where the right operand has the same precedence as the parent.
	if rp >= bp || ((b.Op == "==" || b.Op == "!=") && isComparisonExpr(b.Right)) {
		if _, err := io.WriteString(w, "("); err != nil {
			return err
		}
		if err := right(); err != nil {
			return err
		}
		_, err := io.WriteString(w, ")")
		return err
	}
	return right()
}

func precedence(e Expr) int {
	switch ex := e.(type) {
	case *BinaryExpr:
		switch ex.Op {
		case "*", "/", "%":
			return 1
		case "+", "-":
			return 2
		case "<", "<=", ">", ">=":
			return 3
		case "==", "!=", "in":
			return 4
		case "&&":
			return 5
		case "||":
			return 6
		case "union", "union_all", "except", "intersect":
			return 7
		}
	case *CastExpr:
		return 8
	}
	return 0
}

func isComparisonExpr(e Expr) bool {
	if b, ok := e.(*BinaryExpr); ok {
		switch b.Op {
		case "==", "!=", "<", "<=", ">", ">=":
			return true
		}
	}
	return false
}

// CondExpr represents a conditional expression like `cond ? a : b`.
type CondExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

func (c *CondExpr) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "("); err != nil {
		return err
	}
	if err := c.Cond.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, " ? "); err != nil {
		return err
	}
	if err := c.Then.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, " : "); err != nil {
		return err
	}
	if err := c.Else.emit(w); err != nil {
		return err
	}
	_, err := io.WriteString(w, ")")
	return err
}

type CallExpr struct {
	Func Expr
	Args []Expr
}

// PanicExpr represents a call to panic that throws an exception.
type PanicExpr struct{ Arg Expr }

// emit ignores panic calls so generated programs can continue executing even
// when a `panic` is invoked. This mirrors the behaviour used in other
// transpilers where domain checks are best-effort only.
// The surrounding statement emission code will still add a trailing semicolon,
// resulting in an empty statement in the output.
func (p *PanicExpr) emit(w io.Writer) error { return nil }

func (c *CallExpr) emit(w io.Writer) error {
	if sel, ok := c.Func.(*SelectorExpr); ok {
		if sel.Field == "keys" && len(c.Args) == 0 {
			if strings.HasPrefix(inferType(sel.Receiver), "Map<") {
				if err := sel.Receiver.emit(w); err != nil {
					return err
				}
				if _, err := io.WriteString(w, ".keys"); err != nil {
					return err
				}
				return nil
			}
		}
		if sel.Field == "padStart" && len(c.Args) == 2 {
			sel.Field = "padLeft"
		}
	}
	if n, ok := c.Func.(*Name); ok {
		if n.Name == "keys" && len(c.Args) == 1 {
			if strings.HasPrefix(inferType(c.Args[0]), "Map<") {
				if err := c.Args[0].emit(w); err != nil {
					return err
				}
				_, err := io.WriteString(w, ".keys")
				return err
			}
		}
		if n.Name == "contains" && len(c.Args) == 2 {
			typ := inferType(c.Args[0])
			if strings.HasPrefix(typ, "Map<") {
				if err := c.Args[0].emit(w); err != nil {
					return err
				}
				if _, err := io.WriteString(w, ".containsKey("); err != nil {
					return err
				}
				if err := c.Args[1].emit(w); err != nil {
					return err
				}
				_, err := io.WriteString(w, ")")
				return err
			}
			if err := c.Args[0].emit(w); err != nil {
				return err
			}
			if _, err := io.WriteString(w, ".contains("); err != nil {
				return err
			}
			if err := c.Args[1].emit(w); err != nil {
				return err
			}
			_, err := io.WriteString(w, ")")
			return err
		}
	}
	var paramTypes []string
	if n, ok := c.Func.(*Name); ok && currentEnv != nil {
		if fn, ok := currentEnv.GetFunc(n.Name); ok {
			paramTypes = make([]string, len(fn.Params))
			for i, p := range fn.Params {
				paramTypes[i] = typeRefString(p.Type)
			}
		}
	}

	if err := c.Func.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "("); err != nil {
		return err
	}
	for i, a := range c.Args {
		if i > 0 {
			if _, err := io.WriteString(w, ", "); err != nil {
				return err
			}
		}
		at := inferType(a)
		if i < len(paramTypes) && paramTypes[i] == "int" && (at == "num" || at == "BigInt") {
			if _, err := io.WriteString(w, "("); err != nil {
				return err
			}
			if err := a.emit(w); err != nil {
				return err
			}
			if _, err := io.WriteString(w, ").toInt()"); err != nil {
				return err
			}
		} else if i < len(paramTypes) && strings.HasPrefix(paramTypes[i], "List<") && inferType(a) == "List<dynamic>" {
			elem := strings.TrimSuffix(strings.TrimPrefix(paramTypes[i], "List<"), ">")
			if _, err := io.WriteString(w, "List<"+elem+">.from("); err != nil {
				return err
			}
			if err := a.emit(w); err != nil {
				return err
			}
			if _, err := io.WriteString(w, ")"); err != nil {
				return err
			}
		} else {
			if iex, ok := a.(*IndexExpr); ok && strings.HasSuffix(inferType(a), "?") && (i >= len(paramTypes) || !strings.HasSuffix(paramTypes[i], "?")) {
				oldBang := iex.NoBang
				oldSuf := iex.NoSuffix
				iex.NoBang = true
				iex.NoSuffix = false
				if err := a.emit(w); err != nil {
					return err
				}
				iex.NoBang = oldBang
				iex.NoSuffix = oldSuf
			} else {
				if err := a.emit(w); err != nil {
					return err
				}
			}
		}
	}
	_, err := io.WriteString(w, ")")
	return err
}

type Name struct{ Name string }

func (n *Name) emit(w io.Writer) error {
	if n.Name == "nil" {
		_, err := io.WriteString(w, "null")
		return err
	}
	name := n.Name
	if (benchMain || renameMain) && name == "main" {
		name = "_main"
	}
	_, err := io.WriteString(w, name)
	return err
}

// SelectorExpr represents receiver.field access.
type SelectorExpr struct {
	Receiver Expr
	Field    string
}

func (s *SelectorExpr) emit(w io.Writer) error {
	t := inferType(s.Receiver)
	if strings.HasPrefix(t, "Map<") {
		if err := s.Receiver.emit(w); err != nil {
			return err
		}
		_, err := fmt.Fprintf(w, "[%q]", s.Field)
		return err
	}
	if precedence(s.Receiver) > 0 {
		if _, err := io.WriteString(w, "("); err != nil {
			return err
		}
		if err := s.Receiver.emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ")"); err != nil {
			return err
		}
	} else {
		if err := s.Receiver.emit(w); err != nil {
			return err
		}
	}
	_, err := io.WriteString(w, "."+s.Field)
	return err
}

type StringLit struct{ Value string }

func (s *StringLit) emit(w io.Writer) error {
	val := strings.ReplaceAll(s.Value, "\\", "\\\\")
	val = strings.ReplaceAll(val, "\"", "\\\"")
	val = strings.ReplaceAll(val, "\n", "\\n")
	val = strings.ReplaceAll(val, "\r", "\\r")
	val = strings.ReplaceAll(val, "$", "\\$")
	_, err := io.WriteString(w, "\""+val+"\"")
	return err
}

type IntLit struct{ Value int }

func (i *IntLit) emit(w io.Writer) error { _, err := fmt.Fprintf(w, "%d", i.Value); return err }

// FloatLit represents a floating point literal.
type FloatLit struct{ Value float64 }

func (f *FloatLit) emit(w io.Writer) error {
	s := strconv.FormatFloat(f.Value, 'f', -1, 64)
	if !strings.ContainsAny(s, ".eE") {
		s += ".0"
	}
	_, err := io.WriteString(w, s)
	return err
}

// BoolLit represents a boolean literal.
type BoolLit struct{ Value bool }

func (b *BoolLit) emit(w io.Writer) error {
	if b.Value {
		_, err := io.WriteString(w, "true")
		return err
	}
	_, err := io.WriteString(w, "false")
	return err
}

// ListLit represents a list literal.
// ListLit represents a list literal. ElemType is used for empty list literals
// when a concrete element type is known from context (e.g. function
// parameter type). When ElemType is non-empty and the list has no elements,
// the literal will be emitted as `<ElemType>[]` to avoid `List<dynamic>`.
type ListLit struct {
	Elems    []Expr
	ElemType string
}

func (l *ListLit) emit(w io.Writer) error {
	if len(l.Elems) == 0 {
		if l.ElemType != "" {
			_, err := io.WriteString(w, "<"+l.ElemType+">[]")
			return err
		}
		_, err := io.WriteString(w, "[]")
		return err
	}
	allNull := true
	for _, e := range l.Elems {
		if n, ok := e.(*Name); !ok || n.Name != "null" {
			allNull = false
			break
		}
	}
	if allNull {
		if _, err := io.WriteString(w, "<dynamic>["); err != nil {
			return err
		}
	} else {
		if _, err := io.WriteString(w, "["); err != nil {
			return err
		}
	}
	for i, e := range l.Elems {
		if i > 0 {
			if _, err := io.WriteString(w, ", "); err != nil {
				return err
			}
		}
		if err := e.emit(w); err != nil {
			return err
		}
		if strings.HasSuffix(inferType(e), "?") {
			if _, err := io.WriteString(w, "!"); err != nil {
				return err
			}
		}
	}
	if _, err := io.WriteString(w, "]"); err != nil {
		return err
	}
	return nil
}

// MapLit represents a simple map literal.
type MapLit struct{ Entries []MapEntry }

// MapEntry is a key/value pair inside a map literal.
type MapEntry struct {
	Key   Expr
	Value Expr
}

// MapGetExpr represents m.get(key, default).
type MapGetExpr struct {
	Map     Expr
	Key     Expr
	Default Expr
}

func (m *MapGetExpr) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "("); err != nil {
		return err
	}
	if err := m.Map.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ".containsKey("); err != nil {
		return err
	}
	if err := m.Key.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ") ? "); err != nil {
		return err
	}
	if err := (&IndexExpr{Target: m.Map, Index: m.Key, NoBang: false}).emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, " : "); err != nil {
		return err
	}
	if m.Default != nil {
		if err := m.Default.emit(w); err != nil {
			return err
		}
	} else {
		if _, err := io.WriteString(w, "null"); err != nil {
			return err
		}
	}
	_, err := io.WriteString(w, ")")
	return err
}

// NotNilExpr appends `!` to an expression to assert it is not null.
type NotNilExpr struct{ X Expr }

func (n *NotNilExpr) emit(w io.Writer) error {
	if err := n.X.emit(w); err != nil {
		return err
	}
	_, err := io.WriteString(w, "!")
	return err
}

func (m *MapLit) emit(w io.Writer) error {
	if name, ok := mapLitStructName[m]; ok {
		if _, err := io.WriteString(w, name+"("); err != nil {
			return err
		}
		for i, e := range m.Entries {
			if i > 0 {
				if _, err := io.WriteString(w, ", "); err != nil {
					return err
				}
			}
			var fname string
			switch k := e.Key.(type) {
			case *Name:
				fname = k.Name
			case *StringLit:
				fname = k.Value
			default:
				fname = ""
			}
			if fname != "" {
				if _, err := io.WriteString(w, fname+": "); err != nil {
					return err
				}
			}
			if err := e.Value.emit(w); err != nil {
				return err
			}
		}
		_, err := io.WriteString(w, ")")
		return err
	}
	t := inferType(m)
	valOptional := true
	if strings.HasPrefix(t, "Map<") {
		kv := strings.TrimSuffix(strings.TrimPrefix(strings.TrimSuffix(t, "?"), "Map<"), ">")
		parts := strings.SplitN(kv, ",", 2)
		if len(parts) == 2 {
			valOptional = strings.HasSuffix(strings.TrimSpace(parts[1]), "?")
		}
	}
	if _, err := io.WriteString(w, "{"); err != nil {
		return err
	}
	for i, e := range m.Entries {
		if i > 0 {
			if _, err := io.WriteString(w, ", "); err != nil {
				return err
			}
		}
		if n, ok := e.Key.(*Name); ok {
			if currentEnv != nil {
				if t, err := currentEnv.GetVar(n.Name); err == nil {
					if _, ok := t.(types.FuncType); !ok {
						if err := n.emit(w); err != nil {
							return err
						}
					} else {
						if _, err := fmt.Fprintf(w, "\"%s\"", n.Name); err != nil {
							return err
						}
					}
				} else {
					if _, err := fmt.Fprintf(w, "\"%s\"", n.Name); err != nil {
						return err
					}
				}
			} else {
				if _, err := fmt.Fprintf(w, "\"%s\"", n.Name); err != nil {
					return err
				}
			}
		} else {
			if err := e.Key.emit(w); err != nil {
				return err
			}
		}
		if _, err := io.WriteString(w, ": "); err != nil {
			return err
		}
		if iex, ok := e.Value.(*IndexExpr); ok && !valOptional && strings.HasSuffix(inferType(e.Value), "?") {
			oldBang := iex.NoBang
			oldSuf := iex.NoSuffix
			iex.NoBang = true
			iex.NoSuffix = false
			if err := iex.emit(w); err != nil {
				return err
			}
			iex.NoBang = oldBang
			iex.NoSuffix = oldSuf
		} else {
			if err := e.Value.emit(w); err != nil {
				return err
			}
			if targetMapValType == "int" && inferType(e.Value) == "BigInt" {
				if _, err := io.WriteString(w, ".toInt()"); err != nil {
					return err
				}
			}
		}
	}
	_, err := io.WriteString(w, "}")
	return err
}

// IndexExpr represents target[index].
type IndexExpr struct {
	Target   Expr
	Index    Expr
	NoBang   bool
	NoSuffix bool
}

// SliceExpr represents target[start:end].
type SliceExpr struct {
	Target Expr
	Start  Expr
	End    Expr
}

func (s *SliceExpr) emit(w io.Writer) error {
	if inferType(s.Target) == "String" || isMaybeString(s.Target) {
		useSubstrClamp = true
		if _, err := io.WriteString(w, "_substr("); err != nil {
			return err
		}
		if err := s.Target.emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ", "); err != nil {
			return err
		}
		if s.Start != nil {
			if err := s.Start.emit(w); err != nil {
				return err
			}
		} else {
			if _, err := io.WriteString(w, "0"); err != nil {
				return err
			}
		}
		if _, err := io.WriteString(w, ", "); err != nil {
			return err
		}
		if s.End != nil {
			if err := s.End.emit(w); err != nil {
				return err
			}
		} else {
			if err := s.Target.emit(w); err != nil {
				return err
			}
			if _, err := io.WriteString(w, ".length"); err != nil {
				return err
			}
		}
		_, err := io.WriteString(w, ")")
		return err
	}
	if err := s.Target.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ".sublist("); err != nil {
		return err
	}
	if s.Start != nil {
		if err := s.Start.emit(w); err != nil {
			return err
		}
	} else {
		if _, err := io.WriteString(w, "0"); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, ", "); err != nil {
		return err
	}
	if s.End != nil {
		if err := s.End.emit(w); err != nil {
			return err
		}
	} else {
		if err := s.Target.emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ".length"); err != nil {
			return err
		}
	}
	_, err := io.WriteString(w, ")")
	return err
}

func (i *IndexExpr) emit(w io.Writer) error {
	t := inferType(i.Target)
	wrap := false
	needDefault := strings.HasPrefix(t, "Map<") && !i.NoBang
	switch i.Target.(type) {
	case *CastExpr:
		wrap = true
	}
	if fields, ok := structFields[t]; ok {
		if s, ok2 := i.Index.(*StringLit); ok2 {
			for _, f := range fields {
				if f.Name == s.Value {
					if err := i.Target.emit(w); err != nil {
						return err
					}
					_, err := io.WriteString(w, "."+s.Value)
					return err
				}
			}
		}
	}
	if t == "String" {
		if err := i.Target.emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ".substring("); err != nil {
			return err
		}
		if i.Index != nil {
			if err := i.Index.emit(w); err != nil {
				return err
			}
		} else {
			if _, err := io.WriteString(w, "0"); err != nil {
				return err
			}
		}
		if _, err := io.WriteString(w, ", "); err != nil {
			return err
		}
		if i.Index != nil {
			if err := i.Index.emit(w); err != nil {
				return err
			}
			if _, err := io.WriteString(w, " + 1"); err != nil {
				return err
			}
		} else {
			if err := i.Target.emit(w); err != nil {
				return err
			}
			if _, err := io.WriteString(w, ".length"); err != nil {
				return err
			}
		}
		_, err := io.WriteString(w, ")")
		return err
	}
	if wrap {
		if _, err := io.WriteString(w, "("); err != nil {
			return err
		}
		if err := i.Target.emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ")"); err != nil {
			return err
		}
	} else {
		if needDefault {
			if _, err := io.WriteString(w, "("); err != nil {
				return err
			}
		}
		if err := i.Target.emit(w); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, "["); err != nil {
		return err
	}
	if i.Index != nil {
		idxType := inferType(i.Index)
		needToInt := idxType == "BigInt" || idxType == "num"
		if idxType == "dynamic" {
			if strings.HasPrefix(strings.TrimSuffix(t, "?"), "Map<") {
				kv := strings.TrimSuffix(strings.TrimPrefix(strings.TrimSuffix(t, "?"), "Map<"), ">")
				parts := strings.SplitN(kv, ",", 2)
				if len(parts) == 2 {
					kt := strings.TrimSpace(parts[0])
					needToInt = kt == "int"
				} else {
					needToInt = false
				}
			} else {
				needToInt = false
			}
		}
		if needToInt {
			if _, err := io.WriteString(w, "("); err != nil {
				return err
			}
			if err := i.Index.emit(w); err != nil {
				return err
			}
			if _, err := io.WriteString(w, ").toInt()"); err != nil {
				return err
			}
		} else {
			if err := i.Index.emit(w); err != nil {
				return err
			}
		}
	}
	if _, err := io.WriteString(w, "]"); err != nil {
		return err
	}
	if strings.HasPrefix(t, "Map<") {
		if !i.NoBang {
			kv := strings.TrimSuffix(strings.TrimPrefix(strings.TrimSuffix(t, "?"), "Map<"), ">")
			parts := strings.SplitN(kv, ",", 2)
			val := ""
			if len(parts) == 2 {
				vt := strings.TrimSpace(parts[1])
				switch vt {
				case "int", "num":
					val = "0"
				case "bool", "bool?":
					val = "false"
				case "String":
					val = "\"\""
				default:
					if strings.HasPrefix(vt, "Map<") {
						val = "{}"
					} else if strings.HasSuffix(vt, "?") {
						val = "null"
					}
				}
			}
			if val == "" {
				if _, err := io.WriteString(w, "!"); err != nil {
					return err
				}
			} else {
				_, err := io.WriteString(w, " ?? "+val)
				if err != nil {
					return err
				}
				if needDefault {
					_, err = io.WriteString(w, ")")
					if err != nil {
						return err
					}
				}
				return nil
			}
		} else if !i.NoSuffix {
			_, err := io.WriteString(w, "!")
			return err
		}
	}
	if needDefault {
		_, err := io.WriteString(w, ")")
		return err
	}
	return nil
}

// SubstringExpr represents substring(s, start, end).
type SubstringExpr struct {
	Str   Expr
	Start Expr
	End   Expr
}

func (s *SubstringExpr) emit(w io.Writer) error {
	useSubstrClamp = true
	if _, err := io.WriteString(w, "_substr("); err != nil {
		return err
	}
	if err := s.Str.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ", "); err != nil {
		return err
	}
	if s.Start != nil {
		if err := s.Start.emit(w); err != nil {
			return err
		}
	} else {
		if _, err := io.WriteString(w, "0"); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, ", "); err != nil {
		return err
	}
	if s.End != nil {
		if err := s.End.emit(w); err != nil {
			return err
		}
	} else {
		if err := s.Str.emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ".length"); err != nil {
			return err
		}
	}
	_, err := io.WriteString(w, ")")
	return err
}

// ContainsExpr represents right.contains(left).
type ContainsExpr struct {
	Target Expr
	Elem   Expr
}

func (c *ContainsExpr) emit(w io.Writer) error {
	if err := c.Target.emit(w); err != nil {
		return err
	}
	method := ".contains("
	t := inferType(c.Target)
	if strings.HasPrefix(t, "Map<") {
		method = ".containsKey("
	} else if sel, ok := c.Target.(*SelectorExpr); ok {
		if fields, ok := structFields[inferType(sel.Receiver)]; ok {
			for _, f := range fields {
				if f.Name == sel.Field && strings.HasPrefix(f.Type, "Map<") {
					method = ".containsKey("
					break
				}
			}
		}
	}
	if _, err := io.WriteString(w, method); err != nil {
		return err
	}
	if err := c.Elem.emit(w); err != nil {
		return err
	}
	_, err := io.WriteString(w, ")")
	return err
}

// AppendExpr represents append(list, value).
type AppendExpr struct {
	List  Expr
	Value Expr
}

func (a *AppendExpr) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "[..."); err != nil {
		return err
	}
	if err := a.List.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ", "); err != nil {
		return err
	}
	if err := a.Value.emit(w); err != nil {
		return err
	}
	_, err := io.WriteString(w, "]")
	return err
}

// UnionExpr represents list union without duplicates.
type UnionExpr struct{ Left, Right Expr }

func (u *UnionExpr) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "Set.from([..."); err != nil {
		return err
	}
	if err := u.Left.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ", ..."); err != nil {
		return err
	}
	if err := u.Right.emit(w); err != nil {
		return err
	}
	_, err := io.WriteString(w, "]).toList()")
	return err
}

// UnionAllExpr represents concatenation of two lists.
type UnionAllExpr struct{ Left, Right Expr }

func (u *UnionAllExpr) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "["); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "..."); err != nil {
		return err
	}
	if err := u.Left.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ", ..."); err != nil {
		return err
	}
	if err := u.Right.emit(w); err != nil {
		return err
	}
	_, err := io.WriteString(w, "]")
	return err
}

// ExceptExpr represents elements in Left not in Right.
type ExceptExpr struct{ Left, Right Expr }

func (e *ExceptExpr) emit(w io.Writer) error {
	if err := e.Left.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ".where((x) => !"); err != nil {
		return err
	}
	if err := e.Right.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ".contains(x)).toList()"); err != nil {
		return err
	}
	return nil
}

// IntersectExpr represents intersection of two lists.
type IntersectExpr struct{ Left, Right Expr }

func (i *IntersectExpr) emit(w io.Writer) error {
	if err := i.Left.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ".where((x) => "); err != nil {
		return err
	}
	if err := i.Right.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ".contains(x)).toList()"); err != nil {
		return err
	}
	return nil
}

// AvgExpr represents avg(list).
type AvgExpr struct{ List Expr }

func (a *AvgExpr) emit(w io.Writer) error {
	if err := a.List.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ".isEmpty ? 0 : ("); err != nil {
		return err
	}
	if err := a.List.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ".fold(0, (a, b) => a + b) / "); err != nil {
		return err
	}
	if err := a.List.emit(w); err != nil {
		return err
	}
	_, err := io.WriteString(w, ".length)")
	return err
}

// SumExpr represents sum(list).
type SumExpr struct{ List Expr }

func (s *SumExpr) emit(w io.Writer) error {
	if err := s.List.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ".fold(0, (a, b) => a + b)"); err != nil {
		return err
	}
	return nil
}

// MinExpr represents min(list).
type MinExpr struct{ List Expr }

func (m *MinExpr) emit(w io.Writer) error {
	if err := m.List.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ".reduce((a, b) => a < b ? a : b)"); err != nil {
		return err
	}
	return nil
}

// MaxExpr represents max(list).
type MaxExpr struct{ List Expr }

func (m *MaxExpr) emit(w io.Writer) error {
	if err := m.List.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ".reduce((a, b) => a > b ? a : b)"); err != nil {
		return err
	}
	return nil
}

// ValuesExpr represents values(map).
type ValuesExpr struct{ Map Expr }

func (v *ValuesExpr) emit(w io.Writer) error {
	t := inferType(v.Map)
	if fields, ok := structFields[t]; ok {
		if _, err := io.WriteString(w, "["); err != nil {
			return err
		}
		for i, f := range fields {
			if i > 0 {
				if _, err := io.WriteString(w, ", "); err != nil {
					return err
				}
			}
			if err := (&SelectorExpr{Receiver: v.Map, Field: f.Name}).emit(w); err != nil {
				return err
			}
		}
		_, err := io.WriteString(w, "]")
		return err
	}
	if err := v.Map.emit(w); err != nil {
		return err
	}
	_, err := io.WriteString(w, ".values.toList()")
	return err
}

// StrExpr represents str(x).
type StrExpr struct{ Value Expr }

func (s *StrExpr) emit(w io.Writer) error {
	useStr = true
	if _, err := io.WriteString(w, "_str("); err != nil {
		return err
	}
	if err := s.Value.emit(w); err != nil {
		return err
	}
	_, err := io.WriteString(w, ")")
	return err
}

// FormatList renders a list like "[a b]".
type FormatList struct{ List Expr }

func (f *FormatList) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "\"[\" + "); err != nil {
		return err
	}
	if err := f.List.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ".join(', ') + \"]\""); err != nil {
		return err
	}
	return nil
}

// NowExpr returns a deterministic timestamp similar to the VM's now() builtin.
type NowExpr struct{}

func (n *NowExpr) emit(w io.Writer) error {
	_, err := io.WriteString(w, "_now()")
	return err
}

// InputExpr represents a call to input() returning a line from stdin.
type InputExpr struct{}

func (in *InputExpr) emit(w io.Writer) error {
	_, err := io.WriteString(w, "stdin.readLineSync() ?? ''")
	return err
}

func isMaybeString(e Expr) bool {
	switch ex := e.(type) {
	case *StringLit:
		return true
	case *Name:
		if t, ok := localVarTypes[ex.Name]; ok {
			if t == "String" || t == "dynamic" {
				return true
			}
		}
		if currentEnv != nil {
			if vt, err := currentEnv.GetVar(ex.Name); err == nil {
				if dt := dartType(vt); dt == "String" || dt == "dynamic" {
					return true
				}
			}
		}
	case *IndexExpr:
		if isMaybeString(ex.Target) {
			return true
		}
	case *CallExpr:
		if n, ok := ex.Func.(*Name); ok {
			switch n.Name {
			case "upper", "lower", "trim", "substr", "substring", "padStart", "repeat", "to_json", "str", "slice":
				return true
			}
		}
		if sel, ok := ex.Func.(*SelectorExpr); ok {
			switch sel.Field {
			case "toUpperCase", "toLowerCase", "substring", "substr", "padLeft", "padRight":
				return true
			}
		}
	}
	return false
}

// CastExpr represents value type conversions like `x as int`.
type CastExpr struct {
	Value Expr
	Type  string
}

func (c *CastExpr) emit(w io.Writer) error {
	valType := inferType(c.Value)
	if c.Type == "double" && valType == "int" {
		if err := c.Value.emit(w); err != nil {
			return err
		}
		_, err := io.WriteString(w, ".toDouble()")
		return err
	}
	if c.Type == "BigInt" {
		if lit, ok := c.Value.(*IntLit); ok {
			_, err := fmt.Fprintf(w, "BigInt.from(%d)", lit.Value)
			return err
		}
		if valType == "int" {
			_, err := io.WriteString(w, "BigInt.from(")
			if err != nil {
				return err
			}
			if strings.HasPrefix(c.Type, "List<") {
				if _, err := io.WriteString(w, c.Type+".from("); err != nil {
					return err
				}
				if err := c.Value.emit(w); err != nil {
					return err
				}
				_, err := io.WriteString(w, ")")
				return err
			}

			if err := c.Value.emit(w); err != nil {
				return err
			}
			_, err = io.WriteString(w, ")")
			return err
		}
		if valType == "BigInt" {
			return c.Value.emit(w)
		}
	}

	if c.Type == "int" && valType == "BigInt" {
		if _, err := io.WriteString(w, "("); err != nil {
			return err
		}
		if err := c.Value.emit(w); err != nil {
			return err
		}
		_, err := io.WriteString(w, ").toInt()")
		return err
	}

	if c.Type == "int" && (valType == "num" || valType == "double" || valType == "dynamic") {
		if _, err := io.WriteString(w, "("); err != nil {
			return err
		}
		if err := c.Value.emit(w); err != nil {
			return err
		}
		_, err := io.WriteString(w, ").toInt()")
		return err
	}

	if c.Type == "int" && valType == "String" {
		if _, err := io.WriteString(w, "("); err != nil {
			return err
		}
		if err := c.Value.emit(w); err != nil {
			return err
		}
		_, err := io.WriteString(w, ").codeUnitAt(0)")
		return err
	}

	if c.Type == "num" || c.Type == "double" {
		if valType == "num" || valType == "double" {
			return c.Value.emit(w)
		}
		if _, err := io.WriteString(w, "("); err != nil {
			return err
		}
		if err := c.Value.emit(w); err != nil {
			return err
		}
		_, err := io.WriteString(w, ")")
		if err != nil {
			return err
		}
		_, err = io.WriteString(w, ".toDouble()")
		return err
	}
	if strings.HasPrefix(c.Type, "List<") && valType != c.Type {
		return emitListConversion(w, c.Value, c.Type)
	}
	if err := c.Value.emit(w); err != nil {
		return err
	}
	switch c.Type {
	case "int":
		_, err := io.WriteString(w, " as int")
		return err
	case "num", "double":
		if valType != "num" && valType != "double" {
			_, err := io.WriteString(w, ".toDouble()")
			return err
		}
		_, err := io.WriteString(w, " as "+c.Type)
		return err
	case "String":
		_, err := io.WriteString(w, " as String")
		return err
	case "bool":
		_, err := io.WriteString(w, " as bool")
		return err
	case "BigInt":
		_, err := io.WriteString(w, " as BigInt")
		return err
	default:
		_, err := io.WriteString(w, " as "+c.Type)
		return err
	}
}

// CountExpr represents count(list) or count(group).
type CountExpr struct{ X Expr }

func (c *CountExpr) emit(w io.Writer) error {
	if err := c.X.emit(w); err != nil {
		return err
	}
	_, err := io.WriteString(w, ".length")
	return err
}

// MultiListComp represents a list comprehension with multiple input iterators.
type MultiListComp struct {
	Vars  []string
	Iters []Expr
	Expr  Expr
	Cond  Expr
}

// GroupQueryExpr represents a query with grouping support.
type GroupQueryExpr struct {
	Vars     []string
	Iters    []Expr
	Cond     Expr
	Key      Expr
	Row      Expr
	GroupVar string
	Select   Expr
	Having   Expr
	Sort     *LambdaExpr
	ElemType string
}

// GroupLeftJoinExpr represents a group by query that includes a left join.
type GroupLeftJoinExpr struct {
	LeftVar  string
	LeftSrc  Expr
	RightVar string
	RightSrc Expr
	Cond     Expr
	Key      Expr
	Row      Expr
	GroupVar string
	Select   Expr
	Having   Expr
	Sort     *LambdaExpr
	ElemType string
}

func groupIter(e Expr) Expr {
	if n, ok := e.(*Name); ok && currentEnv != nil {
		if t, err := currentEnv.GetVar(n.Name); err == nil {
			if _, ok := t.(types.GroupType); ok {
				return &SelectorExpr{Receiver: e, Field: "items"}
			}
		}
	}
	return e
}

// LeftJoinExpr represents a simple left join query.
type LeftJoinExpr struct {
	LeftVar  string
	LeftSrc  Expr
	RightVar string
	RightSrc Expr
	Cond     Expr
	Select   Expr
}

// LeftJoinMultiExpr handles a join followed by a left join.
type LeftJoinMultiExpr struct {
	Var1   string
	Src1   Expr
	Var2   string
	Src2   Expr
	Cond2  Expr
	Var3   string
	Src3   Expr
	Cond3  Expr
	Select Expr
}

func (l *LeftJoinExpr) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "(() {\n"); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "  final results = [];\n"); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "  for (var "+l.LeftVar+" in "); err != nil {
		return err
	}
	if err := l.LeftSrc.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ") {\n    var matched = false;\n    for (var "+l.RightVar+" in "); err != nil {
		return err
	}
	if err := l.RightSrc.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ") {\n      if (!("); err != nil {
		return err
	}
	if err := l.Cond.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ")) continue;\n      matched = true;\n      results.add("); err != nil {
		return err
	}
	if err := l.Select.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ");\n    }\n    if (!matched) {\n      var "+l.RightVar+" = null;\n      results.add("); err != nil {
		return err
	}
	if err := l.Select.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ");\n    }\n  }\n  return results;\n})()"); err != nil {
		return err
	}
	return nil
}

func (l *LeftJoinMultiExpr) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "(() {\n"); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "  final results = [];\n"); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "  for (var "+l.Var1+" in "); err != nil {
		return err
	}
	if err := l.Src1.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ") {\n    for (var "+l.Var2+" in "); err != nil {
		return err
	}
	if err := l.Src2.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ") {\n      if (!("); err != nil {
		return err
	}
	if err := l.Cond2.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ")) continue;\n      var matched = false;\n      for (var "+l.Var3+" in "); err != nil {
		return err
	}
	if err := l.Src3.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ") {\n        if (!("); err != nil {
		return err
	}
	if err := l.Cond3.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ")) continue;\n        matched = true;\n        results.add("); err != nil {
		return err
	}
	if err := l.Select.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ");\n      }\n      if (!matched) {\n        var "+l.Var3+" = null;\n        results.add("); err != nil {
		return err
	}
	if err := l.Select.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ");\n      }\n    }\n  }\n  return results;\n})();"); err != nil {
		return err
	}
	return nil
}

// RightJoinExpr represents a simple right join query.
type RightJoinExpr struct {
	LeftVar  string
	LeftSrc  Expr
	RightVar string
	RightSrc Expr
	Cond     Expr
	Select   Expr
}

func (r *RightJoinExpr) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "(() {\n"); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "  final results = [];\n"); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "  for (var "+r.RightVar+" in "); err != nil {
		return err
	}
	if err := r.RightSrc.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ") {\n    var matched = false;\n    for (var "+r.LeftVar+" in "); err != nil {
		return err
	}
	if err := r.LeftSrc.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ") {\n      if (!("); err != nil {
		return err
	}
	if err := r.Cond.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ")) continue;\n      matched = true;\n      results.add("); err != nil {
		return err
	}
	if err := r.Select.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ");\n    }\n    if (!matched) {\n      var "+r.LeftVar+" = null;\n      results.add("); err != nil {
		return err
	}
	if err := r.Select.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ");\n    }\n  }\n  return results;\n})()"); err != nil {
		return err
	}
	return nil
}

// OuterJoinExpr represents a simple full outer join query.
type OuterJoinExpr struct {
	LeftVar  string
	LeftSrc  Expr
	RightVar string
	RightSrc Expr
	Cond     Expr
	Select   Expr
}

func (o *OuterJoinExpr) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "(() {\n"); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "  final results = [];\n"); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "  for (var "+o.LeftVar+" in "); err != nil {
		return err
	}
	if err := o.LeftSrc.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ") {\n    var matched = false;\n    for (var "+o.RightVar+" in "); err != nil {
		return err
	}
	if err := o.RightSrc.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ") {\n      if (!("); err != nil {
		return err
	}
	if err := o.Cond.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ")) continue;\n      matched = true;\n      results.add("); err != nil {
		return err
	}
	if err := o.Select.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ");\n    }\n    if (!matched) {\n      var "+o.RightVar+" = null;\n      results.add("); err != nil {
		return err
	}
	if err := o.Select.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ");\n    }\n  }\n  for (var "+o.RightVar+" in "); err != nil {
		return err
	}
	if err := o.RightSrc.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ") {\n    var exists = false;\n    for (var "+o.LeftVar+" in "); err != nil {
		return err
	}
	if err := o.LeftSrc.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ") {\n      if ("); err != nil {
		return err
	}
	if err := o.Cond.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ") { exists = true; break; }\n    }\n    if (!exists) {\n      var "+o.LeftVar+" = null;\n      results.add("); err != nil {
		return err
	}
	if err := o.Select.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ");\n    }\n  }\n  return results;\n})()"); err != nil {
		return err
	}
	return nil
}

func (gq *GroupQueryExpr) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "(() {\n"); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "  final groups = <String, Map<String, dynamic>>{};\n"); err != nil {
		return err
	}
	for i, v := range gq.Vars {
		if _, err := io.WriteString(w, "  for (var "+v+" in "); err != nil {
			return err
		}
		if err := gq.Iters[i].emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ") {\n"); err != nil {
			return err
		}
	}
	if gq.Cond != nil {
		if _, err := io.WriteString(w, strings.Repeat("  ", len(gq.Vars)+1)+"if (!("); err != nil {
			return err
		}
		if err := gq.Cond.emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ")) continue;\n"); err != nil {
			return err
		}
	}
	indent := strings.Repeat("  ", len(gq.Vars)+1)
	if _, err := io.WriteString(w, indent+"var key = "); err != nil {
		return err
	}
	if err := gq.Key.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ";\n"+indent+"var ks = key.toString();\n"+indent+"var g = groups[ks];\n"+indent+"if (g == null) {\n"); err != nil {
		return err
	}
	nextStructHint = "group"
	if _, err := io.WriteString(w, indent+"  g = {'key': key, 'items': []};\n"+indent+"  groups[ks] = g;\n"+indent+"}\n"+indent+"(g['items'] as List).add("); err != nil {
		return err
	}
	nextStructHint = ""
	if err := gq.Row.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ");\n"); err != nil {
		return err
	}
	for i := len(gq.Vars); i > 0; i-- {
		if _, err := io.WriteString(w, strings.Repeat("  ", i)+"}\n"); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, "  final _list = groups.values.toList();\n"); err != nil {
		return err
	}
	if gq.Sort != nil {
		if _, err := io.WriteString(w, "  _list.sort("); err != nil {
			return err
		}
		if err := gq.Sort.emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ");\n"); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, "  final res = <"+gq.ElemType+">[];\n  for (var g in _list) {\n"); err != nil {
		return err
	}
	localVarTypes[gq.GroupVar] = "Map<String, dynamic>"
	if gq.Having != nil {
		if _, err := io.WriteString(w, "    if ("); err != nil {
			return err
		}
		if err := gq.Having.emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ") {\n      res.add("); err != nil {
			return err
		}
		if err := gq.Select.emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ");\n    }\n"); err != nil {
			return err
		}
	} else {
		if _, err := io.WriteString(w, "    res.add("); err != nil {
			return err
		}
		if err := gq.Select.emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ");\n"); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, "  }\n  return res;\n})();"); err != nil {
		return err
	}
	delete(localVarTypes, gq.GroupVar)
	return nil
}

func (g *GroupLeftJoinExpr) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "(() {\n"); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "  final groups = <String, Map<String, dynamic>>{};\n"); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "  for (var "+g.LeftVar+" in "); err != nil {
		return err
	}
	if err := g.LeftSrc.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ") {\n    var matched = false;\n    for (var "+g.RightVar+" in "); err != nil {
		return err
	}
	if err := g.RightSrc.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ") {\n      if (!("); err != nil {
		return err
	}
	if err := g.Cond.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ")) continue;\n      matched = true;\n"); err != nil {
		return err
	}
	if err := emitGroupAdd(w, "      ", g.Key, g.Row); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "    }\n    if (!matched) {\n      var "+g.RightVar+" = null;\n"); err != nil {
		return err
	}
	if err := emitGroupAdd(w, "      ", g.Key, g.Row); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "    }\n  }\n  final _list = groups.values.toList();\n"); err != nil {
		return err
	}
	if g.Sort != nil {
		if _, err := io.WriteString(w, "  _list.sort("); err != nil {
			return err
		}
		if err := g.Sort.emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ");\n"); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, "  final res = <"+g.ElemType+">[];\n  for (var "+g.GroupVar+" in _list) {\n"); err != nil {
		return err
	}
	localVarTypes[g.GroupVar] = "Map<String, dynamic>"
	if g.Having != nil {
		if _, err := io.WriteString(w, "    if ("); err != nil {
			return err
		}
		if err := g.Having.emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ") {\n      res.add("); err != nil {
			return err
		}
		if err := g.Select.emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ");\n    }\n"); err != nil {
			return err
		}
	} else {
		if _, err := io.WriteString(w, "    res.add("); err != nil {
			return err
		}
		if err := g.Select.emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ");\n"); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, "  }\n  return res;\n})();"); err != nil {
		return err
	}
	delete(localVarTypes, g.GroupVar)
	return nil
}

func emitGroupAdd(w io.Writer, indent string, key Expr, row Expr) error {
	if _, err := io.WriteString(w, indent+"var key = "); err != nil {
		return err
	}
	if err := key.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ";\n"+indent+"var ks = key.toString();\n"+indent+"var g = groups[ks];\n"+indent+"if (g == null) {\n"+indent+"  g = {'key': key, 'items': []};\n"+indent+"  groups[ks] = g;\n"+indent+"}\n"+indent+"(g['items'] as List).add("); err != nil {
		return err
	}
	if err := row.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ");\n"); err != nil {
		return err
	}
	return nil
}

func (lc *MultiListComp) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "[for (var "); err != nil {
		return err
	}
	if len(lc.Vars) > 0 {
		if _, err := io.WriteString(w, lc.Vars[0]+" in "); err != nil {
			return err
		}
		if err := lc.Iters[0].emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ")"); err != nil {
			return err
		}
	}
	for i := 1; i < len(lc.Vars); i++ {
		if _, err := io.WriteString(w, " for (var "+lc.Vars[i]+" in "); err != nil {
			return err
		}
		if err := lc.Iters[i].emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ")"); err != nil {
			return err
		}
	}
	if lc.Cond != nil {
		if _, err := io.WriteString(w, " if ("); err != nil {
			return err
		}
		if err := lc.Cond.emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ")"); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, " "); err != nil {
		return err
	}
	if err := lc.Expr.emit(w); err != nil {
		return err
	}
	_, err := io.WriteString(w, "]")
	return err
}

// SortExpr represents sorting of a list with a comparison lambda.
type SortExpr struct {
	List    Expr
	Compare *LambdaExpr
}

func (s *SortExpr) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "("); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "List.of("); err != nil {
		return err
	}
	if err := s.List.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ")..sort("); err != nil {
		return err
	}
	if err := s.Compare.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ")"); err != nil {
		return err
	}
	_, err := io.WriteString(w, ")")
	return err
}

// LambdaExpr represents an inline function expression.
type LambdaExpr struct {
	Params []string
	Body   Expr
}

// LambdaBlock represents a closure with a statement body.
type LambdaBlock struct {
	Params []string
	Body   []Stmt
}

func (l *LambdaBlock) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "("); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "("); err != nil {
		return err
	}
	for i, p := range l.Params {
		if i > 0 {
			if _, err := io.WriteString(w, ", "); err != nil {
				return err
			}
		}
		if _, err := io.WriteString(w, p); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, ") {\n"); err != nil {
		return err
	}
	for _, st := range l.Body {
		if _, err := io.WriteString(w, "  "); err != nil {
			return err
		}
		if err := st.emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ";\n"); err != nil {
			return err
		}
	}
	_, err := io.WriteString(w, "})")
	return err
}

func (l *LambdaExpr) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "("); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "("); err != nil {
		return err
	}
	for i, p := range l.Params {
		if i > 0 {
			if _, err := io.WriteString(w, ", "); err != nil {
				return err
			}
		}
		if _, err := io.WriteString(w, p); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, ") => "); err != nil {
		return err
	}
	if err := l.Body.emit(w); err != nil {
		return err
	}
	_, err := io.WriteString(w, ")")
	return err
}

// LenExpr represents the `len` builtin.
type LenExpr struct{ X Expr }

func (l *LenExpr) emit(w io.Writer) error {
	if err := l.X.emit(w); err != nil {
		return err
	}
	_, err := io.WriteString(w, ".length")
	return err
}

// inferType attempts to guess the Dart type for the given expression.
var currentEnv *types.Env

func dartType(t types.Type) string {
	switch v := t.(type) {
	case types.IntType, types.Int64Type:
		return "int"
	case types.BigIntType:
		return "BigInt"
	case types.FloatType:
		return "double"
	case types.BigRatType:
		useBigRat = true
		return "BigRat"
	case types.BoolType:
		return "bool"
	case types.StringType:
		return "String"
	case types.ListType:
		return "List<" + dartType(v.Elem) + ">"
	case types.MapType:
		return "Map<" + dartType(v.Key) + ", " + dartType(v.Value) + ">"
	case types.GroupType:
		return "Map<String, dynamic>"
	case types.StructType:
		if v.Name != "" {
			return v.Name
		}
		return "Map<String, dynamic>"
	default:
		return "dynamic"
	}
}

func typeRefString(tr *parser.TypeRef) string {
	if tr == nil {
		return ""
	}
	if currentEnv == nil {
		return ""
	}
	return dartType(types.ResolveTypeRef(tr, currentEnv))
}

func inferType(e Expr) string {
	switch ex := e.(type) {
	case *IntLit:
		return "int"
	case *FloatLit:
		return "double"
	case *BoolLit:
		return "bool"
	case *StringLit:
		return "String"
	case *CastExpr:
		return ex.Type
	case *Name:
		if t, ok := localVarTypes[ex.Name]; ok {
			return t
		}
		if currentEnv != nil {
			if t, err := currentEnv.GetVar(ex.Name); err == nil {
				return dartType(t)
			}
		}
		if t, ok := compVarTypes[ex.Name]; ok {
			return t
		}
		return "dynamic"
	case *ListLit:
		if len(ex.Elems) == 0 {
			if ex.ElemType != "" {
				return "List<" + ex.ElemType + ">"
			}
			return "List<dynamic>"
		}
		typ := ""
		for _, el := range ex.Elems {
			t := inferType(el)
			if t == "List<dynamic>" {
				if ll, ok := el.(*ListLit); ok && len(ll.Elems) == 0 {
					continue
				}
			}
			if typ == "" {
				typ = t
			} else if t != typ {
				typ = "dynamic"
				break
			}
		}
		if typ == "" {
			typ = "dynamic"
		}
		return "List<" + typ + ">"
	case *MapLit:
		if name, ok := mapLitStructName[ex]; ok {
			return name
		}
		if len(ex.Entries) == 0 {
			return "Map<dynamic, dynamic>"
		}
		sigParts := make([]string, len(ex.Entries))
		valid := true
		hasDynamic := false
		hasList := false
		for i, it := range ex.Entries {
			var field string
			switch k := it.Key.(type) {
			case *Name:
				if validIdent(k.Name) {
					field = k.Name
				} else {
					valid = false
				}
			case *StringLit:
				valid = false
			default:
				valid = false
			}
			if !valid {
				break
			}
			t := inferType(it.Value)
			if strings.HasPrefix(t, "List<") {
				hasList = true
			}
			if t == "dynamic" || len(structFields[t]) > 0 {
				hasDynamic = true
			}
			sigParts[i] = field + ":" + t
		}
		if valid && !hasDynamic && !hasList && nextStructHint != "" {
			sig := strings.Join(sigParts, ";")
			name, ok := structSig[sig]
			if !ok {
				if nextStructHint != "" {
					base := capitalize(nextStructHint)
					if strings.HasSuffix(base, "s") && len(base) > 1 {
						base = base[:len(base)-1]
					}
					cnt := structNameCount[base]
					if cnt > 0 {
						name = fmt.Sprintf("%s%d", base, cnt+1)
					} else {
						name = base
					}
					structNameCount[base] = cnt + 1
					nextStructHint = ""
				} else {
					structSeq++
					name = fmt.Sprintf("S%d", structSeq)
				}
				structSig[sig] = name
				var fields []StructField
				for _, part := range ex.Entries {
					var fn string
					switch k := part.Key.(type) {
					case *Name:
						fn = k.Name
					case *StringLit:
						fn = k.Value
					}
					fields = append(fields, StructField{Name: fn, Type: inferType(part.Value)})
				}
				structFields[name] = fields
				structOrder = append(structOrder, name)
			}
			mapLitStructName[ex] = name
			return name
		}

		kt := inferType(ex.Entries[0].Key)
		if kt == "dynamic" {
			if _, ok := ex.Entries[0].Key.(*Name); ok {
				kt = "String"
			}
		}
		vt := "dynamic"
		if len(ex.Entries) > 0 {
			vt = inferType(ex.Entries[0].Value)
		}
		for _, it := range ex.Entries[1:] {
			t := inferType(it.Key)
			if t == "dynamic" {
				if _, ok := it.Key.(*Name); ok {
					t = "String"
				}
			}
			if t != kt {
				kt = "dynamic"
			}
			if t := inferType(it.Value); t != vt {
				vt = "dynamic"
			}
		}
		if kt == "dynamic" {
			kt = "dynamic"
		}
		return "Map<" + kt + ", " + vt + ">"
	case *MultiListComp:
		saved := map[string]string{}
		for i, v := range ex.Vars {
			t := inferType(ex.Iters[i])
			elem := "dynamic"
			if strings.HasPrefix(t, "List<") && strings.HasSuffix(t, ">") {
				elem = strings.TrimSuffix(strings.TrimPrefix(t, "List<"), ">")
			}
			saved[v] = compVarTypes[v]
			compVarTypes[v] = elem
		}
		et := inferType(ex.Expr)
		if et == "dynamic" {
			et = "dynamic"
		}
		for _, v := range ex.Vars {
			if old, ok := saved[v]; ok && old != "" {
				compVarTypes[v] = old
			} else {
				delete(compVarTypes, v)
			}
		}
		return "List<" + et + ">"
	case *GroupQueryExpr:
		saved := map[string]string{}
		for i, v := range ex.Vars {
			t := inferType(ex.Iters[i])
			elem := "dynamic"
			if strings.HasPrefix(t, "List<") && strings.HasSuffix(t, ">") {
				elem = strings.TrimSuffix(strings.TrimPrefix(t, "List<"), ">")
			}
			saved[v] = compVarTypes[v]
			compVarTypes[v] = elem
		}
		if ex.GroupVar != "" {
			saved[ex.GroupVar] = compVarTypes[ex.GroupVar]
			compVarTypes[ex.GroupVar] = "Map<String, dynamic>"
		}
		et := inferType(ex.Select)
		for _, v := range ex.Vars {
			if old, ok := saved[v]; ok && old != "" {
				compVarTypes[v] = old
			} else {
				delete(compVarTypes, v)
			}
		}
		if ex.GroupVar != "" {
			if old, ok := saved[ex.GroupVar]; ok && old != "" {
				compVarTypes[ex.GroupVar] = old
			} else {
				delete(compVarTypes, ex.GroupVar)
			}
		}
		return "List<" + et + ">"
	case *GroupLeftJoinExpr:
		saved := map[string]string{}
		ltype := inferType(ex.LeftSrc)
		rtype := inferType(ex.RightSrc)
		lelem := "dynamic"
		relem := "dynamic"
		if strings.HasPrefix(ltype, "List<") && strings.HasSuffix(ltype, ">") {
			lelem = strings.TrimSuffix(strings.TrimPrefix(ltype, "List<"), ">")
		}
		if strings.HasPrefix(rtype, "List<") && strings.HasSuffix(rtype, ">") {
			relem = strings.TrimSuffix(strings.TrimPrefix(rtype, "List<"), ">")
		}
		saved[ex.LeftVar] = compVarTypes[ex.LeftVar]
		saved[ex.RightVar] = compVarTypes[ex.RightVar]
		compVarTypes[ex.LeftVar] = lelem
		compVarTypes[ex.RightVar] = relem
		saved[ex.GroupVar] = compVarTypes[ex.GroupVar]
		compVarTypes[ex.GroupVar] = "Map<String, dynamic>"
		et := inferType(ex.Select)
		if old := saved[ex.LeftVar]; old != "" {
			compVarTypes[ex.LeftVar] = old
		} else {
			delete(compVarTypes, ex.LeftVar)
		}
		if old := saved[ex.RightVar]; old != "" {
			compVarTypes[ex.RightVar] = old
		} else {
			delete(compVarTypes, ex.RightVar)
		}
		if old := saved[ex.GroupVar]; old != "" {
			compVarTypes[ex.GroupVar] = old
		} else {
			delete(compVarTypes, ex.GroupVar)
		}
		return "List<" + et + ">"
	case *LeftJoinExpr:
		ltype := inferType(ex.LeftSrc)
		rtype := inferType(ex.RightSrc)
		lelem := "dynamic"
		relem := "dynamic"
		if strings.HasPrefix(ltype, "List<") && strings.HasSuffix(ltype, ">") {
			lelem = strings.TrimSuffix(strings.TrimPrefix(ltype, "List<"), ">")
		}
		if strings.HasPrefix(rtype, "List<") && strings.HasSuffix(rtype, ">") {
			relem = strings.TrimSuffix(strings.TrimPrefix(rtype, "List<"), ">")
		}
		savedL, savedR := compVarTypes[ex.LeftVar], compVarTypes[ex.RightVar]
		compVarTypes[ex.LeftVar] = lelem
		compVarTypes[ex.RightVar] = relem
		et := inferType(ex.Select)
		if savedL != "" {
			compVarTypes[ex.LeftVar] = savedL
		} else {
			delete(compVarTypes, ex.LeftVar)
		}
		if savedR != "" {
			compVarTypes[ex.RightVar] = savedR
		} else {
			delete(compVarTypes, ex.RightVar)
		}
		return "List<" + et + ">"
	case *RightJoinExpr:
		ltype := inferType(ex.LeftSrc)
		rtype := inferType(ex.RightSrc)
		lelem := "dynamic"
		relem := "dynamic"
		if strings.HasPrefix(ltype, "List<") && strings.HasSuffix(ltype, ">") {
			lelem = strings.TrimSuffix(strings.TrimPrefix(ltype, "List<"), ">")
		}
		if strings.HasPrefix(rtype, "List<") && strings.HasSuffix(rtype, ">") {
			relem = strings.TrimSuffix(strings.TrimPrefix(rtype, "List<"), ">")
		}
		savedL, savedR := compVarTypes[ex.LeftVar], compVarTypes[ex.RightVar]
		compVarTypes[ex.LeftVar] = lelem
		compVarTypes[ex.RightVar] = relem
		et := inferType(ex.Select)
		if savedL != "" {
			compVarTypes[ex.LeftVar] = savedL
		} else {
			delete(compVarTypes, ex.LeftVar)
		}
		if savedR != "" {
			compVarTypes[ex.RightVar] = savedR
		} else {
			delete(compVarTypes, ex.RightVar)
		}
		return "List<" + et + ">"
	case *OuterJoinExpr:
		ltype := inferType(ex.LeftSrc)
		rtype := inferType(ex.RightSrc)
		lelem := "dynamic"
		relem := "dynamic"
		if strings.HasPrefix(ltype, "List<") && strings.HasSuffix(ltype, ">") {
			lelem = strings.TrimSuffix(strings.TrimPrefix(ltype, "List<"), ">")
		}
		if strings.HasPrefix(rtype, "List<") && strings.HasSuffix(rtype, ">") {
			relem = strings.TrimSuffix(strings.TrimPrefix(rtype, "List<"), ">")
		}
		savedL, savedR := compVarTypes[ex.LeftVar], compVarTypes[ex.RightVar]
		compVarTypes[ex.LeftVar] = lelem
		compVarTypes[ex.RightVar] = relem
		et := inferType(ex.Select)
		if savedL != "" {
			compVarTypes[ex.LeftVar] = savedL
		} else {
			delete(compVarTypes, ex.LeftVar)
		}
		if savedR != "" {
			compVarTypes[ex.RightVar] = savedR
		} else {
			delete(compVarTypes, ex.RightVar)
		}
		return "List<" + et + ">"
	case *SortExpr:
		return inferType(ex.List)
	case *BinaryExpr:
		switch ex.Op {
		case "+":
			lt := inferType(ex.Left)
			rt := inferType(ex.Right)
			if lt == "String" || rt == "String" {
				return "String"
			}
			if lt == "BigInt" || rt == "BigInt" {
				return "BigInt"
			}
			if lt == "int" && rt == "int" {
				return "int"
			}
			if lt == "double" || rt == "double" || lt == "num" || rt == "num" {
				return "double"
			}
			return "num"
		case "-", "*", "%":
			lt := inferType(ex.Left)
			rt := inferType(ex.Right)
			if lt == "BigInt" || rt == "BigInt" {
				return "BigInt"
			}
			if lt == "int" && rt == "int" {
				return "int"
			}
			if lt == "double" || rt == "double" || lt == "num" || rt == "num" {
				return "double"
			}
			return "num"
		case "/":
			lt := inferType(ex.Left)
			rt := inferType(ex.Right)
			if lt == "BigInt" || rt == "BigInt" {
				return "BigInt"
			}
			if lt == "int" && rt == "int" {
				return "int"
			}
			if lt == "double" || rt == "double" || lt == "num" || rt == "num" {
				return "double"
			}
			return "num"
		case "<", "<=", ">", ">=", "==", "!=", "&&", "||":
			return "bool"
		case "union", "union_all", "except", "intersect":
			return inferType(ex.Left)
		default:
			return "dynamic"
		}
	case *UnaryExpr:
		if ex.Op == "-" {
			t := inferType(ex.X)
			if t == "int" || t == "num" {
				return t
			}
			return "num"
		}
		return inferType(ex.X)
	case *CondExpr:
		t1 := inferType(ex.Then)
		t2 := inferType(ex.Else)
		if t1 == t2 {
			return t1
		}
		return "dynamic"
	case *CallExpr:
		if n, ok := ex.Func.(*Name); ok {
			switch n.Name {
			case "len":
				return "int"
			case "avg":
				return "num"
			case "sum":
				return "num"
			case "upper", "lower", "trim", "substr", "substring", "padStart", "repeat", "to_json", "str":
				return "String"
			case "slice":
				if len(ex.Args) > 0 {
					return inferType(ex.Args[0])
				}
				return "List<dynamic>"
			case "split":
				return "List<String>"
			case "indexOf":
				return "int"
			case "min", "max":
				return "num"
			case "values":
				return "List<dynamic>"
			case "append", "concat":
				if len(ex.Args) > 0 {
					return inferType(ex.Args[0])
				}
			case "json":
				return "void"
			default:
				if rt, ok := funcReturnTypes[n.Name]; ok {
					return rt
				}
				if currentEnv != nil {
					if t, err := currentEnv.GetVar(n.Name); err == nil {
						if ft, ok := t.(types.FuncType); ok {
							return dartType(ft.Return)
						}
					}
				}
			}
		}
		if n, ok := ex.Func.(*Name); ok {
			switch n.Name {
			case "_bigrat", "_add", "_sub", "_mul", "_div":
				return "BigRat"
			case "_num", "_denom":
				return "BigInt"
			}
		}
		if sel, ok := ex.Func.(*SelectorExpr); ok {
			rt := inferType(sel.Receiver)
			if sel.Field == "abs" && (rt == "int" || rt == "num") {
				return "num"
			}
			if sel.Field == "toDouble" {
				return "double"
			}
			if sel.Field == "toUpperCase" || sel.Field == "toLowerCase" || sel.Field == "substring" || sel.Field == "substr" || sel.Field == "padLeft" || sel.Field == "padRight" {
				return "String"
			}
			if sel.Field == "parse" {
				if n, ok := sel.Receiver.(*Name); ok && n.Name == "int" {
					return "int"
				}
			}
			if sel.Field == "indexOf" {
				return "int"
			}
		}
		return "dynamic"
	case *IndexExpr:
		t := inferType(ex.Target)
		if t == "String" {
			return "String"
		}
		if strings.HasPrefix(t, "List<") && strings.HasSuffix(t, ">") {
			return strings.TrimSuffix(strings.TrimPrefix(t, "List<"), ">")
		}
		if strings.HasPrefix(t, "Map<") && strings.HasSuffix(t, ">") {
			parts := strings.TrimSuffix(strings.TrimPrefix(t, "Map<"), ">")
			if idx := strings.Index(parts, ","); idx >= 0 {
				val := strings.TrimSpace(parts[idx+1:])
				if !ex.NoBang {
					return val
				}
				return val + "?"
			}
		}
		return "dynamic"
	case *SelectorExpr:
		rt := inferType(ex.Receiver)
		if fields, ok := structFields[rt]; ok {
			for _, f := range fields {
				if f.Name == ex.Field {
					typ := f.Type
					if structMutable[rt] && !strings.HasSuffix(typ, "?") {
						typ += "?"
					}
					return typ
				}
			}
		}
		if ex.Field == "abs" && (rt == "int" || rt == "num") {
			return "num"
		}
		return "dynamic"
	case *ContainsExpr:
		return "bool"
	case *LenExpr:
		return "int"
	case *CountExpr:
		return "int"
	case *SubstringExpr:
		return "String"
	case *SliceExpr:
		t := inferType(ex.Target)
		if t == "String" {
			return "String"
		}
		return t
	case *AppendExpr:
		lt := inferType(ex.List)
		if strings.HasPrefix(lt, "List<") && strings.HasSuffix(lt, ">") {
			elem := strings.TrimSuffix(strings.TrimPrefix(lt, "List<"), ">")
			vt := inferType(ex.Value)
			if strings.HasPrefix(vt, "List<") && strings.HasSuffix(vt, ">") {
				vElem := strings.TrimSuffix(strings.TrimPrefix(vt, "List<"), ">")
				if vElem == elem || vElem == "dynamic" {
					return lt
				}
			} else if vt == elem || vt == "dynamic" {
				return lt
			}
			return "List<dynamic>"
		}
		return "List<dynamic>"
	case *AvgExpr, *SumExpr, *MinExpr, *MaxExpr:
		return "num"
	case *NowExpr:
		return "int"
	case *InputExpr:
		return "String"
	case *ValuesExpr:
		return "List<dynamic>"
	case *NotNilExpr:
		return strings.TrimSuffix(inferType(ex.X), "?")
	case *StrExpr, *FormatList:
		return "String"
	default:
		if e == nil {
			return "dynamic"
		}
		return "dynamic"
	}
}

func inferReturnType(body []Stmt) string {
	if len(body) == 0 {
		return "void"
	}
	if ret, ok := body[len(body)-1].(*ReturnStmt); ok {
		if ret.Value == nil {
			return "void"
		}
		t := inferType(ret.Value)
		if t == "dynamic" {
			return "dynamic"
		}
		return t
	}
	return "void"
}

func containsReturnNull(body []Stmt) bool {
	for _, st := range body {
		switch s := st.(type) {
		case *ReturnStmt:
			if n, ok := s.Value.(*Name); ok && n.Name == "null" {
				return true
			}
		case *IfStmt:
			if containsReturnNull(s.Then) || containsReturnNull(s.Else) {
				return true
			}
		case *WhileStmt:
			if containsReturnNull(s.Body) {
				return true
			}
		case *ForRangeStmt:
			if containsReturnNull(s.Body) {
				return true
			}
		case *ForInStmt:
			if containsReturnNull(s.Body) {
				return true
			}
		}
	}
	return false
}

func emitExpr(w io.Writer, e Expr) error { return e.emit(w) }

func isBlockStmt(s Stmt) bool {
	switch s.(type) {
	case *IfStmt, *WhileStmt, *ForRangeStmt, *ForInStmt, *FuncDecl, *UpdateStmt, *SaveStmt, *BenchStmt:
		return true
	default:
		return false
	}
}

func DebugGatherTypes(p *Program) {
	for _, st := range p.Stmts {
		walkTypes(st)
	}
}

func walkTypes(s Stmt) {
	switch st := s.(type) {
	case *LetStmt:
		nextStructHint = st.Name
		typ := st.Type
		if typ == "" {
			typ = inferType(st.Value)
		}
		nextStructHint = ""
		localVarTypes[st.Name] = typ
	case *VarStmt:
		nextStructHint = st.Name
		typ := st.Type
		if strings.Contains(typ, "Map<") {
			nextStructHint = ""
		}
		if st.Value != nil {
			valType := inferType(st.Value)
			if typ == "" || typ == "dynamic" {
				typ = valType
			}
		}
		if typ == "" {
			typ = "dynamic"
		}
		nextStructHint = ""
		localVarTypes[st.Name] = typ
	case *AssignStmt:
		inferType(st.Value)
		inferType(st.Target)
	case *ExprStmt:
		inferType(st.Expr)
	case *IfStmt:
		inferType(st.Cond)
		for _, t := range st.Then {
			walkTypes(t)
		}
		for _, e := range st.Else {
			walkTypes(e)
		}
	case *WhileStmt:
		inferType(st.Cond)
		for _, b := range st.Body {
			walkTypes(b)
		}
	case *ForRangeStmt:
		if st.Start != nil {
			inferType(st.Start)
		}
		if st.End != nil {
			inferType(st.End)
		}
		// range loops use integer counters
		localVarTypes[st.Name] = "int"
		for _, b := range st.Body {
			walkTypes(b)
		}
	case *ForInStmt:
		t := inferType(st.Iterable)
		elem := "dynamic"
		if strings.HasPrefix(t, "List<") && strings.HasSuffix(t, ">") {
			elem = strings.TrimSuffix(strings.TrimPrefix(t, "List<"), ">")
		}
		savedComp := compVarTypes[st.Name]
		compVarTypes[st.Name] = elem
		savedLocal := localVarTypes[st.Name]
		localVarTypes[st.Name] = elem
		for _, b := range st.Body {
			walkTypes(b)
		}
		if savedComp != "" {
			compVarTypes[st.Name] = savedComp
		} else {
			delete(compVarTypes, st.Name)
		}
		if savedLocal != "" {
			localVarTypes[st.Name] = savedLocal
		} else {
			delete(localVarTypes, st.Name)
		}
	case *UpdateStmt:
		if st.Cond != nil {
			inferType(st.Cond)
		}
		for _, v := range st.Values {
			inferType(v)
		}
	case *SaveStmt:
		inferType(st.Src)
	case *ReturnStmt:
		if st.Value != nil {
			inferType(st.Value)
		}
	case *FuncDecl:
		saved := map[string]string{}
		for n, t := range st.ParamTypes {
			if old, ok := localVarTypes[n]; ok {
				saved[n] = old
			}
			localVarTypes[n] = t
		}
		for _, b := range st.Body {
			walkTypes(b)
		}
		for n := range st.ParamTypes {
			if v, ok := saved[n]; ok {
				localVarTypes[n] = v
			} else {
				delete(localVarTypes, n)
			}
		}
		rt := funcReturnTypes[st.Name]
		if rt == "" {
			rt = inferReturnType(st.Body)
		}
		if containsReturnNull(st.Body) && rt != "void" && rt != "dynamic" && !strings.HasSuffix(rt, "?") {
			rt += "?"
		}
		funcReturnTypes[st.Name] = rt
	case *BenchStmt:
		for _, b := range st.Body {
			walkTypes(b)
		}
	}
}

// Emit writes Dart source for p to w.
func Emit(w io.Writer, p *Program) error {
	DebugGatherTypes(p)
	if p.BenchMain {
		usesJSON = true
		useInput = true
	}
	if _, err := io.WriteString(w, "// Generated by Mochi transpiler\n"); err != nil {
		return err
	}

	added := map[string]bool{}
	for _, imp := range p.Imports {
		if _, err := io.WriteString(w, imp+"\n"); err != nil {
			return err
		}
		parts := strings.Fields(strings.TrimSpace(imp))
		if len(parts) > 1 && strings.HasPrefix(parts[0], "import") {
			path := strings.Trim(parts[1], "'")
			added[path] = true
		}
	}
	if (usesJSON || useFetch || useSHA256) && !added["dart:convert"] {
		if _, err := io.WriteString(w, "import 'dart:convert';\n"); err != nil {
			return err
		}
	}
	if (useNow || useInput || useSHA256 || useEnv || useFetch || useMD5) && !added["dart:io"] {
		if _, err := io.WriteString(w, "import 'dart:io';\n"); err != nil {
			return err
		}
	}
	if len(p.Imports) > 0 || usesJSON || useNow || useInput || useSHA256 || useEnv || useFetch || useMD5 {
		if _, err := io.WriteString(w, "\n"); err != nil {
			return err
		}
	}
	if useNow {
		if _, err := io.WriteString(w, "int _nowSeed = 0;\nbool _nowSeeded = false;\n"); err != nil {
			return err
		}
		if _, err := io.WriteString(w, "void _initNow() {\n  var s = Platform.environment['MOCHI_NOW_SEED'];\n  if (s != null && s.isNotEmpty) {\n    var v = int.tryParse(s);\n    if (v != null) {\n      _nowSeed = v;\n      _nowSeeded = true;\n    }\n  }\n}\n"); err != nil {
			return err
		}
		if _, err := io.WriteString(w, "int _now() {\n  if (_nowSeeded) {\n    _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647;\n    return _nowSeed;\n  }\n  return DateTime.now().microsecondsSinceEpoch;\n}\n\n"); err != nil {
			return err
		}
	}
	if useLookupHost {
		if _, err := io.WriteString(w, "List _lookupHost(String host) {\n  return [<String>[], null];\n}\n\n"); err != nil {
			return err
		}
	}
	if useEnv {
		if _, err := io.WriteString(w, "String _getenv(String k) => Platform.environment[k] ?? '';\n"); err != nil {
			return err
		}
		if _, err := io.WriteString(w, "List<String> _environ() => Platform.environment.entries.map((e) => '${e.key}=${e.value}').toList();\n\n"); err != nil {
			return err
		}
	}
	if useSubprocess {
		if _, err := io.WriteString(w, "class _Subprocess {\n  String getoutput(String cmd) {\n    final r = Process.runSync('sh', ['-c', cmd]);\n    if (r.stdout is String) {\n      return (r.stdout as String).trim();\n    }\n    return '';\n  }\n}\n"); err != nil {
			return err
		}
		keys := make([]string, 0, len(subprocessAliases))
		for k := range subprocessAliases {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		for _, k := range keys {
			if _, err := fmt.Fprintf(w, "final %s = _Subprocess();\n", k); err != nil {
				return err
			}
		}
		if _, err := io.WriteString(w, "\n"); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, "dynamic _substr(dynamic s, num start, num end) {\n  int n = s.length;\n  int s0 = start.toInt();\n  int e0 = end.toInt();\n  if (s0 < 0) s0 += n;\n  if (e0 < 0) e0 += n;\n  if (s0 < 0) s0 = 0;\n  if (s0 > n) s0 = n;\n  if (e0 < 0) e0 = 0;\n  if (e0 > n) e0 = n;\n  if (s0 > e0) s0 = e0;\n  if (s is String) {\n    return s.substring(s0, e0);\n  }\n  return s.sublist(s0, e0);\n}\n\n"); err != nil {
		return err
	}
	if useRepeat {
		if _, err := io.WriteString(w, "String _repeat(String s, int n) => n <= 0 ? '' : List.filled(n, s).join();\n\n"); err != nil {
			return err
		}
	}
	if useParseIntStr {
		if _, err := io.WriteString(w, "int _parseIntStr(String s, int r) => int.parse(s, radix: r);\n\n"); err != nil {
			return err
		}
	}
	if useStr {
		if _, err := io.WriteString(w, "String _str(dynamic v) {"+
			" if (v is double && v == v.roundToDouble()) {"+
			" var i = v.toInt();"+
			" if (i == 0) return '0';"+
			" return i.toString();"+
			" }"+
			" return v.toString();"+
			" }\n\n"); err != nil {
			return err
		}
	}
	if useBigRat {
		if _, err := io.WriteString(w, "class BigRat {\n  BigInt num;\n  BigInt den;\n  BigRat(this.num, [BigInt? d]) : den = d ?? BigInt.one {\n    if (den.isNegative) { num = -num; den = -den; }\n    var g = num.gcd(den);\n    num = num ~/ g;\n    den = den ~/ g;\n  }\n  BigRat add(BigRat o) => BigRat(num * o.den + o.num * den, den * o.den);\n  BigRat sub(BigRat o) => BigRat(num * o.den - o.num * den, den * o.den);\n  BigRat mul(BigRat o) => BigRat(num * o.num, den * o.den);\n  BigRat div(BigRat o) => BigRat(num * o.den, den * o.num);\n}\n\nBigRat _bigrat(dynamic n, [dynamic d]) {\n  if (n is BigRat && d == null) return BigRat(n.num, n.den);\n  BigInt numer;\n  BigInt denom = d == null ? BigInt.one : (d is BigInt ? d : BigInt.from((d as num).toInt()));\n  if (n is BigRat) { numer = n.num; denom = n.den; }\n  else if (n is BigInt) { numer = n; }\n  else if (n is int) { numer = BigInt.from(n); }\n  else if (n is num) { numer = BigInt.from(n.toInt()); }\n  else { numer = BigInt.zero; }\n  return BigRat(numer, denom);\n}\nBigInt _num(BigRat r) => r.num;\nBigInt _denom(BigRat r) => r.den;\nBigRat _add(BigRat a, BigRat b) => a.add(b);\nBigRat _sub(BigRat a, BigRat b) => a.sub(b);\nBigRat _mul(BigRat a, BigRat b) => a.mul(b);\nBigRat _div(BigRat a, BigRat b) => a.div(b);\nBigRat _neg(BigRat a) => BigRat(-a.num, a.den);\n\n"); err != nil {
			return err
		}
	}
	if useSHA256 {
		if _, err := io.WriteString(w, helperSHA256+"\n"); err != nil {
			return err
		}
	}
	if useFetch {
		if _, err := io.WriteString(w, helperFetch+"\n"); err != nil {
			return err
		}
	}
	if useMD5 {
		if _, err := io.WriteString(w, helperMD5+"\n"); err != nil {
			return err
		}
	}
	if useError {
		if _, err := io.WriteString(w, helperError+"\n"); err != nil {
			return err
		}
	}
	if useJSONPrint {
		if _, err := io.WriteString(w, helperJSONPrint+"\n"); err != nil {
			return err
		}
	}
	for _, name := range structOrder {
		fields := structFields[name]
		if _, err := fmt.Fprintf(w, "class %s {\n", name); err != nil {
			return err
		}
		if len(fields) == 0 {
			if _, err := fmt.Fprintf(w, "  %s();\n}\n\n", name); err != nil {
				return err
			}
			continue
		}
		for _, f := range fields {
			t := f.Type
			if structMutable[name] && !strings.HasSuffix(t, "?") {
				t += "?"
			}
			if _, err := fmt.Fprintf(w, "  %s %s;\n", t, f.Name); err != nil {
				return err
			}
		}
		if _, err := fmt.Fprintf(w, "  %s({", name); err != nil {
			return err
		}
		for i, f := range fields {
			if i > 0 {
				if _, err := io.WriteString(w, ", "); err != nil {
					return err
				}
			}
			kw := "required "
			if structMutable[name] {
				kw = ""
			}
			if _, err := fmt.Fprintf(w, "%sthis.%s", kw, f.Name); err != nil {
				return err
			}
		}
		if _, err := io.WriteString(w, "});\n"); err != nil {
			return err
		}
		if useFetch {
			if _, err := fmt.Fprintf(w, "  factory %s.fromJson(Map<String, dynamic> m) => %s(", name, name); err != nil {
				return err
			}
			for i, f := range fields {
				if i > 0 {
					if _, err := io.WriteString(w, ", "); err != nil {
						return err
					}
				}
				if _, err := fmt.Fprintf(w, "%s: m['%s']", f.Name, f.Name); err != nil {
					return err
				}
			}
			if _, err := io.WriteString(w, ");\n"); err != nil {
				return err
			}
		}
		if _, err := io.WriteString(w, "}\n\n"); err != nil {
			return err
		}
	}

	hasMain := false
	mainCalled := false
	for _, st := range p.Stmts {
		if fd, ok := st.(*FuncDecl); ok {
			if fd.Name == "main" || ((benchMain || renameMain) && fd.Name == "_main") {
				hasMain = true
			}
			if err := fd.emit(w); err != nil {
				return err
			}
			if _, err := io.WriteString(w, "\n\n"); err != nil {
				return err
			}
		} else if ls, ok := st.(*LetStmt); ok {
			if err := ls.emit(w); err != nil {
				return err
			}
			if _, err := io.WriteString(w, ";\n"); err != nil {
				return err
			}
		} else if vs, ok := st.(*VarStmt); ok {
			if err := vs.emit(w); err != nil {
				return err
			}
			if _, err := io.WriteString(w, ";\n"); err != nil {
				return err
			}
		} else if es, ok := st.(*ExprStmt); ok {
			if call, ok := es.Expr.(*CallExpr); ok {
				if name, ok := call.Func.(*Name); ok && name.Name == "main" && len(call.Args) == 0 {
					mainCalled = true
				}
			}
		} else if bs, ok := st.(*BenchStmt); ok {
			if stmtCallsMain(bs) {
				mainCalled = true
			}
		}
	}
	entry := "main"
	mainName := "main"
	if hasMain {
		entry = "_start"
		if benchMain {
			mainName = "_main"
		}
	}
	if _, err := io.WriteString(w, "void "+entry+"() {\n"); err != nil {
		return err
	}
	if p.BenchMain {
		if _, err := io.WriteString(w, "  var _benchMem0 = ProcessInfo.currentRss;\n"); err != nil {
			return err
		}
		if _, err := io.WriteString(w, "  var _benchSw = Stopwatch()..start();\n"); err != nil {
			return err
		}
	}
	if useNow {
		if _, err := io.WriteString(w, "  _initNow();\n"); err != nil {
			return err
		}
	}
	for _, st := range p.Stmts {
		switch st.(type) {
		case *FuncDecl, *LetStmt, *VarStmt:
			continue
		}
		if _, err := io.WriteString(w, "  "); err != nil {
			return err
		}
		if err := st.emit(w); err != nil {
			return err
		}
		if isBlockStmt(st) {
			if _, err := io.WriteString(w, "\n"); err != nil {
				return err
			}
		} else {
			if _, err := io.WriteString(w, ";\n"); err != nil {
				return err
			}
		}
	}
	if hasMain && !mainCalled {
		if _, err := io.WriteString(w, "  "+mainName+"();\n"); err != nil {
			return err
		}
	}
	if p.BenchMain {
		if _, err := io.WriteString(w, "  _benchSw.stop();\n"); err != nil {
			return err
		}
		if _, err := io.WriteString(w, "  var _benchMem1 = ProcessInfo.currentRss;\n"); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "  print(jsonEncode({\"duration_us\": _benchSw.elapsedMicroseconds, \"memory_bytes\": (_benchMem1 - _benchMem0).abs(), \"name\": %q}));\n", entry); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, "}\n"); err != nil {
		return err
	}
	if entry == "_start" {
		if _, err := io.WriteString(w, "\nvoid main() => _start();\n"); err != nil {
			return err
		}
	}
	return nil
}

// Transpile converts a Mochi program into a simple Dart AST.
func Transpile(prog *parser.Program, env *types.Env, bench, wrapMain bool) (*Program, error) {
	currentEnv = env
	structSeq = 0
	structSig = map[string]string{}
	structFields = map[string][]StructField{}
	mapLitStructName = map[*MapLit]string{}
	structOrder = nil
	compVarTypes = map[string]string{}
	localVarTypes = map[string]string{}
	funcReturnTypes = map[string]string{}
	structNameCount = map[string]int{}
	nextStructHint = ""
	usesJSON = false
	useNow = false
	useInput = false
	useLookupHost = false
	useBigRat = false
	useSHA256 = false
	useMD5 = false
	useFetch = false
	useError = false
	useJSONPrint = false
	useEnv = false
	useSubprocess = false
	useSubstrClamp = false
	useRepeat = false
	useStr = false
	useParseIntStr = false
	imports = nil
	testpkgAliases = map[string]struct{}{}
	netAliases = map[string]struct{}{}
	osAliases = map[string]struct{}{}
	subprocessAliases = map[string]struct{}{}
	structMutable = map[string]bool{}
	methodDefs = nil
	structMethods = map[string]map[string]bool{}
	benchMain = bench
	renameMain = false
	for _, st := range prog.Statements {
		if st.Fun != nil && st.Fun.Name == "main" {
			renameMain = true
			break
		}
	}
	p := &Program{BenchMain: bench, WrapMain: wrapMain}
	for _, st := range prog.Statements {
		s, err := convertStmtInternal(st)
		if err != nil {
			return nil, err
		}
		if s != nil {
			p.Stmts = append(p.Stmts, s)
		}
	}
	if len(methodDefs) > 0 {
		p.Stmts = append(methodDefs, p.Stmts...)
		methodDefs = nil
	}
	if wrapMain {
		usesJSON = true
		useNow = true
		var globals []Stmt
		var body []Stmt
		for _, st := range p.Stmts {
			switch st.(type) {
			case *FuncDecl, *LetStmt, *VarStmt:
				globals = append(globals, st)
			default:
				body = append(body, st)
			}
		}
		bench := &BenchStmt{Name: "main", Body: body}
		p.Stmts = append(globals, bench)
	}
	p.Imports = append(p.Imports, imports...)
	return p, nil
}

func convertIfStmt(i *parser.IfStmt) (Stmt, error) {
	cond, err := convertExpr(i.Cond)
	if err != nil {
		return nil, err
	}
	thenStmts, err := convertStmtList(i.Then)
	if err != nil {
		return nil, err
	}
	var elseStmts []Stmt
	if i.ElseIf != nil {
		s, err := convertIfStmt(i.ElseIf)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{s}
	} else if len(i.Else) > 0 {
		elseStmts, err = convertStmtList(i.Else)
		if err != nil {
			return nil, err
		}
	}
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
}

func convertWhileStmt(wst *parser.WhileStmt) (Stmt, error) {
	cond, err := convertExpr(wst.Cond)
	if err != nil {
		return nil, err
	}
	body, err := convertStmtList(wst.Body)
	if err != nil {
		return nil, err
	}
	return &WhileStmt{Cond: cond, Body: body}, nil
}

func convertForStmt(fst *parser.ForStmt) (Stmt, error) {
	if fst.RangeEnd != nil {
		start, err := convertExpr(fst.Source)
		if err != nil {
			return nil, err
		}
		end, err := convertExpr(fst.RangeEnd)
		if err != nil {
			return nil, err
		}
		body, err := convertStmtList(fst.Body)
		if err != nil {
			return nil, err
		}
		return &ForRangeStmt{Name: sanitize(fst.Name), Start: start, End: end, Body: body}, nil
	}
	iter, err := convertExpr(fst.Source)
	if err != nil {
		return nil, err
	}
	if iex, ok := iter.(*IndexExpr); ok {
		if strings.HasPrefix(strings.TrimSuffix(inferType(iex.Target), "?"), "Map<") {
			iex.NoBang = true
		}
	}
	elem := "dynamic"
	t := inferType(iter)
	if strings.HasPrefix(t, "List<") && strings.HasSuffix(t, ">") {
		elem = strings.TrimSuffix(strings.TrimPrefix(t, "List<"), ">")
	} else if strings.HasPrefix(t, "Map<") && strings.HasSuffix(t, ">") {
		kv := strings.TrimSuffix(strings.TrimPrefix(t, "Map<"), ">")
		parts := strings.SplitN(kv, ",", 2)
		if len(parts) > 0 {
			elem = strings.TrimSpace(parts[0])
		}
	} else if t == "String" {
		elem = "String"
	}
	saved := compVarTypes[fst.Name]
	compVarTypes[fst.Name] = elem
	body, err := convertStmtList(fst.Body)
	if err != nil {
		return nil, err
	}
	if saved != "" {
		compVarTypes[fst.Name] = saved
	} else {
		delete(compVarTypes, fst.Name)
	}
	return &ForInStmt{Name: sanitize(fst.Name), Iterable: iter, Body: body, ElemType: elem}, nil
}

func convertUpdate(u *parser.UpdateStmt) (Stmt, error) {
	if currentEnv == nil {
		return nil, fmt.Errorf("missing env")
	}
	t, err := currentEnv.GetVar(u.Target)
	if err != nil {
		return nil, err
	}
	lt, ok := t.(types.ListType)
	if !ok {
		return nil, fmt.Errorf("update target not list")
	}
	st, ok := lt.Elem.(types.StructType)
	if !ok {
		return nil, fmt.Errorf("update element not struct")
	}
	child := types.NewEnv(currentEnv)
	fieldSet := map[string]bool{}
	for name, ft := range st.Fields {
		child.SetVar(name, ft, true)
		fieldSet[name] = true
	}
	saved := currentEnv
	currentEnv = child
	var fields []string
	var values []Expr
	for _, it := range u.Set.Items {
		key, ok := isSimpleIdent(it.Key)
		if !ok {
			key, ok = literalString(it.Key)
			if !ok {
				currentEnv = saved
				return nil, fmt.Errorf("unsupported update key")
			}
		}
		val, err := convertExpr(it.Value)
		if err != nil {
			currentEnv = saved
			return nil, err
		}
		val = substituteFields(val, "item", fieldSet)
		fields = append(fields, key)
		values = append(values, val)
	}
	var cond Expr
	if u.Where != nil {
		cond, err = convertExpr(u.Where)
		if err != nil {
			currentEnv = saved
			return nil, err
		}
		cond = substituteFields(cond, "item", fieldSet)
	}
	currentEnv = saved
	return &UpdateStmt{Target: u.Target, Fields: fields, Values: values, Cond: cond}, nil
}

func convertIfExpr(ie *parser.IfExpr) (Expr, error) {
	cond, err := convertExpr(ie.Cond)
	if err != nil {
		return nil, err
	}
	thenExpr, err := convertExpr(ie.Then)
	if err != nil {
		return nil, err
	}
	var elseExpr Expr
	if ie.ElseIf != nil {
		elseExpr, err = convertIfExpr(ie.ElseIf)
		if err != nil {
			return nil, err
		}
	} else if ie.Else != nil {
		elseExpr, err = convertExpr(ie.Else)
		if err != nil {
			return nil, err
		}
	} else {
		elseExpr = &IntLit{Value: 0}
	}
	return &CondExpr{Cond: cond, Then: thenExpr, Else: elseExpr}, nil
}

func convertStmtList(list []*parser.Statement) ([]Stmt, error) {
	var out []Stmt
	for _, st := range list {
		s, err := convertStmtInternal(st)
		if err != nil {
			return nil, err
		}
		if s != nil {
			out = append(out, s)
		}
	}
	return out, nil
}

func convertTypeDecl(td *parser.TypeDecl) error {
	if currentEnv != nil {
		if stt, ok := currentEnv.GetStruct(td.Name); ok {
			var fields []StructField
			for _, name := range stt.Order {
				ft := stt.Fields[name]
				fields = append(fields, StructField{Name: name, Type: dartType(ft)})
			}
			structFields[stt.Name] = fields
			structOrder = append(structOrder, stt.Name)
		}
	}
	fieldSet := map[string]bool{}
	for _, m := range td.Members {
		if m.Field != nil {
			fieldSet[m.Field.Name] = true
		}
	}
	for _, m := range td.Members {
		if m.Method == nil {
			continue
		}
		if structMethods[td.Name] == nil {
			structMethods[td.Name] = map[string]bool{}
		}
		structMethods[td.Name][m.Method.Name] = true
		fn := *m.Method
		fn.Name = td.Name + "_" + fn.Name
		selfName := "self"
		selfTypeName := td.Name
		selfType := &parser.TypeRef{Simple: &selfTypeName}
		param := &parser.Param{Name: selfName, Type: selfType}
		fn.Params = append([]*parser.Param{param}, fn.Params...)
		stmt := &parser.Statement{Fun: &fn}
		conv, err := convertStmtInternal(stmt)
		if err != nil {
			return err
		}
		if conv != nil {
			// substitute field references with self indexing
			if fd, ok := conv.(*FuncDecl); ok {
				for i, st := range fd.Body {
					st = substituteStmtFields(st, selfName, fieldSet)
					fd.Body[i] = substituteStmtMethods(st, td.Name, selfName)
				}
			}
			methodDefs = append(methodDefs, conv)
		}
	}
	return nil
}

func convertStmtInternal(st *parser.Statement) (Stmt, error) {
	switch {
	case st.Test != nil:
		if _, err := convertStmtList(st.Test.Body); err != nil {
			return nil, err
		}
		return nil, nil
	case st.Expect != nil:
		return nil, nil
	case st.Type != nil:
		if err := convertTypeDecl(st.Type); err != nil {
			return nil, err
		}
		return nil, nil
	case st.ExternType != nil:
		return nil, nil
	case st.ExternVar != nil:
		return nil, nil
	case st.ExternFun != nil:
		return nil, nil
	case st.ExternObject != nil:
		return nil, nil
	case st.Import != nil:
		alias := st.Import.As
		if alias == "" {
			alias = parser.AliasFromPath(st.Import.Path)
		}
		if st.Import.Lang != nil && *st.Import.Lang == "python" && strings.Trim(st.Import.Path, "\"") == "math" {
			imports = append(imports, fmt.Sprintf("import 'dart:math' as %s;", alias))
			return nil, nil
		}
		if st.Import.Lang != nil && *st.Import.Lang == "python" && strings.Trim(st.Import.Path, "\"") == "subprocess" {
			subprocessAliases[alias] = struct{}{}
			useSubprocess = true
			imports = append(imports, "import 'dart:io';")
			return nil, nil
		}
		if st.Import.Lang != nil && *st.Import.Lang == "go" && strings.Trim(st.Import.Path, "\"") == "mochi/runtime/ffi/go/testpkg" {
			testpkgAliases[alias] = struct{}{}
			return nil, nil
		}
		if st.Import.Lang != nil && *st.Import.Lang == "go" && strings.Trim(st.Import.Path, "\"") == "net" {
			netAliases[alias] = struct{}{}
			useLookupHost = true
			imports = append(imports, "import 'dart:io';")
			return nil, nil
		}
		if st.Import.Lang != nil && *st.Import.Lang == "go" && strings.Trim(st.Import.Path, "\"") == "os" {
			osAliases[alias] = struct{}{}
			useEnv = true
			imports = append(imports, "import 'dart:io';")
			return nil, nil
		}
		return nil, fmt.Errorf("unsupported import")
	case st.Expr != nil:
		if se := extractSaveExpr(st.Expr.Expr); se != nil {
			src, err := convertExpr(se.Src)
			if err != nil {
				return nil, err
			}
			format := parseFormat(se.With)
			path := ""
			if se.Path != nil {
				path = strings.Trim(*se.Path, "\"")
			}
			if format == "jsonl" {
				usesJSON = true
			}
			return &SaveStmt{Src: src, Path: path, Format: format}, nil
		}
		e, err := convertExpr(st.Expr.Expr)
		if err != nil {
			return nil, err
		}
		return &ExprStmt{Expr: e}, nil
	case st.Let != nil:
		var e Expr
		var err error
		if st.Let.Value != nil {
			e, err = convertExpr(st.Let.Value)
			if err != nil {
				return nil, err
			}
		}
		name := sanitize(st.Let.Name)
		typ := typeRefString(st.Let.Type)
		if typ == "" && currentEnv != nil && !convInFunc {
			if t, err := currentEnv.GetVar(st.Let.Name); err == nil {
				typ = dartType(t)
			}
		}
		if typ == "" && e != nil {
			if st.Let.Type == nil {
				// leave type empty so that VarStmt decides based on runtime use
				typ = ""
			} else {
				typ = inferType(e)
				if ce, ok := e.(*CallExpr); ok {
					if n, ok2 := ce.Func.(*Name); ok2 {
						if rt, ok3 := funcReturnTypes[n.Name]; ok3 {
							typ = rt
						}
						if n.Name == "_fetch" && typ != "" {
							e = &CallExpr{Func: &SelectorExpr{Receiver: &Name{Name: typ}, Field: "fromJson"}, Args: []Expr{ce}}
						}
					}
				}
			}
		}
		if ce, ok := e.(*CallExpr); ok {
			if n, ok2 := ce.Func.(*Name); ok2 && n.Name == "_fetch" && typ != "" {
				e = &CallExpr{Func: &SelectorExpr{Receiver: &Name{Name: typ}, Field: "fromJson"}, Args: []Expr{ce}}
			}
		}
		if _, ok := e.(*IndexExpr); ok && strings.HasSuffix(typ, "?") {
			typ = strings.TrimSuffix(typ, "?")
		}
		valType := ""
		if e != nil {
			valType = inferType(e)
		}
		if typ != "" {
			localVarTypes[name] = typ
		} else if valType != "" {
			localVarTypes[name] = valType
		}
		return &LetStmt{Name: name, Type: typ, Value: e}, nil
	case st.Var != nil:
		var e Expr
		if st.Var.Value != nil {
			var err error
			e, err = convertExpr(st.Var.Value)
			if err != nil {
				return nil, err
			}
		} else if st.Var.Type != nil && st.Var.Type.Simple != nil && *st.Var.Type.Simple == "int" {
			e = &IntLit{Value: 0}
		}
		name := sanitize(st.Var.Name)
		typ := typeRefString(st.Var.Type)
		if typ == "" && currentEnv != nil && !convInFunc {
			if t, err := currentEnv.GetVar(st.Var.Name); err == nil {
				typ = dartType(t)
			}
		}
		if typ == "" && e != nil {
			if st.Var.Type == nil {
				// keep empty type so VarStmt emits a dynamic variable
				typ = ""
			} else {
				typ = inferType(e)
				if ce, ok := e.(*CallExpr); ok {
					if n, ok2 := ce.Func.(*Name); ok2 {
						if rt, ok3 := funcReturnTypes[n.Name]; ok3 {
							typ = rt
						}
						if n.Name == "_fetch" && typ != "" {
							e = &CallExpr{Func: &SelectorExpr{Receiver: &Name{Name: typ}, Field: "fromJson"}, Args: []Expr{ce}}
						}
					}
				}
			}
		}
		if ce, ok := e.(*CallExpr); ok {
			if n, ok2 := ce.Func.(*Name); ok2 && n.Name == "_fetch" && typ != "" {
				e = &CallExpr{Func: &SelectorExpr{Receiver: &Name{Name: typ}, Field: "fromJson"}, Args: []Expr{ce}}
			}
		}
		if _, ok := e.(*IndexExpr); ok && strings.HasSuffix(typ, "?") {
			typ = strings.TrimSuffix(typ, "?")
		}
		if _, ok := localVarTypes[name]; ok {
			return &AssignStmt{Target: &Name{Name: name}, Value: e}, nil
		}
		if typ != "" {
			localVarTypes[name] = typ
		}
		return &VarStmt{Name: name, Type: typ, Value: e}, nil
	case st.Assign != nil:
		target, err := convertAssignTarget(st.Assign)
		if err != nil {
			return nil, err
		}
		val, err := convertExpr(st.Assign.Value)
		if err != nil {
			return nil, err
		}
		return &AssignStmt{Target: target, Value: val}, nil
	case st.Return != nil:
		var e Expr
		if st.Return.Value != nil {
			var err error
			e, err = convertExpr(st.Return.Value)
			if err != nil {
				return nil, err
			}
		}
		return &ReturnStmt{Value: e}, nil
	case st.Break != nil:
		return &BreakStmt{}, nil
	case st.Continue != nil:
		return &ContinueStmt{}, nil
	case st.Fun != nil:
		var params []string
		paramTypes := make(map[string]string)
		savedGlobals := localVarTypes
		localVarTypes = map[string]string{}
		for k, v := range savedGlobals {
			localVarTypes[k] = v
		}
		for _, p := range st.Fun.Params {
			typ := typeRefString(p.Type)
			name := sanitize(p.Name)
			if typ == "" && currentEnv != nil {
				if t, err := currentEnv.GetVar(p.Name); err == nil {
					typ = dartType(t)
				}
			}
			if typ != "" {
				paramTypes[name] = typ
				localVarTypes[name] = typ
				params = append(params, fmt.Sprintf("%s %s", typ, name))
			} else {
				params = append(params, name)
			}
		}
		name := sanitize(st.Fun.Name)
		if (benchMain || renameMain) && name == "main" {
			name = "_main"
		}
		if rt := typeRefString(st.Fun.Return); rt != "" {
			funcReturnTypes[name] = rt
		}

		savedConv := convInFunc
		convInFunc = true
		body, err := convertStmtList(st.Fun.Body)
		convInFunc = savedConv
		if err != nil {
			localVarTypes = savedGlobals
			return nil, err
		}
		localVarTypes = savedGlobals
		fd := &FuncDecl{Name: name, Params: params, ParamTypes: paramTypes, Body: body}
		return fd, nil
	case st.While != nil:
		return convertWhileStmt(st.While)
	case st.For != nil:
		return convertForStmt(st.For)
	case st.If != nil:
		return convertIfStmt(st.If)
	case st.Bench != nil:
		body, err := convertStmtList(st.Bench.Body)
		if err != nil {
			return nil, err
		}
		usesJSON = true
		useNow = true
		name := strings.Trim(st.Bench.Name, "\"")
		return &BenchStmt{Name: name, Body: body}, nil
	case st.Update != nil:
		up, err := convertUpdate(st.Update)
		if err != nil {
			return nil, err
		}
		return up, nil
	default:
		return nil, fmt.Errorf("unsupported statement")
	}
}

func convertAssignTarget(as *parser.AssignStmt) (Expr, error) {
	baseName := sanitize(as.Name)
	expr := Expr(&Name{Name: baseName})
	for i, idx := range as.Index {
		if idx.Start == nil || idx.Colon != nil || idx.Colon2 != nil || idx.End != nil || idx.Step != nil {
			return nil, fmt.Errorf("complex assignment not supported")
		}
		val, err := convertExpr(idx.Start)
		if err != nil {
			return nil, err
		}
		iexpr := &IndexExpr{Target: expr, Index: val}
		if i < len(as.Index)-1 || len(as.Field) > 0 {
			iexpr.NoBang = true
			iexpr.NoSuffix = true
			expr = &NotNilExpr{X: iexpr}
		} else {
			if strings.HasPrefix(strings.TrimSuffix(inferType(expr), "?"), "Map<") {
				iexpr.NoBang = true
				iexpr.NoSuffix = true
			} else {
				iexpr.NoBang = true
			}
			expr = iexpr
		}
	}
	for i, f := range as.Field {
		nexpr := &SelectorExpr{Receiver: expr, Field: f.Name}
		if i < len(as.Field)-1 {
			expr = &NotNilExpr{X: nexpr}
		} else {
			expr = nexpr
			var typ string
			if currentEnv != nil {
				if t, err := currentEnv.GetVar(baseName); err == nil {
					if st, ok := t.(types.StructType); ok {
						typ = st.Name
					}
				}
			}
			if typ == "" {
				typ = localVarTypes[baseName]
			}
			// Struct fields are mutable by default in Dart, so we
			// don't need to track mutability for assignments.
		}
	}
	return expr, nil
}

func convertExpr(e *parser.Expr) (Expr, error) {
	if e == nil {
		return nil, fmt.Errorf("nil expr")
	}
	return convertBinary(e.Binary)
}

func convertBinary(b *parser.BinaryExpr) (Expr, error) {
	if b == nil {
		return nil, fmt.Errorf("nil binary")
	}
	first, err := convertUnary(b.Left)
	if err != nil {
		return nil, err
	}
	operands := []Expr{first}
	ops := make([]string, len(b.Right))
	opnodes := make([]*parser.BinaryOp, len(b.Right))
	for i, op := range b.Right {
		right, err := convertPostfix(op.Right)
		if err != nil {
			return nil, err
		}
		operands = append(operands, right)
		ops[i] = op.Op
		opnodes[i] = op
	}

	levels := [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
		{"union", "except", "intersect"},
	}

	contains := func(list []string, op string) bool {
		for _, s := range list {
			if s == op {
				return true
			}
		}
		return false
	}

	for _, level := range levels {
		for i := 0; i < len(ops); {
			if contains(level, ops[i]) {
				left := operands[i]
				right := operands[i+1]
				var expr Expr
				switch ops[i] {
				case "in":
					expr = &ContainsExpr{Target: right, Elem: left}
				case "union":
					if opnodes[i].All {
						expr = &UnionAllExpr{Left: left, Right: right}
					} else {
						expr = &UnionExpr{Left: left, Right: right}
					}
				case "except":
					expr = &ExceptExpr{Left: left, Right: right}
				case "intersect":
					expr = &IntersectExpr{Left: left, Right: right}
				default:
					expr = &BinaryExpr{Left: left, Op: ops[i], Right: right}
				}
				operands[i] = expr
				operands = append(operands[:i+1], operands[i+2:]...)
				ops = append(ops[:i], ops[i+1:]...)
				opnodes = append(opnodes[:i], opnodes[i+1:]...)
			} else {
				i++
			}
		}
	}

	return operands[0], nil
}

func convertUnary(u *parser.Unary) (Expr, error) {
	if u == nil {
		return nil, fmt.Errorf("nil unary")
	}
	ex, err := convertPostfix(u.Value)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		switch op {
		case "-", "!":
			ex = &UnaryExpr{Op: op, X: ex}
		default:
			return nil, fmt.Errorf("unary op %s not supported", op)
		}
	}
	return ex, nil
}

func convertPostfix(pf *parser.PostfixExpr) (Expr, error) {
	if pf == nil {
		return nil, fmt.Errorf("nil postfix")
	}
	expr, err := convertPrimary(pf.Target)
	if err != nil {
		return nil, err
	}
	replaced := false
	for i := 0; i < len(pf.Ops); i++ {
		op := pf.Ops[i]
		switch {
		case op.Call != nil:
			args := make([]Expr, len(op.Call.Args))
			for j, a := range op.Call.Args {
				arg, err := convertExpr(a)
				if err != nil {
					return nil, err
				}
				args[j] = arg
			}
			if sel, ok := expr.(*SelectorExpr); ok && sel.Field == "get" && (len(args) == 1 || len(args) == 2) {
				key := args[0]
				var def Expr
				if len(args) == 2 {
					def = args[1]
				}
				expr = &MapGetExpr{Map: sel.Receiver, Key: key, Default: def}
			} else {
				expr = &CallExpr{Func: expr, Args: args}
			}
		case op.Index != nil:
			if op.Index.Colon != nil || op.Index.Colon2 != nil || op.Index.End != nil || op.Index.Step != nil {
				if op.Index.Step != nil || op.Index.Colon2 != nil {
					return nil, fmt.Errorf("slice step not supported")
				}
				var startExpr, endExpr Expr
				if op.Index.Start != nil {
					startExpr, err = convertExpr(op.Index.Start)
					if err != nil {
						return nil, err
					}
				}
				if op.Index.End != nil {
					endExpr, err = convertExpr(op.Index.End)
					if err != nil {
						return nil, err
					}
				}
				if inferType(expr) == "String" || isMaybeString(expr) {
					useSubstrClamp = true
				}
				expr = &SliceExpr{Target: expr, Start: startExpr, End: endExpr}
			} else {
				if op.Index.Start == nil {
					return nil, fmt.Errorf("nil index")
				}
				idx, err := convertExpr(op.Index.Start)
				if err != nil {
					return nil, err
				}
				iex := &IndexExpr{Target: expr, Index: idx}
				expr = iex
			}
		case op.Field != nil:
			// method call if next op is call
			if i+1 < len(pf.Ops) && pf.Ops[i+1].Call != nil {
				call := pf.Ops[i+1]
				if op.Field.Name == "get" && (len(call.Call.Args) == 1 || len(call.Call.Args) == 2) {
					key, err := convertExpr(call.Call.Args[0])
					if err != nil {
						return nil, err
					}
					var def Expr
					if len(call.Call.Args) == 2 {
						def, err = convertExpr(call.Call.Args[1])
						if err != nil {
							return nil, err
						}
					}
					expr = &MapGetExpr{Map: expr, Key: key, Default: def}
					i++
					continue
				}
				if n, ok := expr.(*Name); ok {
					if _, ok := testpkgAliases[n.Name]; ok {
						switch op.Field.Name {
						case "FifteenPuzzleExample":
							if len(call.Call.Args) == 0 {
								expr = &StringLit{Value: testpkg.FifteenPuzzleExample()}
								i++
								continue
							}
						case "MD5Hex":
							if len(call.Call.Args) == 1 {
								arg, err := convertExpr(call.Call.Args[0])
								if err != nil {
									return nil, err
								}
								useMD5 = true
								expr = &CallExpr{Func: &Name{Name: "_md5hex"}, Args: []Expr{arg}}
								i++
								replaced = true
								continue
							}
						}
					}
					if _, ok := netAliases[n.Name]; ok {
						if op.Field.Name == "LookupHost" && len(call.Call.Args) == 1 {
							arg, err := convertExpr(call.Call.Args[0])
							if err != nil {
								return nil, err
							}
							useLookupHost = true
							expr = &CallExpr{Func: &Name{Name: "_lookupHost"}, Args: []Expr{arg}}
							i++
							replaced = true
							continue
						}
					}
					if _, ok := osAliases[n.Name]; ok {
						if op.Field.Name == "Getenv" && len(call.Call.Args) == 1 {
							arg, err := convertExpr(call.Call.Args[0])
							if err != nil {
								return nil, err
							}
							useEnv = true
							expr = &CallExpr{Func: &Name{Name: "_getenv"}, Args: []Expr{arg}}
							i++
							replaced = true
							continue
						}
						if op.Field.Name == "Environ" && len(call.Call.Args) == 0 {
							useEnv = true
							expr = &CallExpr{Func: &Name{Name: "_environ"}}
							i++
							replaced = true
							continue
						}
					}
				}
				var args []Expr
				for _, a := range call.Call.Args {
					ex, err := convertExpr(a)
					if err != nil {
						return nil, err
					}
					args = append(args, ex)
				}
				recvType := strings.TrimSuffix(inferType(expr), "?")
				if mset, ok := structMethods[recvType]; ok && mset[op.Field.Name] {
					args = append([]Expr{expr}, args...)
					expr = &CallExpr{Func: &Name{Name: recvType + "_" + op.Field.Name}, Args: args}
				} else if op.Field.Name == "padStart" {
					if len(args) != 2 {
						return nil, fmt.Errorf("padStart expects 2 args")
					}
					toStr := &CallExpr{Func: &SelectorExpr{Receiver: expr, Field: "toString"}}
					expr = &CallExpr{Func: &SelectorExpr{Receiver: toStr, Field: "padLeft"}, Args: args}
				} else {
					expr = &CallExpr{Func: &SelectorExpr{Receiver: expr, Field: op.Field.Name}, Args: args}
				}
				i++
			} else {
				expr = &SelectorExpr{Receiver: expr, Field: op.Field.Name}
			}
		case op.Call != nil:
			var args []Expr
			for _, a := range op.Call.Args {
				ex, err := convertExpr(a)
				if err != nil {
					return nil, err
				}
				args = append(args, ex)
			}
			if sel, ok := expr.(*SelectorExpr); ok {
				recvType := strings.TrimSuffix(inferType(sel.Receiver), "?")
				if mset, ok := structMethods[recvType]; ok && mset[sel.Field] {
					args = append([]Expr{sel.Receiver}, args...)
					expr = &CallExpr{Func: &Name{Name: recvType + "_" + sel.Field}, Args: args}
					replaced = true
				} else if n, ok := sel.Receiver.(*Name); ok {
					if _, ok := testpkgAliases[n.Name]; ok {
						switch sel.Field {
						case "FifteenPuzzleExample":
							if len(args) == 0 {
								expr = &StringLit{Value: testpkg.FifteenPuzzleExample()}
								break
							}
						case "MD5Hex":
							if len(args) == 1 {
								useMD5 = true
								expr = &CallExpr{Func: &Name{Name: "_md5hex"}, Args: args[:1]}
								args = nil
								replaced = true
								break
							}
						}
					} else if _, ok := netAliases[n.Name]; ok {
						if sel.Field == "LookupHost" && len(args) == 1 {
							useLookupHost = true
							expr = &CallExpr{Func: &Name{Name: "_lookupHost"}, Args: args}
							args = nil
							replaced = true
						}
					} else if _, ok := osAliases[n.Name]; ok {
						if sel.Field == "Getenv" && len(args) == 1 {
							useEnv = true
							expr = &CallExpr{Func: &Name{Name: "_getenv"}, Args: args}
							args = nil
							replaced = true
						} else if sel.Field == "Environ" && len(args) == 0 {
							useEnv = true
							expr = &CallExpr{Func: &Name{Name: "_environ"}}
							replaced = true
						}
					}
				}
			}
			if !replaced {
				if _, ok := expr.(*StringLit); !ok {
					expr = &CallExpr{Func: expr, Args: args}
				}
			}
		case op.Cast != nil:
			typ := typeRefString(op.Cast.Type)
			if typ == "BigRat" {
				useBigRat = true
				expr = &CallExpr{Func: &Name{"_bigrat"}, Args: []Expr{expr}}
			} else {
				expr = &CastExpr{Value: expr, Type: typ}
			}
		default:
			return nil, fmt.Errorf("postfix op not supported")
		}
	}
	return expr, nil
}

func convertPrimary(p *parser.Primary) (Expr, error) {
	switch {
	case p.Call != nil:
		if p.Call.Func == "len" && len(p.Call.Args) == 1 {
			arg, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &LenExpr{X: arg}, nil
		}
		if p.Call.Func == "append" && len(p.Call.Args) == 2 {
			list, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			val, err := convertExpr(p.Call.Args[1])
			if err != nil {
				return nil, err
			}
			if ll, ok := val.(*ListLit); ok && len(ll.Elems) == 0 && ll.ElemType == "" {
				lt := inferType(list)
				if strings.HasPrefix(lt, "List<") && strings.HasSuffix(lt, ">") {
					elem := strings.TrimSuffix(strings.TrimPrefix(lt, "List<"), ">")
					if !strings.HasPrefix(elem, "List<") && elem != "dynamic" {
						ll.ElemType = elem
					}
				}
			}
			return &AppendExpr{List: list, Value: val}, nil
		}
		if p.Call.Func == "concat" && len(p.Call.Args) == 2 {
			left, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			right, err := convertExpr(p.Call.Args[1])
			if err != nil {
				return nil, err
			}
			return &UnionAllExpr{Left: left, Right: right}, nil
		}
		if p.Call.Func == "avg" && len(p.Call.Args) == 1 {
			list, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &AvgExpr{List: list}, nil
		}
		if p.Call.Func == "sum" && len(p.Call.Args) == 1 {
			list, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &SumExpr{List: list}, nil
		}
		if p.Call.Func == "min" && len(p.Call.Args) == 1 {
			list, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &MinExpr{List: list}, nil
		}
		if p.Call.Func == "max" && len(p.Call.Args) == 1 {
			list, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &MaxExpr{List: list}, nil
		}
		if p.Call.Func == "now" && len(p.Call.Args) == 0 {
			useNow = true
			return &NowExpr{}, nil
		}
		if p.Call.Func == "input" && len(p.Call.Args) == 0 {
			useInput = true
			return &InputExpr{}, nil
		}
		if p.Call.Func == "json" && len(p.Call.Args) == 1 {
			usesJSON = true
			useJSONPrint = true
			arg, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &CallExpr{Func: &Name{Name: "_json"}, Args: []Expr{arg}}, nil
		}
		if p.Call.Func == "error" && len(p.Call.Args) == 1 {
			useError = true
			arg, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &CallExpr{Func: &Name{Name: "_error"}, Args: []Expr{arg}}, nil
		}
		if p.Call.Func == "values" && len(p.Call.Args) == 1 {
			mp, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &ValuesExpr{Map: mp}, nil
		}
		if p.Call.Func == "int" && len(p.Call.Args) == 1 {
			v, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			vt := inferType(v)
			if vt == "BigInt" {
				return &CallExpr{Func: &SelectorExpr{Receiver: v, Field: "toInt"}}, nil
			}
			if vt == "String" || vt == "dynamic" {
				return &CallExpr{Func: &SelectorExpr{Receiver: &Name{Name: "int"}, Field: "parse"}, Args: []Expr{v}}, nil
			}
			return &CastExpr{Value: v, Type: "int"}, nil
		}
		if p.Call.Func == "parseIntStr" && (len(p.Call.Args) == 1 || len(p.Call.Args) == 2) {
			arg, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			if len(p.Call.Args) == 2 {
				base, err := convertExpr(p.Call.Args[1])
				if err != nil {
					return nil, err
				}
				useParseIntStr = true
				return &CallExpr{Func: &Name{Name: "_parseIntStr"}, Args: []Expr{arg, base}}, nil
			}
			return &CallExpr{Func: &SelectorExpr{Receiver: &Name{Name: "int"}, Field: "parse"}, Args: []Expr{arg}}, nil
		}
		if p.Call.Func == "float" && len(p.Call.Args) == 1 {
			v, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &CastExpr{Value: v, Type: "num"}, nil
		}
		if p.Call.Func == "to_float" && len(p.Call.Args) == 1 {
			v, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &CallExpr{Func: &SelectorExpr{Receiver: v, Field: "toDouble"}}, nil
		}
		if p.Call.Func == "abs" && len(p.Call.Args) == 1 {
			v, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &CallExpr{Func: &SelectorExpr{Receiver: v, Field: "abs"}}, nil
		}
		if p.Call.Func == "panic" && len(p.Call.Args) == 1 {
			useError = true
			arg, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &CallExpr{Func: &Name{Name: "_error"}, Args: []Expr{arg}}, nil
		}
		if p.Call.Func == "str" && len(p.Call.Args) == 1 {
			v, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			useStr = true
			return &StrExpr{Value: v}, nil
		}
		if p.Call.Func == "count" && len(p.Call.Args) == 1 {
			arg, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			if t := types.ExprType(p.Call.Args[0], currentEnv); t != nil {
				if _, ok := t.(types.GroupType); ok {
					arg = &SelectorExpr{Receiver: arg, Field: "items"}
				}
			}
			return &CountExpr{X: arg}, nil
		}
		if p.Call.Func == "exists" && len(p.Call.Args) == 1 {
			if q := extractQuery(p.Call.Args[0]); q != nil {
				return convertExistsQuery(q)
			}
		}
		if p.Call.Func == "bigrat" && (len(p.Call.Args) == 1 || len(p.Call.Args) == 2) {
			a0, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			var a1 Expr
			if len(p.Call.Args) == 2 {
				a1, err = convertExpr(p.Call.Args[1])
				if err != nil {
					return nil, err
				}
			} else {
				a1 = &IntLit{Value: 1}
			}
			useBigRat = true
			return &CallExpr{Func: &Name{Name: "_bigrat"}, Args: []Expr{a0, a1}}, nil
		}
		if (p.Call.Func == "substring" || p.Call.Func == "substr") && len(p.Call.Args) == 3 {
			s0, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			s1, err := convertExpr(p.Call.Args[1])
			if err != nil {
				return nil, err
			}
			s2, err := convertExpr(p.Call.Args[2])
			if err != nil {
				return nil, err
			}
			useSubstrClamp = true
			return &SubstringExpr{Str: s0, Start: s1, End: s2}, nil
		}
		if p.Call.Func == "split" && len(p.Call.Args) == 2 {
			s0, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			s1, err := convertExpr(p.Call.Args[1])
			if err != nil {
				return nil, err
			}
			return &CallExpr{Func: &SelectorExpr{Receiver: s0, Field: "split"}, Args: []Expr{s1}}, nil
		}
		if p.Call.Func == "slice" && (len(p.Call.Args) == 2 || len(p.Call.Args) == 3) {
			l0, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			s1, err := convertExpr(p.Call.Args[1])
			if err != nil {
				return nil, err
			}
			var args []Expr
			args = append(args, s1)
			if len(p.Call.Args) == 3 {
				s2, err := convertExpr(p.Call.Args[2])
				if err != nil {
					return nil, err
				}
				args = append(args, s2)
			}
			return &CallExpr{Func: &SelectorExpr{Receiver: l0, Field: "sublist"}, Args: args}, nil
		}
		if p.Call.Func == "padStart" && len(p.Call.Args) == 3 {
			s0, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			s1, err := convertExpr(p.Call.Args[1])
			if err != nil {
				return nil, err
			}
			s2, err := convertExpr(p.Call.Args[2])
			if err != nil {
				return nil, err
			}
			return &CallExpr{Func: &SelectorExpr{Receiver: s0, Field: "padLeft"}, Args: []Expr{s1, s2}}, nil
		}
		if p.Call.Func == "repeat" && len(p.Call.Args) == 2 {
			s0, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			s1, err := convertExpr(p.Call.Args[1])
			if err != nil {
				return nil, err
			}
			useRepeat = true
			return &CallExpr{Func: &Name{Name: "_repeat"}, Args: []Expr{s0, s1}}, nil
		}
		if p.Call.Func == "pow" && len(p.Call.Args) == 2 {
			if currentEnv != nil {
				if _, ok := currentEnv.GetFunc("pow"); ok {
					// use user-defined pow
				} else {
					x, err := convertExpr(p.Call.Args[0])
					if err != nil {
						return nil, err
					}
					y, err := convertExpr(p.Call.Args[1])
					if err != nil {
						return nil, err
					}
					imports = append(imports, "import 'dart:math';")
					expr := &CallExpr{Func: &Name{Name: "pow"}, Args: []Expr{x, y}}
					expr = &CallExpr{Func: &SelectorExpr{Receiver: expr, Field: "toInt"}}
					return expr, nil
				}
			} else {
				x, err := convertExpr(p.Call.Args[0])
				if err != nil {
					return nil, err
				}
				y, err := convertExpr(p.Call.Args[1])
				if err != nil {
					return nil, err
				}
				imports = append(imports, "import 'dart:math';")
				expr := &CallExpr{Func: &Name{Name: "pow"}, Args: []Expr{x, y}}
				expr = &CallExpr{Func: &SelectorExpr{Receiver: expr, Field: "toInt"}}
				return expr, nil
			}
		}
		if p.Call.Func == "sha256" && len(p.Call.Args) == 1 {
			arg, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			useSHA256 = true
			return &CallExpr{Func: &Name{Name: "_sha256"}, Args: []Expr{arg}}, nil
		}
		if p.Call.Func == "num" && len(p.Call.Args) == 1 {
			arg, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			if inferType(arg) == "BigRat" {
				useBigRat = true
				return &CallExpr{Func: &Name{"_num"}, Args: []Expr{arg}}, nil
			}
			return arg, nil
		}
		if p.Call.Func == "denom" && len(p.Call.Args) == 1 {
			arg, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			if inferType(arg) == "BigRat" {
				useBigRat = true
				return &CallExpr{Func: &Name{"_denom"}, Args: []Expr{arg}}, nil
			}
			return &IntLit{Value: 1}, nil
		}
		if p.Call.Func == "upper" && len(p.Call.Args) == 1 {
			arg, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &CallExpr{Func: &SelectorExpr{Receiver: arg, Field: "toUpperCase"}}, nil
		}
		if p.Call.Func == "lower" && len(p.Call.Args) == 1 {
			arg, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &CallExpr{Func: &SelectorExpr{Receiver: arg, Field: "toLowerCase"}}, nil
		}
		if p.Call.Func == "indexOf" && len(p.Call.Args) == 2 {
			s, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			sub, err := convertExpr(p.Call.Args[1])
			if err != nil {
				return nil, err
			}
			return &CallExpr{Func: &SelectorExpr{Receiver: s, Field: "indexOf"}, Args: []Expr{sub}}, nil
		}
		if p.Call.Func == "print" {
			var args []Expr
			for _, a := range p.Call.Args {
				ex, err := convertExpr(a)
				if err != nil {
					return nil, err
				}
				if v, ok := ex.(*ValuesExpr); ok {
					ex = &CallExpr{Func: &SelectorExpr{Receiver: v, Field: "join"}, Args: []Expr{&StringLit{Value: " "}}}
				} else if t := types.ExprType(a, currentEnv); t != nil {
					if _, ok := t.(types.ListType); ok {
						ex = &FormatList{List: ex}
					}
				}
				if len(p.Call.Args) == 1 && isBoolExpr(a) {
					ex = &CondExpr{Cond: ex, Then: &IntLit{Value: 1}, Else: &IntLit{Value: 0}}
				}
				args = append(args, ex)
			}
			if len(args) == 1 {
				return &CallExpr{Func: &Name{"print"}, Args: args}, nil
			}
			join := &CallExpr{
				Func: &SelectorExpr{Receiver: &ListLit{Elems: args}, Field: "join"},
				Args: []Expr{&StringLit{Value: " "}},
			}
			return &CallExpr{Func: &Name{"print"}, Args: []Expr{join}}, nil
		}
		if p.Call.Func == "net.LookupHost" && len(p.Call.Args) == 1 {
			arg, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			useLookupHost = true
			return &CallExpr{Func: &Name{Name: "_lookupHost"}, Args: []Expr{arg}}, nil
		}
		if p.Call.Func == "os.Getenv" && len(p.Call.Args) == 1 {
			arg, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			useEnv = true
			return &CallExpr{Func: &Name{Name: "_getenv"}, Args: []Expr{arg}}, nil
		}
		if p.Call.Func == "os.Environ" && len(p.Call.Args) == 0 {
			useEnv = true
			return &CallExpr{Func: &Name{Name: "_environ"}}, nil
		}
		ce := &CallExpr{Func: &Name{p.Call.Func}}
		var paramTypes []string
		if currentEnv != nil {
			if fn, ok := currentEnv.GetFunc(p.Call.Func); ok {
				for _, p := range fn.Params {
					typ := dartType(types.ResolveTypeRef(p.Type, currentEnv))
					paramTypes = append(paramTypes, typ)
				}
			}
		}
		for i, a := range p.Call.Args {
			ex, err := convertExpr(a)
			if err != nil {
				return nil, err
			}
			if ll, ok := ex.(*ListLit); ok && len(ll.Elems) == 0 && ll.ElemType == "" {
				if i < len(paramTypes) && strings.HasPrefix(paramTypes[i], "List<") {
					ll.ElemType = strings.TrimSuffix(strings.TrimPrefix(paramTypes[i], "List<"), ">")
				}
			}
			ce.Args = append(ce.Args, ex)
		}
		return ce, nil
	case p.Lit != nil && p.Lit.Bool != nil:
		return &BoolLit{Value: bool(*p.Lit.Bool)}, nil
	case p.Lit != nil && p.Lit.Str != nil:
		return &StringLit{Value: *p.Lit.Str}, nil
	case p.Lit != nil && p.Lit.Int != nil:
		return &IntLit{Value: int(*p.Lit.Int)}, nil
	case p.Lit != nil && p.Lit.Float != nil:
		return &FloatLit{Value: *p.Lit.Float}, nil
	case p.Lit != nil && p.Lit.Null:
		return &Name{Name: "null"}, nil
	case p.List != nil:
		var elems []Expr
		for _, e := range p.List.Elems {
			ex, err := convertExpr(e)
			if err != nil {
				return nil, err
			}
			elems = append(elems, ex)
		}
		return &ListLit{Elems: elems}, nil
	case p.Map != nil:
		var entries []MapEntry
		for _, it := range p.Map.Items {
			k, err := convertExpr(it.Key)
			if err != nil {
				return nil, err
			}
			v, err := convertExpr(it.Value)
			if err != nil {
				return nil, err
			}
			entries = append(entries, MapEntry{Key: k, Value: v})
		}
		return &MapLit{Entries: entries}, nil
	case p.Load != nil:
		format, delim, header := parseLoadOptions(p.Load.With)
		path := ""
		if p.Load.Path != nil {
			path = strings.Trim(*p.Load.Path, "\"")
		}
		expr, err := dataExprFromFile(path, format, delim, header, p.Load.Type)
		if err == nil {
			return expr, nil
		}
		return nil, err
	case p.Fetch != nil:
		urlExpr, err := convertExpr(p.Fetch.URL)
		if err != nil {
			return nil, err
		}
		useFetch = true
		return &CallExpr{Func: &Name{Name: "_fetch"}, Args: []Expr{urlExpr}}, nil
	case p.Struct != nil:
		return convertStructLiteral(p.Struct)
	case p.If != nil:
		return convertIfExpr(p.If)
	case p.Match != nil:
		return convertMatchExpr(p.Match)
	case p.Query != nil:
		if ex, err := convertLeftJoinMultiQuery(p.Query); err == nil {
			return ex, nil
		}
		if ex, err := convertLeftJoinQuery(p.Query); err == nil {
			return ex, nil
		}
		if ex, err := convertRightJoinQuery(p.Query); err == nil {
			return ex, nil
		}
		if ex, err := convertOuterJoinQuery(p.Query); err == nil {
			return ex, nil
		}
		if p.Query.Group != nil {
			if ex, err := convertGroupLeftJoinQuery(p.Query); err == nil {
				return ex, nil
			}
			if ex, err := convertGroupQuery(p.Query); err == nil {
				return ex, nil
			}
		}
		return convertQueryExpr(p.Query)
	case p.FunExpr != nil && p.FunExpr.ExprBody != nil:
		var params []string
		saved := map[string]string{}
		for _, pa := range p.FunExpr.Params {
			name := sanitize(pa.Name)
			params = append(params, name)
			if pa.Type != nil {
				t := typeRefString(pa.Type)
				if t != "" {
					saved[name] = localVarTypes[name]
					localVarTypes[name] = t
				}
			}
		}
		body, err := convertExpr(p.FunExpr.ExprBody)
		for n, v := range saved {
			if v != "" {
				localVarTypes[n] = v
			} else {
				delete(localVarTypes, n)
			}
		}
		if err != nil {
			return nil, err
		}
		return &LambdaExpr{Params: params, Body: body}, nil
	case p.FunExpr != nil && p.FunExpr.BlockBody != nil:
		var params []string
		saved := map[string]string{}
		for _, pa := range p.FunExpr.Params {
			name := sanitize(pa.Name)
			params = append(params, name)
			if pa.Type != nil {
				t := typeRefString(pa.Type)
				if t != "" {
					saved[name] = localVarTypes[name]
					localVarTypes[name] = t
				}
			}
		}
		if len(p.FunExpr.BlockBody) == 1 && p.FunExpr.BlockBody[0].Return != nil {
			body, err := convertExpr(p.FunExpr.BlockBody[0].Return.Value)
			for n, v := range saved {
				if v != "" {
					localVarTypes[n] = v
				} else {
					delete(localVarTypes, n)
				}
			}
			if err != nil {
				return nil, err
			}
			return &LambdaExpr{Params: params, Body: body}, nil
		}
		body, err := convertStmtList(p.FunExpr.BlockBody)
		for n, v := range saved {
			if v != "" {
				localVarTypes[n] = v
			} else {
				delete(localVarTypes, n)
			}
		}
		if err != nil {
			return nil, err
		}
		return &LambdaBlock{Params: params, Body: body}, nil
	case p.Selector != nil:
		expr := Expr(&Name{Name: sanitize(p.Selector.Root)})
		for _, f := range p.Selector.Tail {
			expr = &SelectorExpr{Receiver: expr, Field: f}
		}
		return expr, nil
	case p.Group != nil:
		return convertExpr(p.Group)
	}
	return nil, fmt.Errorf("unsupported expression: %+v", *p)
}

func isBoolExpr(e *parser.Expr) bool { return isBoolBinary(e.Binary) }

func isBoolBinary(b *parser.BinaryExpr) bool {
	if b == nil {
		return false
	}
	if len(b.Right) == 0 {
		return isBoolUnary(b.Left)
	}
	for _, op := range b.Right {
		switch op.Op {
		case "==", "!=", "<", "<=", ">", ">=", "&&", "||":
			return true
		}
		if isBoolPostfix(op.Right) {
			return true
		}
	}
	return isBoolUnary(b.Left)
}

func isBoolUnary(u *parser.Unary) bool {
	if u == nil {
		return false
	}
	for _, op := range u.Ops {
		if op == "!" {
			return true
		}
	}
	return isBoolPostfix(u.Value)
}

func isBoolPostfix(pf *parser.PostfixExpr) bool {
	if pf == nil || len(pf.Ops) > 0 {
		return false
	}
	return isBoolPrimary(pf.Target)
}

func isBoolPrimary(p *parser.Primary) bool {
	if p == nil {
		return false
	}
	switch {
	case p.Lit != nil && p.Lit.Bool != nil:
		return true
	case p.Group != nil:
		return isBoolExpr(p.Group)
	default:
		return false
	}
}

func extractQuery(e *parser.Expr) *parser.QueryExpr {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return nil
	}
	if e.Binary.Left.Value.Target != nil {
		return e.Binary.Left.Value.Target.Query
	}
	return nil
}

func convertExistsQuery(q *parser.QueryExpr) (Expr, error) {
	if len(q.Froms) > 0 || len(q.Joins) > 0 || q.Group != nil || q.Sort != nil || q.Skip != nil || q.Take != nil || q.Distinct {
		return nil, fmt.Errorf("unsupported query")
	}
	src, err := convertExpr(q.Source)
	if err != nil {
		return nil, err
	}
	src = groupIter(src)
	if q.Where != nil {
		cond, err := convertExpr(q.Where)
		if err != nil {
			return nil, err
		}
		lam := &LambdaExpr{Params: []string{q.Var}, Body: cond}
		return &CallExpr{Func: &SelectorExpr{Receiver: src, Field: "any"}, Args: []Expr{lam}}, nil
	}
	return &SelectorExpr{Receiver: src, Field: "isNotEmpty"}, nil
}

func convertStructLiteral(sl *parser.StructLiteral) (Expr, error) {
	var entries []MapEntry
	savedHint := nextStructHint
	nextStructHint = ""
	if currentEnv != nil {
		if _, ok := currentEnv.FindUnionByVariant(sl.Name); ok {
			entries = append(entries, MapEntry{Key: &StringLit{Value: "__name"}, Value: &StringLit{Value: sl.Name}})
			for _, f := range sl.Fields {
				v, err := convertExpr(f.Value)
				if err != nil {
					return nil, err
				}
				entries = append(entries, MapEntry{Key: &StringLit{Value: f.Name}, Value: v})
			}
			return &MapLit{Entries: entries}, nil
		}
	}
	for _, f := range sl.Fields {
		v, err := convertExpr(f.Value)
		if err != nil {
			return nil, err
		}
		entries = append(entries, MapEntry{Key: &StringLit{Value: f.Name}, Value: v})
	}
	ml := &MapLit{Entries: entries}
	mapLitStructName[ml] = sl.Name
	nextStructHint = savedHint
	return ml, nil
}

func convertMatchExpr(me *parser.MatchExpr) (Expr, error) {
	target, err := convertExpr(me.Target)
	if err != nil {
		return nil, err
	}
	var expr Expr = &StringLit{Value: ""}
	for i := len(me.Cases) - 1; i >= 0; i-- {
		c := me.Cases[i]
		res, err := convertExpr(c.Result)
		if err != nil {
			return nil, err
		}
		if call, ok := callPattern(c.Pattern); ok && currentEnv != nil {
			if ut, ok := currentEnv.FindUnionByVariant(call.Func); ok {
				st := ut.Variants[call.Func]
				repl := map[string]Expr{}
				for idx, a := range call.Args {
					if name, ok := isSimpleIdent(a); ok && name != "_" {
						repl[name] = &IndexExpr{Target: target, Index: &StringLit{Value: st.Order[idx]}, NoBang: true}
					}
				}
				res = replaceVars(res, repl)
				cond := &BinaryExpr{Left: &IndexExpr{Target: target, Index: &StringLit{Value: "__name"}, NoBang: true}, Op: "==", Right: &StringLit{Value: call.Func}}
				expr = &CondExpr{Cond: cond, Then: res, Else: expr}
				continue
			}
		}
		if name, ok := isSimpleIdent(c.Pattern); ok {
			if _, ok := currentEnv.FindUnionByVariant(name); ok {
				cond := &BinaryExpr{Left: &IndexExpr{Target: target, Index: &StringLit{Value: "__name"}, NoBang: true}, Op: "==", Right: &StringLit{Value: name}}
				expr = &CondExpr{Cond: cond, Then: res, Else: expr}
				continue
			}
		}
		pat, err := convertExpr(c.Pattern)
		if err != nil {
			return nil, err
		}
		if n, ok := pat.(*Name); ok && n.Name == "_" {
			expr = res
			continue
		}
		cond := &BinaryExpr{Left: target, Op: "==", Right: pat}
		expr = &CondExpr{Cond: cond, Then: res, Else: expr}
	}
	return expr, nil
}

func cloneReplace(e Expr, old, new string) Expr {
	switch ex := e.(type) {
	case *Name:
		if ex.Name == old {
			return &Name{Name: new}
		}
		return &Name{Name: ex.Name}
	case *SelectorExpr:
		return &SelectorExpr{Receiver: cloneReplace(ex.Receiver, old, new), Field: ex.Field}
	case *UnaryExpr:
		return &UnaryExpr{Op: ex.Op, X: cloneReplace(ex.X, old, new)}
	case *BinaryExpr:
		return &BinaryExpr{Left: cloneReplace(ex.Left, old, new), Op: ex.Op, Right: cloneReplace(ex.Right, old, new)}
	case *CallExpr:
		args := make([]Expr, len(ex.Args))
		for i, a := range ex.Args {
			args[i] = cloneReplace(a, old, new)
		}
		return &CallExpr{Func: cloneReplace(ex.Func, old, new), Args: args}
	case *IndexExpr:
		return &IndexExpr{Target: cloneReplace(ex.Target, old, new), Index: cloneReplace(ex.Index, old, new)}
	case *ListLit:
		elems := make([]Expr, len(ex.Elems))
		for i, a := range ex.Elems {
			elems[i] = cloneReplace(a, old, new)
		}
		return &ListLit{Elems: elems}
	case *MapLit:
		ents := make([]MapEntry, len(ex.Entries))
		for i, m := range ex.Entries {
			ents[i] = MapEntry{Key: cloneReplace(m.Key, old, new), Value: cloneReplace(m.Value, old, new)}
		}
		return &MapLit{Entries: ents}
	case *AvgExpr:
		return &AvgExpr{List: cloneReplace(ex.List, old, new)}
	case *SumExpr:
		return &SumExpr{List: cloneReplace(ex.List, old, new)}
	case *MinExpr:
		return &MinExpr{List: cloneReplace(ex.List, old, new)}
	case *MaxExpr:
		return &MaxExpr{List: cloneReplace(ex.List, old, new)}
	case *MultiListComp:
		iters := make([]Expr, len(ex.Iters))
		for i, it := range ex.Iters {
			iters[i] = cloneReplace(it, old, new)
		}
		var cond Expr
		if ex.Cond != nil {
			cond = cloneReplace(ex.Cond, old, new)
		}
		return &MultiListComp{Vars: append([]string(nil), ex.Vars...), Iters: iters, Expr: cloneReplace(ex.Expr, old, new), Cond: cond}
	case *GroupQueryExpr:
		iters := make([]Expr, len(ex.Iters))
		for i, it := range ex.Iters {
			iters[i] = cloneReplace(it, old, new)
		}
		var cond Expr
		if ex.Cond != nil {
			cond = cloneReplace(ex.Cond, old, new)
		}
		var having Expr
		if ex.Having != nil {
			having = cloneReplace(ex.Having, old, new)
		}
		var sort *LambdaExpr
		if ex.Sort != nil {
			sort = &LambdaExpr{Params: append([]string(nil), ex.Sort.Params...), Body: cloneReplace(ex.Sort.Body, old, new)}
		}
		return &GroupQueryExpr{Vars: append([]string(nil), ex.Vars...), Iters: iters, Cond: cond, Key: cloneReplace(ex.Key, old, new), Row: cloneReplace(ex.Row, old, new), GroupVar: ex.GroupVar, Select: cloneReplace(ex.Select, old, new), Having: having, Sort: sort}
	case *GroupLeftJoinExpr:
		itersLeft := cloneReplace(ex.LeftSrc, old, new)
		itersRight := cloneReplace(ex.RightSrc, old, new)
		cond := cloneReplace(ex.Cond, old, new)
		var having Expr
		if ex.Having != nil {
			having = cloneReplace(ex.Having, old, new)
		}
		var sort *LambdaExpr
		if ex.Sort != nil {
			sort = &LambdaExpr{Params: append([]string(nil), ex.Sort.Params...), Body: cloneReplace(ex.Sort.Body, old, new)}
		}
		return &GroupLeftJoinExpr{LeftVar: ex.LeftVar, LeftSrc: itersLeft, RightVar: ex.RightVar, RightSrc: itersRight, Cond: cond, Key: cloneReplace(ex.Key, old, new), Row: cloneReplace(ex.Row, old, new), GroupVar: ex.GroupVar, Select: cloneReplace(ex.Select, old, new), Having: having, Sort: sort}
	default:
		return ex
	}
}

func isSimpleIdent(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return "", false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil {
		return "", false
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	return "", false
}

func callPattern(e *parser.Expr) (*parser.CallExpr, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return nil, false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil || p.Target.Call == nil {
		return nil, false
	}
	return p.Target.Call, true
}

func literalString(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return "", false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil {
		return "", false
	}
	if p.Target.Lit != nil && p.Target.Lit.Str != nil {
		return *p.Target.Lit.Str, true
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	return "", false
}

func literalBool(e *parser.Expr) (bool, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false, false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return false, false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil {
		return false, false
	}
	if p.Target.Lit != nil && p.Target.Lit.Bool != nil {
		return bool(*p.Target.Lit.Bool), true
	}
	return false, false
}

func parseFormat(e *parser.Expr) string {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return ""
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return ""
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil || p.Target.Map == nil {
		return ""
	}
	for _, it := range p.Target.Map.Items {
		key, ok := isSimpleIdent(it.Key)
		if !ok {
			key, ok = literalString(it.Key)
		}
		if key == "format" {
			if s, ok := literalString(it.Value); ok {
				return s
			}
		}
	}
	return ""
}

func parseLoadOptions(e *parser.Expr) (format, delimiter string, header bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil || p.Target.Map == nil {
		return
	}
	for _, it := range p.Target.Map.Items {
		key, ok := isSimpleIdent(it.Key)
		if !ok {
			key, ok = literalString(it.Key)
		}
		if !ok {
			continue
		}
		switch key {
		case "format":
			if s, ok := literalString(it.Value); ok {
				format = s
			}
		case "delimiter":
			if s, ok := literalString(it.Value); ok {
				delimiter = s
			}
		case "header":
			if b, ok := literalBool(it.Value); ok {
				header = b
			}
		}
	}
	return
}

func repoRoot() string {
	dir, _ := os.Getwd()
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return ""
}

func valueToExpr(v interface{}, typ *parser.TypeRef) Expr {
	switch val := v.(type) {
	case map[string]interface{}:
		keys := make([]string, 0, len(val))
		for k := range val {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		if typ != nil && typ.Simple != nil {
			st, ok := currentEnv.GetStruct(*typ.Simple)
			if ok {
				args := make([]Expr, len(st.Order))
				for i, name := range st.Order {
					args[i] = valueToExpr(val[name], nil)
				}
				return &CallExpr{Func: &Name{Name: *typ.Simple}, Args: args}
			}
		}
		entries := make([]MapEntry, len(keys))
		for i, k := range keys {
			entries[i] = MapEntry{Key: &StringLit{Value: k}, Value: valueToExpr(val[k], nil)}
		}
		return &MapLit{Entries: entries}
	case []interface{}:
		elems := make([]Expr, len(val))
		for i, it := range val {
			elems[i] = valueToExpr(it, typ)
		}
		return &ListLit{Elems: elems}
	case string:
		return &StringLit{Value: val}
	case bool:
		return &BoolLit{Value: val}
	case int:
		return &IntLit{Value: val}
	case int64:
		return &IntLit{Value: int(val)}
	case float64:
		return &FloatLit{Value: val}
	case float32:
		return &FloatLit{Value: float64(val)}
	default:
		return &Name{Name: "null"}
	}
}

func dataExprFromFile(path, format, delim string, header bool, typ *parser.TypeRef) (Expr, error) {
	if path == "" {
		return &ListLit{}, nil
	}
	root := repoRoot()
	if root != "" {
		if strings.HasPrefix(path, "../") {
			clean := strings.TrimPrefix(path, "../")
			path = filepath.Join(root, "tests", clean)
		} else if strings.HasPrefix(path, "tests/") {
			path = filepath.Join(root, path)
		}
	}
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	var v interface{}
	switch format {
	case "yaml":
		if err := yaml.Unmarshal(data, &v); err != nil {
			return nil, err
		}
	case "json":
		if err := json.Unmarshal(data, &v); err != nil {
			return nil, err
		}
	case "jsonl":
		var arr []interface{}
		for _, line := range bytes.Split(data, []byte{'\n'}) {
			line = bytes.TrimSpace(line)
			if len(line) == 0 {
				continue
			}
			var item interface{}
			if err := json.Unmarshal(line, &item); err == nil {
				arr = append(arr, item)
			}
		}
		v = arr
	case "csv", "":
		d := delim
		if d == "" {
			d = ","
		}
		lines := bytes.Split(data, []byte{'\n'})
		var rows []interface{}
		var headers []string
		for i, line := range lines {
			line = bytes.TrimSpace(line)
			if len(line) == 0 {
				continue
			}
			parts := strings.Split(string(line), d)
			if i == 0 && header {
				headers = parts
				continue
			}
			m := map[string]interface{}{}
			for j, part := range parts {
				key := fmt.Sprintf("c%d", j)
				if header && j < len(headers) {
					key = headers[j]
				}
				m[key] = strings.TrimSpace(part)
			}
			rows = append(rows, m)
		}
		v = rows
	default:
		return nil, fmt.Errorf("unsupported load format")
	}
	return valueToExpr(v, typ), nil
}

func extractSaveExpr(e *parser.Expr) *parser.SaveExpr {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return nil
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil {
		return nil
	}
	return p.Target.Save
}

func substituteFields(e Expr, varName string, fields map[string]bool) Expr {
	switch ex := e.(type) {
	case *Name:
		if fields[ex.Name] {
			return &SelectorExpr{Receiver: &Name{Name: varName}, Field: ex.Name}
		}
		return ex
	case *BinaryExpr:
		ex.Left = substituteFields(ex.Left, varName, fields)
		ex.Right = substituteFields(ex.Right, varName, fields)
		return ex
	case *UnaryExpr:
		ex.X = substituteFields(ex.X, varName, fields)
		return ex
	case *CallExpr:
		for i := range ex.Args {
			ex.Args[i] = substituteFields(ex.Args[i], varName, fields)
		}
		ex.Func = substituteFields(ex.Func, varName, fields)
		return ex
	case *IndexExpr:
		ex.Target = substituteFields(ex.Target, varName, fields)
		ex.Index = substituteFields(ex.Index, varName, fields)
		return ex
	case *SliceExpr:
		ex.Target = substituteFields(ex.Target, varName, fields)
		if ex.Start != nil {
			ex.Start = substituteFields(ex.Start, varName, fields)
		}
		if ex.End != nil {
			ex.End = substituteFields(ex.End, varName, fields)
		}
		return ex
	case *ListLit:
		for i := range ex.Elems {
			ex.Elems[i] = substituteFields(ex.Elems[i], varName, fields)
		}
		return ex
	case *MapLit:
		for i := range ex.Entries {
			ex.Entries[i].Key = substituteFields(ex.Entries[i].Key, varName, fields)
			ex.Entries[i].Value = substituteFields(ex.Entries[i].Value, varName, fields)
		}
		return ex
	case *CondExpr:
		ex.Cond = substituteFields(ex.Cond, varName, fields)
		ex.Then = substituteFields(ex.Then, varName, fields)
		ex.Else = substituteFields(ex.Else, varName, fields)
		return ex
	default:
		return ex
	}
}

func substituteStmtFields(s Stmt, varName string, fields map[string]bool) Stmt {
	switch st := s.(type) {
	case *ExprStmt:
		st.Expr = substituteFields(st.Expr, varName, fields)
		return st
	case *ReturnStmt:
		if st.Value != nil {
			st.Value = substituteFields(st.Value, varName, fields)
		}
		return st
	case *AssignStmt:
		st.Value = substituteFields(st.Value, varName, fields)
		return st
	default:
		return st
	}
}

func substituteMethodCalls(e Expr, structName, selfName string) Expr {
	switch ex := e.(type) {
	case *CallExpr:
		if n, ok := ex.Func.(*Name); ok {
			if mset, ok2 := structMethods[structName]; ok2 && mset[n.Name] {
				ex.Func = &Name{Name: structName + "_" + n.Name}
				ex.Args = append([]Expr{&Name{Name: selfName}}, ex.Args...)
			}
		} else {
			ex.Func = substituteMethodCalls(ex.Func, structName, selfName).(Expr)
		}
		for i := range ex.Args {
			ex.Args[i] = substituteMethodCalls(ex.Args[i], structName, selfName)
		}
		return ex
	case *BinaryExpr:
		ex.Left = substituteMethodCalls(ex.Left, structName, selfName)
		ex.Right = substituteMethodCalls(ex.Right, structName, selfName)
		return ex
	case *UnaryExpr:
		ex.X = substituteMethodCalls(ex.X, structName, selfName)
		return ex
	case *IndexExpr:
		ex.Target = substituteMethodCalls(ex.Target, structName, selfName)
		ex.Index = substituteMethodCalls(ex.Index, structName, selfName)
		return ex
	case *SliceExpr:
		ex.Target = substituteMethodCalls(ex.Target, structName, selfName)
		if ex.Start != nil {
			ex.Start = substituteMethodCalls(ex.Start, structName, selfName)
		}
		if ex.End != nil {
			ex.End = substituteMethodCalls(ex.End, structName, selfName)
		}
		return ex
	case *ListLit:
		for i := range ex.Elems {
			ex.Elems[i] = substituteMethodCalls(ex.Elems[i], structName, selfName)
		}
		return ex
	case *MapLit:
		for i := range ex.Entries {
			ex.Entries[i].Key = substituteMethodCalls(ex.Entries[i].Key, structName, selfName)
			ex.Entries[i].Value = substituteMethodCalls(ex.Entries[i].Value, structName, selfName)
		}
		return ex
	case *CondExpr:
		ex.Cond = substituteMethodCalls(ex.Cond, structName, selfName)
		ex.Then = substituteMethodCalls(ex.Then, structName, selfName)
		ex.Else = substituteMethodCalls(ex.Else, structName, selfName)
		return ex
	default:
		return ex
	}
}

func substituteStmtMethods(s Stmt, structName, selfName string) Stmt {
	switch st := s.(type) {
	case *ExprStmt:
		st.Expr = substituteMethodCalls(st.Expr, structName, selfName)
		return st
	case *ReturnStmt:
		if st.Value != nil {
			st.Value = substituteMethodCalls(st.Value, structName, selfName)
		}
		return st
	case *AssignStmt:
		st.Value = substituteMethodCalls(st.Value, structName, selfName)
		return st
	default:
		return st
	}
}

func replaceVars(e Expr, vars map[string]Expr) Expr {
	switch ex := e.(type) {
	case *Name:
		if v, ok := vars[ex.Name]; ok {
			return v
		}
		return ex
	case *BinaryExpr:
		ex.Left = replaceVars(ex.Left, vars)
		ex.Right = replaceVars(ex.Right, vars)
		return ex
	case *UnaryExpr:
		ex.X = replaceVars(ex.X, vars)
		return ex
	case *CallExpr:
		ex.Func = replaceVars(ex.Func, vars)
		for i := range ex.Args {
			ex.Args[i] = replaceVars(ex.Args[i], vars)
		}
		return ex
	case *IndexExpr:
		ex.Target = replaceVars(ex.Target, vars)
		ex.Index = replaceVars(ex.Index, vars)
		return ex
	case *SliceExpr:
		ex.Target = replaceVars(ex.Target, vars)
		if ex.Start != nil {
			ex.Start = replaceVars(ex.Start, vars)
		}
		if ex.End != nil {
			ex.End = replaceVars(ex.End, vars)
		}
		return ex
	case *ListLit:
		for i := range ex.Elems {
			ex.Elems[i] = replaceVars(ex.Elems[i], vars)
		}
		return ex
	case *MapLit:
		for i := range ex.Entries {
			ex.Entries[i].Key = replaceVars(ex.Entries[i].Key, vars)
			ex.Entries[i].Value = replaceVars(ex.Entries[i].Value, vars)
		}
		return ex
	case *CondExpr:
		ex.Cond = replaceVars(ex.Cond, vars)
		ex.Then = replaceVars(ex.Then, vars)
		ex.Else = replaceVars(ex.Else, vars)
		return ex
	default:
		return ex
	}
}

func convertLeftJoinQuery(q *parser.QueryExpr) (Expr, error) {
	if q == nil || len(q.Joins) != 1 || len(q.Froms) > 0 || q.Group != nil || q.Sort != nil || q.Skip != nil || q.Take != nil || q.Where != nil || q.Distinct {
		return nil, fmt.Errorf("unsupported query")
	}
	j := q.Joins[0]
	if j.Side == nil || *j.Side != "left" {
		return nil, fmt.Errorf("unsupported query")
	}
	leftSrc, err := convertExpr(q.Source)
	if err != nil {
		return nil, err
	}
	leftSrc = groupIter(leftSrc)
	rightSrc, err := convertExpr(j.Src)
	if err != nil {
		return nil, err
	}
	rightSrc = groupIter(rightSrc)
	child := types.NewEnv(currentEnv)
	child.SetVar(q.Var, types.AnyType{}, true)
	child.SetVar(j.Var, types.AnyType{}, true)
	saved := currentEnv
	currentEnv = child
	cond, err := convertExpr(j.On)
	if err != nil {
		currentEnv = saved
		return nil, err
	}
	sel, err := convertExpr(q.Select)
	currentEnv = saved
	if err != nil {
		return nil, err
	}
	return &LeftJoinExpr{LeftVar: q.Var, LeftSrc: leftSrc, RightVar: j.Var, RightSrc: rightSrc, Cond: cond, Select: sel}, nil
}

func convertLeftJoinMultiQuery(q *parser.QueryExpr) (Expr, error) {
	if q == nil || len(q.Joins) != 2 || len(q.Froms) > 0 || q.Group != nil || q.Sort != nil || q.Skip != nil || q.Take != nil || q.Where != nil || q.Distinct {
		return nil, fmt.Errorf("unsupported query")
	}
	j1 := q.Joins[0]
	j2 := q.Joins[1]
	if j1.Side != nil || j2.Side == nil || *j2.Side != "left" {
		return nil, fmt.Errorf("unsupported query")
	}
	src1, err := convertExpr(q.Source)
	if err != nil {
		return nil, err
	}
	src1 = groupIter(src1)
	src2, err := convertExpr(j1.Src)
	if err != nil {
		return nil, err
	}
	src2 = groupIter(src2)
	child := types.NewEnv(currentEnv)
	child.SetVar(q.Var, types.AnyType{}, true)
	child.SetVar(j1.Var, types.AnyType{}, true)
	saved := currentEnv
	currentEnv = child
	cond2, err := convertExpr(j1.On)
	if err != nil {
		currentEnv = saved
		return nil, err
	}
	child.SetVar(j2.Var, types.AnyType{}, true)
	src3, err := convertExpr(j2.Src)
	if err != nil {
		currentEnv = saved
		return nil, err
	}
	src3 = groupIter(src3)
	cond3, err := convertExpr(j2.On)
	if err != nil {
		currentEnv = saved
		return nil, err
	}
	sel, err := convertExpr(q.Select)
	currentEnv = saved
	if err != nil {
		return nil, err
	}
	return &LeftJoinMultiExpr{Var1: q.Var, Src1: src1, Var2: j1.Var, Src2: src2, Cond2: cond2, Var3: j2.Var, Src3: src3, Cond3: cond3, Select: sel}, nil
}

func convertRightJoinQuery(q *parser.QueryExpr) (Expr, error) {
	if q == nil || len(q.Joins) != 1 || len(q.Froms) > 0 || q.Group != nil || q.Sort != nil || q.Skip != nil || q.Take != nil || q.Where != nil || q.Distinct {
		return nil, fmt.Errorf("unsupported query")
	}
	j := q.Joins[0]
	if j.Side == nil || *j.Side != "right" {
		return nil, fmt.Errorf("unsupported query")
	}
	leftSrc, err := convertExpr(q.Source)
	if err != nil {
		return nil, err
	}
	leftSrc = groupIter(leftSrc)
	rightSrc, err := convertExpr(j.Src)
	if err != nil {
		return nil, err
	}
	rightSrc = groupIter(rightSrc)
	child := types.NewEnv(currentEnv)
	child.SetVar(q.Var, types.AnyType{}, true)
	child.SetVar(j.Var, types.AnyType{}, true)
	saved := currentEnv
	currentEnv = child
	cond, err := convertExpr(j.On)
	if err != nil {
		currentEnv = saved
		return nil, err
	}
	sel, err := convertExpr(q.Select)
	currentEnv = saved
	if err != nil {
		return nil, err
	}
	return &RightJoinExpr{LeftVar: q.Var, LeftSrc: leftSrc, RightVar: j.Var, RightSrc: rightSrc, Cond: cond, Select: sel}, nil
}

func convertOuterJoinQuery(q *parser.QueryExpr) (Expr, error) {
	if q == nil || len(q.Joins) != 1 || len(q.Froms) > 0 || q.Group != nil || q.Sort != nil || q.Skip != nil || q.Take != nil || q.Where != nil || q.Distinct {
		return nil, fmt.Errorf("unsupported query")
	}
	j := q.Joins[0]
	if j.Side == nil || *j.Side != "outer" {
		return nil, fmt.Errorf("unsupported query")
	}
	leftSrc, err := convertExpr(q.Source)
	if err != nil {
		return nil, err
	}
	rightSrc, err := convertExpr(j.Src)
	if err != nil {
		return nil, err
	}
	child := types.NewEnv(currentEnv)
	child.SetVar(q.Var, types.AnyType{}, true)
	child.SetVar(j.Var, types.AnyType{}, true)
	saved := currentEnv
	currentEnv = child
	cond, err := convertExpr(j.On)
	if err != nil {
		currentEnv = saved
		return nil, err
	}
	sel, err := convertExpr(q.Select)
	currentEnv = saved
	if err != nil {
		return nil, err
	}
	return &OuterJoinExpr{LeftVar: q.Var, LeftSrc: leftSrc, RightVar: j.Var, RightSrc: rightSrc, Cond: cond, Select: sel}, nil
}

func convertGroupQuery(q *parser.QueryExpr) (Expr, error) {
	if q == nil || q.Group == nil || len(q.Group.Exprs) != 1 || q.Distinct {
		return nil, fmt.Errorf("unsupported query")
	}
	src, err := convertExpr(q.Source)
	if err != nil {
		return nil, err
	}

	vars := []string{q.Var}
	iters := []Expr{src}
	var cond Expr

	for _, f := range q.Froms {
		e, err := convertExpr(f.Src)
		if err != nil {
			return nil, err
		}
		vars = append(vars, f.Var)
		iters = append(iters, groupIter(e))
	}

	for _, j := range q.Joins {
		if j.Side != nil {
			return nil, fmt.Errorf("unsupported query")
		}
		e, err := convertExpr(j.Src)
		if err != nil {
			return nil, err
		}
		vars = append(vars, j.Var)
		iters = append(iters, groupIter(e))
		jc, err := convertExpr(j.On)
		if err != nil {
			return nil, err
		}
		if cond == nil {
			cond = jc
		} else {
			cond = &BinaryExpr{Left: cond, Op: "&&", Right: jc}
		}
	}

	if q.Where != nil {
		c, err := convertExpr(q.Where)
		if err != nil {
			return nil, err
		}
		if cond == nil {
			cond = c
		} else {
			cond = &BinaryExpr{Left: cond, Op: "&&", Right: c}
		}
	}
	key, err := convertExpr(q.Group.Exprs[0])
	if err != nil {
		return nil, err
	}

	var row Expr
	if len(vars) == 1 {
		row = &Name{Name: vars[0]}
	} else {
		var ents []MapEntry
		for _, v := range vars {
			ents = append(ents, MapEntry{Key: &Name{Name: v}, Value: &Name{Name: v}})
		}
		row = &MapLit{Entries: ents}
	}

	saved := currentEnv
	child := types.NewEnv(currentEnv)
	for _, v := range vars {
		child.SetVar(v, types.AnyType{}, true)
	}
	genv := types.NewEnv(child)
	genv.SetVar(q.Group.Name, types.GroupType{Key: types.AnyType{}, Elem: types.AnyType{}}, true)
	currentEnv = genv
	sel, err := convertExpr(q.Select)
	if err != nil {
		currentEnv = saved
		return nil, err
	}
	elemType := inferType(sel)
	var having Expr
	var sort *LambdaExpr
	if q.Group.Having != nil {
		having, err = convertExpr(q.Group.Having)
		if err != nil {
			currentEnv = saved
			return nil, err
		}
	}
	if q.Sort != nil {
		s, err := convertExpr(q.Sort)
		if err != nil {
			currentEnv = saved
			return nil, err
		}
		desc := false
		if ue, ok := s.(*UnaryExpr); ok && ue.Op == "-" {
			desc = true
			s = ue.X
		}
		a := cloneReplace(s, q.Group.Name, "a")
		b := cloneReplace(s, q.Group.Name, "b")
		if desc {
			a, b = b, a
		}
		cmp := &CallExpr{Func: &SelectorExpr{Receiver: a, Field: "compareTo"}, Args: []Expr{b}}
		sort = &LambdaExpr{Params: []string{"a", "b"}, Body: cmp}
	}
	currentEnv = saved
	return &GroupQueryExpr{Vars: vars, Iters: iters, Cond: cond, Key: key, Row: row, GroupVar: q.Group.Name, Select: sel, Having: having, Sort: sort, ElemType: elemType}, nil
}

func convertGroupLeftJoinQuery(q *parser.QueryExpr) (Expr, error) {
	if q == nil || q.Group == nil || len(q.Group.Exprs) != 1 || len(q.Joins) != 1 || len(q.Froms) > 0 || q.Distinct {
		return nil, fmt.Errorf("unsupported query")
	}
	j := q.Joins[0]
	if j.Side == nil || *j.Side != "left" {
		return nil, fmt.Errorf("unsupported query")
	}
	leftSrc, err := convertExpr(q.Source)
	if err != nil {
		return nil, err
	}
	rightSrc, err := convertExpr(j.Src)
	if err != nil {
		return nil, err
	}
	child := types.NewEnv(currentEnv)
	child.SetVar(q.Var, types.AnyType{}, true)
	child.SetVar(j.Var, types.AnyType{}, true)
	saved := currentEnv
	currentEnv = child
	cond, err := convertExpr(j.On)
	if err != nil {
		currentEnv = saved
		return nil, err
	}
	key, err := convertExpr(q.Group.Exprs[0])
	if err != nil {
		currentEnv = saved
		return nil, err
	}
	row := &MapLit{Entries: []MapEntry{{Key: &Name{Name: q.Var}, Value: &Name{Name: q.Var}}, {Key: &Name{Name: j.Var}, Value: &Name{Name: j.Var}}}}
	genv := types.NewEnv(child)
	genv.SetVar(q.Group.Name, types.GroupType{Key: types.AnyType{}, Elem: types.AnyType{}}, true)
	currentEnv = genv
	sel, err := convertExpr(q.Select)
	if err != nil {
		currentEnv = saved
		return nil, err
	}
	var having Expr
	elemType := inferType(sel)
	var sort *LambdaExpr
	if q.Group.Having != nil {
		having, err = convertExpr(q.Group.Having)
		if err != nil {
			currentEnv = saved
			return nil, err
		}
	}
	if q.Sort != nil {
		s, err := convertExpr(q.Sort)
		if err != nil {
			currentEnv = saved
			return nil, err
		}
		desc := false
		if ue, ok := s.(*UnaryExpr); ok && ue.Op == "-" {
			desc = true
			s = ue.X
		}
		a := cloneReplace(s, q.Group.Name, "a")
		b := cloneReplace(s, q.Group.Name, "b")
		if desc {
			a, b = b, a
		}
		cmp := &CallExpr{Func: &SelectorExpr{Receiver: a, Field: "compareTo"}, Args: []Expr{b}}
		sort = &LambdaExpr{Params: []string{"a", "b"}, Body: cmp}
	}
	currentEnv = saved
	return &GroupLeftJoinExpr{LeftVar: q.Var, LeftSrc: leftSrc, RightVar: j.Var, RightSrc: rightSrc, Cond: cond, Key: key, Row: row, GroupVar: q.Group.Name, Select: sel, Having: having, Sort: sort, ElemType: elemType}, nil
}

func convertQueryExpr(q *parser.QueryExpr) (Expr, error) {
	if q.Group != nil || q.Distinct {
		return nil, fmt.Errorf("unsupported query")
	}

	vars := []string{q.Var}
	iters := []Expr{}

	src, err := convertExpr(q.Source)
	if err != nil {
		return nil, err
	}
	iters = append(iters, groupIter(src))

	for _, f := range q.Froms {
		e, err := convertExpr(f.Src)
		if err != nil {
			return nil, err
		}
		vars = append(vars, f.Var)
		iters = append(iters, groupIter(e))
	}

	var cond Expr

	for _, j := range q.Joins {
		if j.Side != nil {
			return nil, fmt.Errorf("unsupported query")
		}
		e, err := convertExpr(j.Src)
		if err != nil {
			return nil, err
		}
		vars = append(vars, j.Var)
		iters = append(iters, groupIter(e))
		jc, err := convertExpr(j.On)
		if err != nil {
			return nil, err
		}
		if cond == nil {
			cond = jc
		} else {
			cond = &BinaryExpr{Left: cond, Op: "&&", Right: jc}
		}
	}

	if q.Where != nil {
		wcond, err := convertExpr(q.Where)
		if err != nil {
			return nil, err
		}
		if cond == nil {
			cond = wcond
		} else {
			cond = &BinaryExpr{Left: cond, Op: "&&", Right: wcond}
		}
	}

	body := Expr(&Name{Name: q.Var})
	if q.Select != nil {
		body, err = convertExpr(q.Select)
		if err != nil {
			return nil, err
		}
	}

	expr := Expr(&MultiListComp{Vars: vars, Iters: iters, Expr: body, Cond: cond})

	if q.Sort != nil {
		sortExpr, err := convertExpr(q.Sort)
		if err != nil {
			return nil, err
		}
		desc := false
		if ue, ok := sortExpr.(*UnaryExpr); ok && ue.Op == "-" {
			desc = true
			sortExpr = ue.X
		}
		a := cloneReplace(sortExpr, q.Var, "a")
		b := cloneReplace(sortExpr, q.Var, "b")
		if desc {
			a, b = b, a
		}
		cmp := &CallExpr{Func: &SelectorExpr{Receiver: a, Field: "compareTo"}, Args: []Expr{b}}
		expr = &SortExpr{List: expr, Compare: &LambdaExpr{Params: []string{"a", "b"}, Body: cmp}}
	}

	if q.Skip != nil || q.Take != nil {
		iter := expr
		if q.Skip != nil {
			s, err := convertExpr(q.Skip)
			if err != nil {
				return nil, err
			}
			iter = &CallExpr{Func: &SelectorExpr{Receiver: iter, Field: "skip"}, Args: []Expr{s}}
		}
		if q.Take != nil {
			t, err := convertExpr(q.Take)
			if err != nil {
				return nil, err
			}
			iter = &CallExpr{Func: &SelectorExpr{Receiver: iter, Field: "take"}, Args: []Expr{t}}
		}
		expr = &CallExpr{Func: &SelectorExpr{Receiver: iter, Field: "toList"}, Args: nil}
	}

	return expr, nil
}

// --- AST -> generic node (for debugging) ---
func toNode(p *Program) *ast.Node {
	n := &ast.Node{Kind: "program"}
	for _, st := range p.Stmts {
		n.Children = append(n.Children, stmtNode(st))
	}
	return n
}

func stmtNode(s Stmt) *ast.Node {
	switch st := s.(type) {
	case *ExprStmt:
		return &ast.Node{Kind: "expr", Children: []*ast.Node{exprNode(st.Expr)}}
	case *LetStmt:
		return &ast.Node{Kind: "let", Value: st.Name, Children: []*ast.Node{exprNode(st.Value)}}
	case *VarStmt:
		node := &ast.Node{Kind: "var", Value: st.Name}
		if st.Value != nil {
			node.Children = []*ast.Node{exprNode(st.Value)}
		}
		return node
	case *AssignStmt:
		return &ast.Node{Kind: "assign", Children: []*ast.Node{exprNode(st.Target), exprNode(st.Value)}}
	case *ReturnStmt:
		n := &ast.Node{Kind: "return"}
		if st.Value != nil {
			n.Children = []*ast.Node{exprNode(st.Value)}
		}
		return n
	case *FuncDecl:
		n := &ast.Node{Kind: "func", Value: st.Name}
		for _, p := range st.Params {
			n.Children = append(n.Children, &ast.Node{Kind: "param", Value: p})
		}
		for _, b := range st.Body {
			n.Children = append(n.Children, stmtNode(b))
		}
		return n
	case *IfStmt:
		n := &ast.Node{Kind: "if", Children: []*ast.Node{exprNode(st.Cond)}}
		thenNode := &ast.Node{Kind: "then"}
		for _, c := range st.Then {
			thenNode.Children = append(thenNode.Children, stmtNode(c))
		}
		n.Children = append(n.Children, thenNode)
		if len(st.Else) > 0 {
			elseNode := &ast.Node{Kind: "else"}
			for _, c := range st.Else {
				elseNode.Children = append(elseNode.Children, stmtNode(c))
			}
			n.Children = append(n.Children, elseNode)
		}
		return n
	case *WhileStmt:
		n := &ast.Node{Kind: "while", Children: []*ast.Node{exprNode(st.Cond)}}
		body := &ast.Node{Kind: "body"}
		for _, b := range st.Body {
			body.Children = append(body.Children, stmtNode(b))
		}
		n.Children = append(n.Children, body)
		return n
	case *ForRangeStmt:
		n := &ast.Node{Kind: "for-range", Value: st.Name, Children: []*ast.Node{exprNode(st.Start), exprNode(st.End)}}
		body := &ast.Node{Kind: "body"}
		for _, b := range st.Body {
			body.Children = append(body.Children, stmtNode(b))
		}
		n.Children = append(n.Children, body)
		return n
	case *ForInStmt:
		n := &ast.Node{Kind: "for-in", Value: st.Name, Children: []*ast.Node{exprNode(st.Iterable)}}
		body := &ast.Node{Kind: "body"}
		for _, b := range st.Body {
			body.Children = append(body.Children, stmtNode(b))
		}
		n.Children = append(n.Children, body)
		return n
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

func exprNode(e Expr) *ast.Node {
	switch ex := e.(type) {
	case *CallExpr:
		n := &ast.Node{Kind: "call"}
		n.Children = append(n.Children, exprNode(ex.Func))
		for _, a := range ex.Args {
			n.Children = append(n.Children, exprNode(a))
		}
		return n
	case *Name:
		return &ast.Node{Kind: "name", Value: ex.Name}
	case *StringLit:
		return &ast.Node{Kind: "string", Value: ex.Value}
	case *IntLit:
		return &ast.Node{Kind: "int", Value: fmt.Sprint(ex.Value)}
	case *BoolLit:
		if ex.Value {
			return &ast.Node{Kind: "bool", Value: "true"}
		}
		return &ast.Node{Kind: "bool", Value: "false"}
	case *BinaryExpr:
		return &ast.Node{Kind: "binary", Value: ex.Op, Children: []*ast.Node{exprNode(ex.Left), exprNode(ex.Right)}}
	case *UnaryExpr:
		return &ast.Node{Kind: "unary", Value: ex.Op, Children: []*ast.Node{exprNode(ex.X)}}
	case *LambdaExpr:
		n := &ast.Node{Kind: "lambda"}
		for _, p := range ex.Params {
			n.Children = append(n.Children, &ast.Node{Kind: "param", Value: p})
		}
		n.Children = append(n.Children, exprNode(ex.Body))
		return n
	case *IndexExpr:
		return &ast.Node{Kind: "index", Children: []*ast.Node{exprNode(ex.Target), exprNode(ex.Index)}}
	case *SubstringExpr:
		return &ast.Node{Kind: "substring", Children: []*ast.Node{exprNode(ex.Str), exprNode(ex.Start), exprNode(ex.End)}}
	case *SliceExpr:
		return &ast.Node{Kind: "slice", Children: []*ast.Node{exprNode(ex.Target), exprNode(ex.Start), exprNode(ex.End)}}
	case *AppendExpr:
		return &ast.Node{Kind: "append", Children: []*ast.Node{exprNode(ex.List), exprNode(ex.Value)}}
	case *UnionExpr:
		return &ast.Node{Kind: "union", Children: []*ast.Node{exprNode(ex.Left), exprNode(ex.Right)}}
	case *UnionAllExpr:
		return &ast.Node{Kind: "unionall", Children: []*ast.Node{exprNode(ex.Left), exprNode(ex.Right)}}
	case *ExceptExpr:
		return &ast.Node{Kind: "except", Children: []*ast.Node{exprNode(ex.Left), exprNode(ex.Right)}}
	case *IntersectExpr:
		return &ast.Node{Kind: "intersect", Children: []*ast.Node{exprNode(ex.Left), exprNode(ex.Right)}}
	case *ContainsExpr:
		return &ast.Node{Kind: "contains", Children: []*ast.Node{exprNode(ex.Target), exprNode(ex.Elem)}}
	case *AvgExpr:
		return &ast.Node{Kind: "avg", Children: []*ast.Node{exprNode(ex.List)}}
	case *SumExpr:
		return &ast.Node{Kind: "sum", Children: []*ast.Node{exprNode(ex.List)}}
	case *MinExpr:
		return &ast.Node{Kind: "min", Children: []*ast.Node{exprNode(ex.List)}}
	case *MaxExpr:
		return &ast.Node{Kind: "max", Children: []*ast.Node{exprNode(ex.List)}}
	case *ValuesExpr:
		return &ast.Node{Kind: "values", Children: []*ast.Node{exprNode(ex.Map)}}
	case *StrExpr:
		return &ast.Node{Kind: "str", Children: []*ast.Node{exprNode(ex.Value)}}
	case *FormatList:
		return &ast.Node{Kind: "format-list", Children: []*ast.Node{exprNode(ex.List)}}
	case *CountExpr:
		return &ast.Node{Kind: "count", Children: []*ast.Node{exprNode(ex.X)}}
	case *GroupQueryExpr:
		n := &ast.Node{Kind: "group-query"}
		for i, v := range ex.Vars {
			n.Children = append(n.Children, &ast.Node{Kind: "for", Value: v, Children: []*ast.Node{exprNode(ex.Iters[i])}})
		}
		if ex.Cond != nil {
			n.Children = append(n.Children, &ast.Node{Kind: "if", Children: []*ast.Node{exprNode(ex.Cond)}})
		}
		n.Children = append(n.Children, exprNode(ex.Key))
		n.Children = append(n.Children, exprNode(ex.Row))
		if ex.Having != nil {
			n.Children = append(n.Children, &ast.Node{Kind: "having", Children: []*ast.Node{exprNode(ex.Having)}})
		}
		if ex.Sort != nil {
			n.Children = append(n.Children, &ast.Node{Kind: "sort-lambda", Children: []*ast.Node{exprNode(ex.Sort)}})
		}
		n.Children = append(n.Children, exprNode(ex.Select))
		return n
	case *SortExpr:
		return &ast.Node{Kind: "sort", Children: []*ast.Node{exprNode(ex.List), exprNode(ex.Compare)}}
	case *MultiListComp:
		n := &ast.Node{Kind: "multi-list-comp"}
		for i, v := range ex.Vars {
			n.Children = append(n.Children, &ast.Node{Kind: "for", Value: v, Children: []*ast.Node{exprNode(ex.Iters[i])}})
		}
		if ex.Cond != nil {
			n.Children = append(n.Children, &ast.Node{Kind: "if", Children: []*ast.Node{exprNode(ex.Cond)}})
		}
		n.Children = append(n.Children, exprNode(ex.Expr))
		return n
	case *NotNilExpr:
		return &ast.Node{Kind: "notnil", Children: []*ast.Node{exprNode(ex.X)}}
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

// Print writes the AST in Lisp-like form to stdout.
func Print(p *Program) { toNode(p).Print("") }
