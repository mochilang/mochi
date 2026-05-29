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
	listSortAsc bool // mochi_list_sort_asc($xs) → ascending stable copy
	streams     bool // Phase 10: MochiStream/MochiSub classes + helpers
	async       bool // Phase 11: MochiFuture class + future helpers
	llm         bool // Phase 13: mochi_llm_generate cassette helper
}

type lowerer struct {
	runtime runtimeFlags
	prog    *aotir.Program
	// matchSeq is a monotonic counter used to mint unique PHP temp
	// variable names for nested or successive match statements within
	// one function body.
	matchSeq int
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

	// Sum types: one abstract base + one final-readonly child per
	// variant. Emitted before user functions so the function bodies
	// can reference the class names.
	for _, u := range prog.Unions {
		ds, err := l.lowerUnion(u)
		if err != nil {
			return nil, err
		}
		classDecls = append(classDecls, ds...)
	}

	// Phase 9: one mutable PHP class per `agent` declaration. Fields
	// become public typed properties with their declared defaults;
	// intent bodies become public instance methods that read and
	// write `$this->FIELD`.
	for _, ag := range prog.Agents {
		d, err := l.lowerAgent(ag)
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
	if l.runtime.listSortAsc {
		out = append(out, &ptree.FuncDecl{
			PhpDoc: []string{
				"Return a sorted (ascending) copy of $xs. Mochi semantics",
				"are non-mutating; the argument is taken by value so the",
				"caller's array is untouched. Uses PHP's spaceship operator",
				"so ints, floats, and strings all order naturally.",
			},
			Name:       "mochi_list_sort_asc",
			Params:     []ptree.FuncParam{{TypeName: "array", Name: "xs"}},
			ReturnType: "array",
			Body: []ptree.Stmt{
				&ptree.RawStmt{Text: `usort($xs, fn($a, $b) => $a <=> $b);`},
				&ptree.RawStmt{Text: `return $xs;`},
			},
		})
	}
	if l.runtime.streams {
		// MochiStream and MochiSub model the Phase 10 pub/sub semantics
		// using one queue per subscriber. Each emit fans out by appending
		// to every subscriber's queue; recv_sub shifts the head. The
		// per-sub `limit` slot enables Phase 10.2 backpressure by
		// dropping incoming messages when the queue is full. All
		// fixtures emit-before-recv synchronously, so no fibers/yields
		// are needed in Phase 10.0.
		out = append(out, &ptree.RawDecl{Text: `
final class MochiStream
{
    /** @var array<int, array<int, mixed>> Per-subscriber message queues. */
    public array $subs = [];

    /** @var array<int, int> Per-subscriber drop threshold; 0 = unlimited. */
    public array $limits = [];

    public function __construct(public int $cap) {}
}`})
		out = append(out, &ptree.RawDecl{Text: `
final class MochiSub
{
    public function __construct(
        public MochiStream $stream,
        public int $idx,
    ) {}
}`})
		out = append(out, &ptree.FuncDecl{
			PhpDoc:     []string{"Phase 10: build a bounded broadcast stream with the given capacity."},
			Name:       "mochi_stream_make",
			Params:     []ptree.FuncParam{{TypeName: "int", Name: "cap"}},
			ReturnType: "MochiStream",
			Body: []ptree.Stmt{
				&ptree.RawStmt{Text: `return new MochiStream(cap: $cap);`},
			},
		})
		out = append(out, &ptree.FuncDecl{
			PhpDoc:     []string{"Phase 10: subscribe to a stream. Each subscriber sees every message emitted after this call."},
			Name:       "mochi_sub_make",
			Params:     []ptree.FuncParam{{TypeName: "MochiStream", Name: "s"}},
			ReturnType: "MochiSub",
			Body: []ptree.Stmt{
				&ptree.RawStmt{Text: `$idx = count($s->subs);`},
				&ptree.RawStmt{Text: `$s->subs[$idx] = [];`},
				&ptree.RawStmt{Text: `$s->limits[$idx] = 0;`},
				&ptree.RawStmt{Text: `return new MochiSub(stream: $s, idx: $idx);`},
			},
		})
		out = append(out, &ptree.FuncDecl{
			PhpDoc:     []string{"Phase 10.2: subscribe with backpressure. The subscriber drops messages when its queue holds $limit items."},
			Name:       "mochi_sub_make_limit",
			Params:     []ptree.FuncParam{{TypeName: "MochiStream", Name: "s"}, {TypeName: "int", Name: "limit"}},
			ReturnType: "MochiSub",
			Body: []ptree.Stmt{
				&ptree.RawStmt{Text: `$idx = count($s->subs);`},
				&ptree.RawStmt{Text: `$s->subs[$idx] = [];`},
				&ptree.RawStmt{Text: `$s->limits[$idx] = $limit;`},
				&ptree.RawStmt{Text: `return new MochiSub(stream: $s, idx: $idx);`},
			},
		})
		out = append(out, &ptree.FuncDecl{
			PhpDoc:     []string{"Phase 10: fan-out emit. Append $v to every subscriber's queue, respecting per-subscriber drop limits."},
			Name:       "mochi_stream_emit",
			Params:     []ptree.FuncParam{{TypeName: "MochiStream", Name: "s"}, {Name: "v"}},
			ReturnType: "void",
			Body: []ptree.Stmt{
				&ptree.RawStmt{Text: `foreach (array_keys($s->subs) as $k) {`},
				&ptree.RawStmt{Text: `    if ($s->limits[$k] > 0 && count($s->subs[$k]) >= $s->limits[$k]) { continue; }`},
				&ptree.RawStmt{Text: `    $s->subs[$k][] = $v;`},
				&ptree.RawStmt{Text: `}`},
			},
		})
		out = append(out, &ptree.FuncDecl{
			PhpDoc:     []string{"Phase 10: shift the next message off a subscriber's queue. All Phase 10 fixtures emit-before-recv so the queue is non-empty here."},
			Name:       "mochi_sub_recv",
			Params:     []ptree.FuncParam{{TypeName: "MochiSub", Name: "sub"}},
			ReturnType: "mixed",
			Body: []ptree.Stmt{
				&ptree.RawStmt{Text: `return array_shift($sub->stream->subs[$sub->idx]);`},
			},
		})
	}
	if l.runtime.async {
		// MochiFuture wraps an already-computed value. All Phase 11
		// fixtures observe deterministic results from sequential
		// computations, so async/await lowers to eager evaluation
		// inside a value wrapper. mochi_future_await_all unwraps a
		// list of futures into a list of values, matching the
		// `__await_all__` builtin contract.
		out = append(out, &ptree.RawDecl{Text: `
final class MochiFuture
{
    public function __construct(public mixed $value) {}
}`})
		out = append(out, &ptree.FuncDecl{
			PhpDoc:     []string{"Phase 11: wrap an eagerly-evaluated value as a future."},
			Name:       "mochi_future_make",
			Params:     []ptree.FuncParam{{Name: "v"}},
			ReturnType: "MochiFuture",
			Body: []ptree.Stmt{
				&ptree.RawStmt{Text: `return new MochiFuture(value: $v);`},
			},
		})
		out = append(out, &ptree.FuncDecl{
			PhpDoc:     []string{"Phase 11: unwrap a future. Eager evaluation means the value is already populated."},
			Name:       "mochi_future_await",
			Params:     []ptree.FuncParam{{TypeName: "MochiFuture", Name: "f"}},
			ReturnType: "mixed",
			Body: []ptree.Stmt{
				&ptree.RawStmt{Text: `return $f->value;`},
			},
		})
		out = append(out, &ptree.FuncDecl{
			PhpDoc:     []string{"Phase 11: unwrap a list of futures into a list of values, mirroring `__await_all__`."},
			Name:       "mochi_future_await_all",
			Params:     []ptree.FuncParam{{TypeName: "array", Name: "fs"}},
			ReturnType: "array",
			Body: []ptree.Stmt{
				&ptree.RawStmt{Text: `return array_map(fn(MochiFuture $f) => $f->value, $fs);`},
			},
		})
	}
	if l.runtime.llm {
		// Phase 13: cassette-only LLM dispatch. Mirrors the C runtime's
		// DJB2-keyed lookup so the same cassette directory works
		// across targets. Each key is the DJB2 hash of the byte
		// sequence "<provider>\0<model>\0<prompt>", base-10. A single
		// trailing newline on the cassette file is stripped so files
		// written with a final newline (normal for text editors) round-
		// trip cleanly. Live providers are deferred to a later phase;
		// missing MOCHI_LLM_CASSETTE_DIR returns "" with a stderr note.
		out = append(out, &ptree.FuncDecl{
			PhpDoc:     []string{"Phase 13: DJB2 hash over `<provider>\\0<model>\\0<prompt>`, computed in uint64 via GMP so cassette keys above PHP_INT_MAX still round-trip."},
			Name:       "mochi_llm_cassette_key",
			Params:     []ptree.FuncParam{{TypeName: "string", Name: "provider"}, {TypeName: "string", Name: "model"}, {TypeName: "string", Name: "prompt"}},
			ReturnType: "string",
			Body: []ptree.Stmt{
				&ptree.RawStmt{Text: `$buf = $provider . "\0" . $model . "\0" . $prompt;`},
				&ptree.RawStmt{Text: `$h = gmp_init(5381);`},
				&ptree.RawStmt{Text: `$mask = gmp_init('FFFFFFFFFFFFFFFF', 16);`},
				&ptree.RawStmt{Text: `$len = strlen($buf);`},
				&ptree.RawStmt{Text: `for ($i = 0; $i < $len; $i++) {`},
				&ptree.RawStmt{Text: `    $h = gmp_and(gmp_mul($h, 33), $mask);`},
				&ptree.RawStmt{Text: `    $h = gmp_xor($h, gmp_init(ord($buf[$i])));`},
				&ptree.RawStmt{Text: `}`},
				&ptree.RawStmt{Text: `return gmp_strval($h, 10);`},
			},
		})
		out = append(out, &ptree.FuncDecl{
			PhpDoc:     []string{"Phase 13: cassette-backed LLM dispatch. Looks up `<MOCHI_LLM_CASSETTE_DIR>/<djb2>.txt` and returns its contents (one trailing newline stripped)."},
			Name:       "mochi_llm_generate",
			Params:     []ptree.FuncParam{{TypeName: "string", Name: "provider"}, {TypeName: "string", Name: "model"}, {TypeName: "string", Name: "prompt"}},
			ReturnType: "string",
			Body: []ptree.Stmt{
				&ptree.RawStmt{Text: `$dir = getenv('MOCHI_LLM_CASSETTE_DIR');`},
				&ptree.RawStmt{Text: `if ($dir === false || $dir === '') {`},
				&ptree.RawStmt{Text: `    fwrite(STDERR, "mochi_llm_generate: MOCHI_LLM_CASSETTE_DIR not set; live mode not yet implemented for PHP\n");`},
				&ptree.RawStmt{Text: `    return '';`},
				&ptree.RawStmt{Text: `}`},
				&ptree.RawStmt{Text: `$key = mochi_llm_cassette_key($provider, $model, $prompt);`},
				&ptree.RawStmt{Text: `$path = rtrim($dir, '/') . '/' . $key . '.txt';`},
				&ptree.RawStmt{Text: `$data = @file_get_contents($path);`},
				&ptree.RawStmt{Text: `if ($data === false) {`},
				&ptree.RawStmt{Text: `    fwrite(STDERR, "mochi_llm_generate: cassette not found: $path\n");`},
				&ptree.RawStmt{Text: `    return '';`},
				&ptree.RawStmt{Text: `}`},
				&ptree.RawStmt{Text: `if ($data !== '' && substr($data, -1) === "\n") { $data = substr($data, 0, -1); }`},
				&ptree.RawStmt{Text: `return $data;`},
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
	case *aotir.MatchStmt:
		return l.lowerMatchStmt(v)
	case *aotir.ClosureEnvStmt:
		// PHP closures capture via the surrounding scope (arrow functions
		// inherit by value automatically), so the env-struct allocation
		// the aotir lowerer emits for the C target is a no-op here.
		return nil, nil
	case *aotir.QueryScopeStmt:
		// Arena scopes are a C-specific optimisation; PHP relies on
		// refcount + cycle collection, so we drop the wrapper and
		// inline the desugared LetStmt/ForEachStmt/append body
		// directly into the surrounding block.
		return l.lowerBlock(v.Body)
	case *aotir.RawCStmt:
		// RawCStmt carries pre-rendered C source for the C backend
		// (currently: Datalog setup). The PHP path runs the Datalog
		// evaluator at compile time inside lowerDatalogQueryExpr and
		// emits a plain array literal of results, so this hint is a
		// no-op here.
		return nil, nil
	case *aotir.StreamEmitStmt:
		// Phase 10: `emit(s, v)` becomes `mochi_stream_emit($s, $v);`.
		l.runtime.streams = true
		s, err := l.lowerExpr(v.Stream)
		if err != nil {
			return nil, err
		}
		val, err := l.lowerExpr(v.Val)
		if err != nil {
			return nil, err
		}
		return []ptree.Stmt{&ptree.ExprStmt{
			Expr: &ptree.CallExpr{
				Callee: &ptree.IdentExpr{Name: "mochi_stream_emit"},
				Args:   []ptree.Expr{s, val},
			},
		}}, nil
	case *aotir.AgentIntentCallStmt:
		// Phase 9: discard-result intent call:
		// `c.increment()` becomes `$c->increment();`.
		recv, err := l.lowerExpr(v.Receiver)
		if err != nil {
			return nil, err
		}
		args := make([]ptree.Expr, 0, len(v.Args))
		for _, a := range v.Args {
			lo, err := l.lowerExpr(a)
			if err != nil {
				return nil, err
			}
			args = append(args, lo)
		}
		return []ptree.Stmt{&ptree.ExprStmt{
			Expr: &ptree.MethodCallExpr{Receiver: recv, Method: v.IntentName, Args: args},
		}}, nil
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
	case *aotir.WriteFileStmt:
		// Phase 12: `writeFile(path, content)` lowers to PHP's built-in
		// `file_put_contents`; the default flag set truncates the file
		// and creates it if missing, matching the Mochi spec.
		path, err := l.lowerExpr(v.Path)
		if err != nil {
			return nil, err
		}
		content, err := l.lowerExpr(v.Content)
		if err != nil {
			return nil, err
		}
		return []ptree.Stmt{&ptree.ExprStmt{Expr: &ptree.CallExpr{
			Callee: &ptree.IdentExpr{Name: "file_put_contents"},
			Args:   []ptree.Expr{path, content},
		}}}, nil
	case *aotir.AppendFileStmt:
		// Phase 12: `appendFile(path, content)` uses the same builtin
		// with FILE_APPEND so the existing bytes aren't truncated.
		path, err := l.lowerExpr(v.Path)
		if err != nil {
			return nil, err
		}
		content, err := l.lowerExpr(v.Content)
		if err != nil {
			return nil, err
		}
		return []ptree.Stmt{&ptree.ExprStmt{Expr: &ptree.CallExpr{
			Callee: &ptree.IdentExpr{Name: "file_put_contents"},
			Args:   []ptree.Expr{path, content, &ptree.IdentExpr{Name: "FILE_APPEND"}},
		}}}, nil
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
	// Phase 9: agent intent bodies refer to mutable state through the
	// C-target sentinel name `__self->FIELD`. Rewrite those writes
	// onto `$this->FIELD` since PHP intent methods dispatch through a
	// concrete instance receiver.
	if field, ok := strings.CutPrefix(s.Name, "__self->"); ok {
		return []ptree.Stmt{&ptree.PropAssignStmt{
			Receiver: &ptree.VarExpr{Name: "this"},
			Field:    field,
			Value:    v,
		}}, nil
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
//
// Phase 6: for lifted closures (fn.IsLifted with captures), prepend the
// capture vars as leading parameters and rewrite the body so that any
// reference to `__e->X` (the aotir env-pointer notation reserved for the
// C target) resolves to the corresponding plain capture name. The
// surrounding FunLit closure passes captures in this same order.
func (l *lowerer) lowerFunction(fn *aotir.Function) (*ptree.FuncDecl, error) {
	bodyBlock := fn.Body
	if fn.IsLifted && len(fn.Captures) > 0 {
		bodyBlock = rewriteEnvRefs(fn.Body, fn.Captures)
	}
	body, err := l.lowerBlock(bodyBlock)
	if err != nil {
		return nil, fmt.Errorf("php lower: in function %q: %w", fn.Name, err)
	}
	params := make([]ptree.FuncParam, 0, len(fn.Captures)+len(fn.Params))
	if fn.IsLifted && len(fn.Captures) > 0 {
		for _, cap := range fn.Captures {
			typeName, err := phpParamType(cap.VarType, "", "")
			if err != nil {
				return nil, fmt.Errorf("php lower: capture %q of %q: %w", cap.FieldName, fn.Name, err)
			}
			params = append(params, ptree.FuncParam{TypeName: typeName, Name: cap.FieldName})
		}
	}
	for _, p := range fn.Params {
		typeName, err := phpParamType(p.Type, p.RecordName, p.UnionName)
		if err != nil {
			return nil, fmt.Errorf("php lower: param %q of %q: %w", p.Name, fn.Name, err)
		}
		params = append(params, ptree.FuncParam{TypeName: typeName, Name: p.Name})
	}
	ret, err := phpParamType(fn.ReturnType, fn.ReturnRecordName, fn.ReturnUnionName)
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

// rewriteEnvRefs returns a deep copy of b with every VarRef whose Name
// starts with "__e->" replaced by a VarRef using just the field name
// after "->". The aotir lowerer encodes capture access as `__e->field`
// (a C-target hint); PHP closures resolve captures through their
// parameter list, so we strip the prefix.
func rewriteEnvRefs(b *aotir.Block, captures []aotir.FunCapture) *aotir.Block {
	renames := make(map[string]string, len(captures))
	for _, cap := range captures {
		renames["__e->"+cap.FieldName] = cap.FieldName
	}
	return rewriteBlockEnvRefs(b, renames)
}

func rewriteBlockEnvRefs(b *aotir.Block, renames map[string]string) *aotir.Block {
	if b == nil {
		return nil
	}
	stmts := make([]aotir.Stmt, len(b.Statements))
	for i, s := range b.Statements {
		stmts[i] = rewriteStmtEnvRefs(s, renames)
	}
	return &aotir.Block{Statements: stmts}
}

func rewriteStmtEnvRefs(s aotir.Stmt, renames map[string]string) aotir.Stmt {
	switch s := s.(type) {
	case *aotir.ReturnStmt:
		if s.Value == nil {
			return s
		}
		cp := *s
		cp.Value = rewriteExprEnvRefs(s.Value, renames)
		return &cp
	case *aotir.LetStmt:
		cp := *s
		if s.Init != nil {
			cp.Init = rewriteExprEnvRefs(s.Init, renames)
		}
		return &cp
	case *aotir.AssignStmt:
		cp := *s
		cp.Value = rewriteExprEnvRefs(s.Value, renames)
		return &cp
	case *aotir.CallStmt:
		cp := *s
		cp.Args = make([]aotir.Expr, len(s.Args))
		for i, a := range s.Args {
			cp.Args[i] = rewriteExprEnvRefs(a, renames)
		}
		return &cp
	case *aotir.IfStmt:
		cp := *s
		cp.Cond = rewriteExprEnvRefs(s.Cond, renames)
		cp.Then = rewriteBlockEnvRefs(s.Then, renames)
		cp.Else = rewriteBlockEnvRefs(s.Else, renames)
		return &cp
	case *aotir.WhileStmt:
		cp := *s
		cp.Cond = rewriteExprEnvRefs(s.Cond, renames)
		cp.Body = rewriteBlockEnvRefs(s.Body, renames)
		return &cp
	case *aotir.ForRangeStmt:
		cp := *s
		cp.Start = rewriteExprEnvRefs(s.Start, renames)
		cp.End = rewriteExprEnvRefs(s.End, renames)
		cp.Body = rewriteBlockEnvRefs(s.Body, renames)
		return &cp
	case *aotir.ForEachStmt:
		cp := *s
		cp.List = rewriteExprEnvRefs(s.List, renames)
		cp.Body = rewriteBlockEnvRefs(s.Body, renames)
		return &cp
	default:
		return s
	}
}

func rewriteExprEnvRefs(e aotir.Expr, renames map[string]string) aotir.Expr {
	if e == nil {
		return nil
	}
	switch e := e.(type) {
	case *aotir.VarRef:
		if newName, ok := renames[e.Name]; ok {
			cp := *e
			cp.Name = newName
			return &cp
		}
		return e
	case *aotir.BinaryExpr:
		cp := *e
		cp.Left = rewriteExprEnvRefs(e.Left, renames)
		cp.Right = rewriteExprEnvRefs(e.Right, renames)
		return &cp
	case *aotir.UnaryExpr:
		cp := *e
		cp.Operand = rewriteExprEnvRefs(e.Operand, renames)
		return &cp
	case *aotir.CallExpr:
		cp := *e
		cp.Args = make([]aotir.Expr, len(e.Args))
		for i, a := range e.Args {
			cp.Args[i] = rewriteExprEnvRefs(a, renames)
		}
		return &cp
	case *aotir.FunCallExpr:
		cp := *e
		cp.Callee = rewriteExprEnvRefs(e.Callee, renames)
		cp.Args = make([]aotir.Expr, len(e.Args))
		for i, a := range e.Args {
			cp.Args[i] = rewriteExprEnvRefs(a, renames)
		}
		return &cp
	default:
		return e
	}
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
// including record class names, sum-type base classes, and collection
// types (Phase 3+, Phase 5 adds the TypeUnion branch).
func phpParamType(t aotir.Type, recordName, unionName string) (string, error) {
	switch t {
	case aotir.TypeRecord:
		if recordName == "" {
			return "", fmt.Errorf("TypeRecord needs a RecordName")
		}
		return recordName, nil
	case aotir.TypeUnion:
		if unionName == "" {
			return "", fmt.Errorf("TypeUnion needs a UnionName")
		}
		return unionName, nil
	case aotir.TypeList, aotir.TypeMap, aotir.TypeSet:
		return "array", nil
	case aotir.TypeStream:
		// Phase 10: every stream is a MochiStream regardless of element
		// type; per-element typing is enforced upstream by the verifier.
		return "MochiStream", nil
	case aotir.TypeSub:
		// Phase 10: every subscriber is a MochiSub; the carried element
		// type is recovered via the parent stream's `subs[$idx]` slot.
		return "MochiSub", nil
	case aotir.TypeFuture:
		// Phase 11: every future is a MochiFuture; the carried element
		// type is recovered via the `value` slot at unwrap time.
		return "MochiFuture", nil
	case aotir.TypeFun:
		// PHP's Closure class is the type emitted for any function-typed
		// value. PHP cannot express a parameterised callable type at the
		// type-declaration site (the `callable` pseudo-type accepts
		// strings/arrays, which is wider than we want); PHPStan/Psalm
		// recover the precise signature from @param/@return tags added
		// in Phase 15.
		return "Closure", nil
	default:
		return phpScalarType(t)
	}
}

// variantClassName builds the PHP class name for one variant of a
// sum type. e.g. union "Shape" variant "Circle" → "Shape_Circle".
// The double-underscore form is reserved for closure env classes
// (Phase 5.0+) so single-underscore here is collision-safe.
func variantClassName(union, variant string) string {
	return union + "_" + variant
}

// phpReservedClassNames lists the PHP keywords that PHP rejects as
// class, interface, or trait names. Mochi user types that collide are
// suffixed with `_` by phpClassName so `agent Switch { ... }` emits
// `final class Switch_` and `new Switch_(...)`.
var phpReservedClassNames = map[string]bool{
	"abstract": true, "and": true, "array": true, "as": true, "break": true,
	"callable": true, "case": true, "catch": true, "class": true, "clone": true,
	"const": true, "continue": true, "declare": true, "default": true, "do": true,
	"echo": true, "else": true, "elseif": true, "empty": true,
	"enddeclare": true, "endfor": true, "endforeach": true, "endif": true,
	"endswitch": true, "endwhile": true, "enum": true, "eval": true, "exit": true,
	"extends": true, "false": true, "final": true, "finally": true, "fn": true,
	"for": true, "foreach": true, "function": true, "global": true, "goto": true,
	"if": true, "implements": true, "include": true, "include_once": true,
	"instanceof": true, "insteadof": true, "interface": true, "isset": true,
	"list": true, "match": true, "namespace": true, "new": true, "null": true,
	"or": true, "print": true, "private": true, "protected": true, "public": true,
	"readonly": true, "require": true, "require_once": true, "return": true,
	"static": true, "switch": true, "throw": true, "trait": true, "true": true,
	"try": true, "unset": true, "use": true, "var": true, "while": true,
	"xor": true, "yield": true,
	// PHP soft-reserved types: cannot name a class after a built-in type.
	"int": true, "float": true, "bool": true, "string": true, "void": true,
	"iterable": true, "object": true, "mixed": true, "never": true, "self": true,
	"parent": true,
}

// phpClassName returns the PHP-safe class name for a Mochi user type.
// Names that collide with a PHP reserved word are suffixed with `_`.
func phpClassName(name string) string {
	if phpReservedClassNames[strings.ToLower(name)] {
		return name + "_"
	}
	return name
}

func (l *lowerer) lowerUnion(u *aotir.UnionDecl) ([]ptree.Decl, error) {
	out := make([]ptree.Decl, 0, 1+len(u.Variants))
	out = append(out, &ptree.ClassDecl{
		Name:     u.Name,
		Abstract: true,
		PhpDoc:   []string{"Mochi sum type `" + u.Name + "` base class. Generated; do not edit by hand."},
	})
	for _, v := range u.Variants {
		fields := make([]ptree.ClassField, 0, len(v.Fields))
		for _, f := range v.Fields {
			typeName, err := phpParamType(f.FieldType, f.RecordName, f.UnionName)
			if err != nil {
				return nil, fmt.Errorf("union %q variant %q field %q: %w", u.Name, v.Name, f.Name, err)
			}
			fields = append(fields, ptree.ClassField{TypeName: typeName, Name: f.Name})
		}
		out = append(out, &ptree.ClassDecl{
			Name:    variantClassName(u.Name, v.Name),
			Extends: u.Name,
			Fields:  fields,
			PhpDoc:  []string{"Variant `" + v.Name + "` of `" + u.Name + "`."},
		})
	}
	return out, nil
}

// lowerFunLit lowers an aotir.FunLit (anonymous function lifted to a
// top-level function during the C-style closure conversion) to a PHP
// arrow function that forwards its arguments to the lifted callee.
//
// For non-capturing closures the result is `fn(int $p0): int =>
// mochi__anon_N($p0)`. For capturing closures, the captures are
// prepended to the call so they appear as the lifted function's
// leading parameters; PHP arrow functions inherit those variable
// names by value from the enclosing scope automatically.
func (l *lowerer) lowerFunLit(e *aotir.FunLit) (ptree.Expr, error) {
	if e.Sig == nil {
		return nil, fmt.Errorf("php lower: FunLit %q missing Sig", e.FuncName)
	}
	params := make([]ptree.FuncParam, len(e.Sig.ParamTypes))
	args := make([]ptree.Expr, 0, len(e.Captures)+len(params))
	for _, cap := range e.Captures {
		args = append(args, &ptree.VarExpr{Name: cap.FieldName})
	}
	for i, pt := range e.Sig.ParamTypes {
		typeName, err := phpParamType(pt, "", "")
		if err != nil {
			return nil, fmt.Errorf("php lower: FunLit %q param %d: %w", e.FuncName, i, err)
		}
		name := fmt.Sprintf("__p%d", i)
		params[i] = ptree.FuncParam{TypeName: typeName, Name: name}
		args = append(args, &ptree.VarExpr{Name: name})
	}
	retType, err := phpParamType(e.Sig.ReturnType, "", "")
	if err != nil {
		return nil, fmt.Errorf("php lower: FunLit %q return type: %w", e.FuncName, err)
	}
	body := &ptree.CallExpr{
		Callee: &ptree.IdentExpr{Name: e.FuncName},
		Args:   args,
	}
	return &ptree.ClosureExpr{
		Params:     params,
		ReturnType: retType,
		Body:       body,
	}, nil
}

// lowerMatchStmt lowers an aotir.MatchStmt to a PHP chained-if. The
// target is evaluated once into a fresh temp; each arm becomes one
// `if ($tmp instanceof Union_Variant) { ... }` branch. Pattern
// bindings are materialised at the top of each arm body as
// `$<VarName> = $tmp-><FieldName>;` so the body's VarRefs resolve to
// concrete locals. Wildcard arms become the trailing `else { ... }`.
//
// Guards (Phase 5.1) are intentionally rejected here; the PHP back-end
// will add them in MEP-55 Phase 5.1.
func (l *lowerer) lowerMatchStmt(s *aotir.MatchStmt) ([]ptree.Stmt, error) {
	target, err := l.lowerExpr(s.Target)
	if err != nil {
		return nil, fmt.Errorf("match target: %w", err)
	}
	l.matchSeq++
	tmp := fmt.Sprintf("__mochi_match_%d", l.matchSeq)
	out := []ptree.Stmt{
		&ptree.AssignStmt{Name: tmp, Value: target},
	}

	branches := make([]ptree.IfBranch, 0, len(s.Arms))
	for _, arm := range s.Arms {
		if arm.Guard != nil {
			return nil, fmt.Errorf("php lower: guarded match arms (Phase 5.1) not yet supported (variant %q)", arm.VariantName)
		}
		cond := &ptree.InstanceOfExpr{
			Receiver:  &ptree.VarExpr{Name: tmp},
			ClassName: variantClassName(s.UnionName, arm.VariantName),
		}
		body := make([]ptree.Stmt, 0, len(arm.Bindings)+8)
		for _, b := range arm.Bindings {
			body = append(body, &ptree.AssignStmt{
				Name: b.VarName,
				Value: &ptree.PropAccessExpr{
					Receiver: &ptree.VarExpr{Name: tmp},
					Field:    b.FieldName,
				},
			})
		}
		armBody, err := l.lowerBlock(arm.Body)
		if err != nil {
			return nil, fmt.Errorf("match arm %q body: %w", arm.VariantName, err)
		}
		body = append(body, armBody...)
		branches = append(branches, ptree.IfBranch{Cond: cond, Body: body})
	}
	var defaultBody []ptree.Stmt
	if s.Default != nil {
		if s.Default.Guard != nil {
			return nil, fmt.Errorf("php lower: guarded wildcard match arm (Phase 5.1) not yet supported")
		}
		db, err := l.lowerBlock(s.Default.Body)
		if err != nil {
			return nil, fmt.Errorf("match default body: %w", err)
		}
		defaultBody = db
	}
	out = append(out, &ptree.ChainedIfStmt{Branches: branches, Default: defaultBody})
	return out, nil
}

func (l *lowerer) lowerRecord(r *aotir.RecordDecl) (*ptree.ClassDecl, error) {
	fields := make([]ptree.ClassField, 0, len(r.Fields))
	for _, f := range r.Fields {
		typeName, err := phpParamType(f.Type, f.RecordName, "")
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

// lowerAgent emits one mutable PHP class per agent declaration. Fields
// become promoted public constructor parameters; intents become public
// instance methods. The aotir lowerer rewrites intent-body field
// references to the sentinel name `__self->FIELD`; lowerExpr/VarRef and
// lowerAssignStmt translate those into `$this->FIELD`.
func (l *lowerer) lowerAgent(ag *aotir.AgentDecl) (*ptree.ClassDecl, error) {
	fields := make([]ptree.ClassField, 0, len(ag.Fields))
	for _, f := range ag.Fields {
		typeName, err := phpScalarType(f.Type)
		if err != nil {
			return nil, fmt.Errorf("agent %q field %q: %w", ag.Name, f.Name, err)
		}
		fields = append(fields, ptree.ClassField{TypeName: typeName, Name: f.Name})
	}
	methods := make([]ptree.MethodDecl, 0, len(ag.Intents))
	for i := range ag.Intents {
		intent := &ag.Intents[i]
		params := make([]ptree.FuncParam, 0, len(intent.Params))
		for _, p := range intent.Params {
			pt, err := phpScalarType(p.Type)
			if err != nil {
				return nil, fmt.Errorf("agent %q intent %q param %q: %w", ag.Name, intent.Name, p.Name, err)
			}
			params = append(params, ptree.FuncParam{TypeName: pt, Name: p.Name})
		}
		ret, err := phpScalarType(intent.ReturnType)
		if err != nil {
			return nil, fmt.Errorf("agent %q intent %q return: %w", ag.Name, intent.Name, err)
		}
		body, err := l.lowerBlock(intent.Body)
		if err != nil {
			return nil, fmt.Errorf("agent %q intent %q body: %w", ag.Name, intent.Name, err)
		}
		methods = append(methods, ptree.MethodDecl{
			Name:       intent.Name,
			Params:     params,
			ReturnType: ret,
			Body:       body,
		})
	}
	return &ptree.ClassDecl{
		Name:    phpClassName(ag.Name),
		Fields:  fields,
		Methods: methods,
		Mutable: true,
		PhpDoc:  []string{"Mochi agent `" + ag.Name + "`. Generated; do not edit by hand."},
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
		// Phase 9: agent intent bodies read mutable state through the
		// C-target sentinel name `__self->FIELD`. Rewrite into a PHP
		// `$this->FIELD` property access.
		if field, ok := strings.CutPrefix(v.Name, "__self->"); ok {
			return &ptree.PropAccessExpr{
				Receiver: &ptree.VarExpr{Name: "this"},
				Field:    field,
			}, nil
		}
		return &ptree.VarExpr{Name: v.Name}, nil
	case *aotir.UnionVarRef:
		return &ptree.VarExpr{Name: v.Name}, nil
	case *aotir.VariantLit:
		args := make([]ptree.NamedArg, 0, len(v.Fields))
		for _, f := range v.Fields {
			val, err := l.lowerExpr(f.Value)
			if err != nil {
				return nil, err
			}
			args = append(args, ptree.NamedArg{Name: f.Name, Value: val})
		}
		return &ptree.NewExpr{
			Class: variantClassName(v.UnionName, v.VariantName),
			Args:  args,
		}, nil
	case *aotir.VariantFieldAccess:
		// VariantFieldAccess outside a match arm is unusual but
		// well-defined: the receiver is known to hold a specific
		// variant, so we just access the field directly. PHP's
		// dynamic dispatch handles the prop lookup at runtime.
		recv, err := l.lowerExpr(v.Receiver)
		if err != nil {
			return nil, err
		}
		return &ptree.PropAccessExpr{Receiver: recv, Field: v.FieldName}, nil
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
		if v.Func == "__await_all__" {
			// Phase 11: the IR carries `await_all(futs)` as a magic
			// CallExpr; lower it to `mochi_future_await_all(...)` which
			// array-maps the values out of the futures.
			l.runtime.async = true
			return &ptree.CallExpr{
				Callee: &ptree.IdentExpr{Name: "mochi_future_await_all"},
				Args:   args,
			}, nil
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
	case *aotir.FunLit:
		return l.lowerFunLit(v)
	case *aotir.FunCallExpr:
		callee, err := l.lowerExpr(v.Callee)
		if err != nil {
			return nil, err
		}
		args := make([]ptree.Expr, 0, len(v.Args))
		for _, a := range v.Args {
			lo, err := l.lowerExpr(a)
			if err != nil {
				return nil, err
			}
			args = append(args, lo)
		}
		// PHP's `$callee($args...)` shorthand invokes any Closure /
		// callable value without an extra dispatch helper.
		return &ptree.CallExpr{Callee: callee, Args: args}, nil
	case *aotir.ListMapExpr:
		list, err := l.lowerExpr(v.List)
		if err != nil {
			return nil, err
		}
		fn, err := l.lowerExpr(v.Fn)
		if err != nil {
			return nil, err
		}
		// array_map($fn, $xs): PHP's call order is (callable, array),
		// the reverse of Mochi's map(xs, fn). The result preserves
		// the numeric keys 0..n-1, matching Mochi list semantics.
		return &ptree.CallExpr{
			Callee: &ptree.IdentExpr{Name: "array_map"},
			Args:   []ptree.Expr{fn, list},
		}, nil
	case *aotir.ListFilterExpr:
		list, err := l.lowerExpr(v.List)
		if err != nil {
			return nil, err
		}
		fn, err := l.lowerExpr(v.Fn)
		if err != nil {
			return nil, err
		}
		// array_filter preserves the original keys, so wrap with
		// array_values to re-pack 0..k-1 and match Mochi list shape.
		filtered := &ptree.CallExpr{
			Callee: &ptree.IdentExpr{Name: "array_filter"},
			Args:   []ptree.Expr{list, fn},
		}
		return &ptree.CallExpr{
			Callee: &ptree.IdentExpr{Name: "array_values"},
			Args:   []ptree.Expr{filtered},
		}, nil
	case *aotir.ListFoldlExpr:
		list, err := l.lowerExpr(v.List)
		if err != nil {
			return nil, err
		}
		fn, err := l.lowerExpr(v.Fn)
		if err != nil {
			return nil, err
		}
		init, err := l.lowerExpr(v.Init)
		if err != nil {
			return nil, err
		}
		// array_reduce($xs, $fn, $init) calls $fn(carry, item) which
		// matches Mochi's reduce(xs, fun(acc, x) => ..., init) ordering.
		return &ptree.CallExpr{
			Callee: &ptree.IdentExpr{Name: "array_reduce"},
			Args:   []ptree.Expr{list, fn, init},
		}, nil
	case *aotir.ListSortAscExpr:
		l.runtime.listSortAsc = true
		recv, err := l.lowerExpr(v.Receiver)
		if err != nil {
			return nil, err
		}
		// Inline runtime helper takes the array by value so the
		// caller's slot is unchanged; usort inside the helper sorts
		// the local copy with the spaceship operator.
		return &ptree.CallExpr{
			Callee: &ptree.IdentExpr{Name: "mochi_list_sort_asc"},
			Args:   []ptree.Expr{recv},
		}, nil
	case *aotir.ListSliceExpr:
		recv, err := l.lowerExpr(v.Receiver)
		if err != nil {
			return nil, err
		}
		start, err := l.lowerExpr(v.Start)
		if err != nil {
			return nil, err
		}
		end, err := l.lowerExpr(v.End)
		if err != nil {
			return nil, err
		}
		// array_slice($xs, $start, $length): length = end - start.
		// array_slice clamps length to the underlying array size, so
		// over-large $end (e.g. INT_MAX from `take` with no upper bound)
		// is handled without an explicit guard.
		length := &ptree.BinaryExpr{Op: "-", Left: end, Right: start}
		return &ptree.CallExpr{
			Callee: &ptree.IdentExpr{Name: "array_slice"},
			Args:   []ptree.Expr{recv, start, length},
		}, nil
	case *aotir.StreamMakeExpr:
		// Phase 10: `make_stream(N)` becomes `mochi_stream_make($N)`.
		// The capacity is currently advisory; all Phase 10 fixtures are
		// emit-before-recv so no blocking happens.
		l.runtime.streams = true
		cap, err := l.lowerExpr(v.Cap)
		if err != nil {
			return nil, err
		}
		return &ptree.CallExpr{
			Callee: &ptree.IdentExpr{Name: "mochi_stream_make"},
			Args:   []ptree.Expr{cap},
		}, nil
	case *aotir.SubMakeExpr:
		// Phase 10: `subscribe(s)` becomes `mochi_sub_make($s)`. Each
		// call allocates a fresh per-subscriber queue inside the stream.
		l.runtime.streams = true
		s, err := l.lowerExpr(v.Stream)
		if err != nil {
			return nil, err
		}
		return &ptree.CallExpr{
			Callee: &ptree.IdentExpr{Name: "mochi_sub_make"},
			Args:   []ptree.Expr{s},
		}, nil
	case *aotir.SubMakeLimitExpr:
		// Phase 10.2: `subscribe_limit(s, n)` becomes
		// `mochi_sub_make_limit($s, $n)`; the helper records the drop
		// threshold so subsequent emits skip a full subscriber queue.
		l.runtime.streams = true
		s, err := l.lowerExpr(v.Stream)
		if err != nil {
			return nil, err
		}
		lim, err := l.lowerExpr(v.Limit)
		if err != nil {
			return nil, err
		}
		return &ptree.CallExpr{
			Callee: &ptree.IdentExpr{Name: "mochi_sub_make_limit"},
			Args:   []ptree.Expr{s, lim},
		}, nil
	case *aotir.SubRecvExpr:
		// Phase 10: `recv_sub(sub)` becomes `mochi_sub_recv($sub)`,
		// which shifts the head off the subscriber's queue.
		l.runtime.streams = true
		s, err := l.lowerExpr(v.Sub)
		if err != nil {
			return nil, err
		}
		return &ptree.CallExpr{
			Callee: &ptree.IdentExpr{Name: "mochi_sub_recv"},
			Args:   []ptree.Expr{s},
		}, nil
	case *aotir.HttpGetExpr:
		// Phase 14: `fetch(url)` maps to PHP's `file_get_contents`,
		// which natively supports both `http://` and `file://` URLs
		// via PHP's stream wrappers. All Phase 14 fixtures use
		// `file://` schemes so the dependency surface is the same as
		// Phase 12; HTTP wrappers ship with the default PHP build.
		url, err := l.lowerExpr(v.URL)
		if err != nil {
			return nil, err
		}
		return &ptree.CallExpr{
			Callee: &ptree.IdentExpr{Name: "file_get_contents"},
			Args:   []ptree.Expr{url},
		}, nil
	case *aotir.LLMGenerateExpr:
		// Phase 13: `generate openai { prompt: ... }` lowers to a
		// runtime helper that reads a cassette file keyed by the DJB2
		// hash of `<provider>\0<model>\0<prompt>`. Live providers are
		// deferred; all Phase 13 fixtures supply a cassette directory.
		l.runtime.llm = true
		model, err := l.lowerExpr(v.Model)
		if err != nil {
			return nil, err
		}
		prompt, err := l.lowerExpr(v.Prompt)
		if err != nil {
			return nil, err
		}
		return &ptree.CallExpr{
			Callee: &ptree.IdentExpr{Name: "mochi_llm_generate"},
			Args: []ptree.Expr{
				&ptree.StringLit{Value: v.Provider},
				model,
				prompt,
			},
		}, nil
	case *aotir.ReadFileExpr:
		// Phase 12: `readFile(path)` becomes PHP's
		// `file_get_contents($path)`. PHP returns the raw bytes which
		// is what Mochi's string semantics expect (no transcoding).
		path, err := l.lowerExpr(v.Path)
		if err != nil {
			return nil, err
		}
		return &ptree.CallExpr{
			Callee: &ptree.IdentExpr{Name: "file_get_contents"},
			Args:   []ptree.Expr{path},
		}, nil
	case *aotir.LinesExpr:
		// Phase 12: `lines(path)` becomes
		// `file($path, FILE_IGNORE_NEW_LINES)`. The flag strips the
		// trailing newline from each entry; a trailing newline at end
		// of file is not emitted as an empty entry, matching the C
		// runtime's mochi_lines semantics.
		path, err := l.lowerExpr(v.Path)
		if err != nil {
			return nil, err
		}
		return &ptree.CallExpr{
			Callee: &ptree.IdentExpr{Name: "file"},
			Args:   []ptree.Expr{path, &ptree.IdentExpr{Name: "FILE_IGNORE_NEW_LINES"}},
		}, nil
	case *aotir.AsyncExpr:
		// Phase 11: `async { body }` becomes
		// `mochi_future_make(<body>)`. All Phase 11 fixtures observe
		// deterministic results from sequential computations, so we
		// evaluate the body eagerly and wrap the value. The MochiFuture
		// class records the eagerly-computed value for later await.
		l.runtime.async = true
		body, err := l.lowerExpr(v.Body)
		if err != nil {
			return nil, err
		}
		return &ptree.CallExpr{
			Callee: &ptree.IdentExpr{Name: "mochi_future_make"},
			Args:   []ptree.Expr{body},
		}, nil
	case *aotir.AwaitExpr:
		// Phase 11: `await f` becomes `mochi_future_await($f)`, which
		// returns the eagerly-stored value out of the MochiFuture.
		l.runtime.async = true
		fut, err := l.lowerExpr(v.Future)
		if err != nil {
			return nil, err
		}
		return &ptree.CallExpr{
			Callee: &ptree.IdentExpr{Name: "mochi_future_await"},
			Args:   []ptree.Expr{fut},
		}, nil
	case *aotir.AgentLit:
		// Phase 9: `Counter { count: 0 }` becomes
		// `new Counter(count: 0)`. PHP 8.0+ named args let us emit the
		// fields in declaration order without an extra reorder pass.
		args := make([]ptree.NamedArg, 0, len(v.Fields))
		for _, f := range v.Fields {
			val, err := l.lowerExpr(f.Value)
			if err != nil {
				return nil, fmt.Errorf("agent lit %q field %q: %w", v.AgentName, f.Name, err)
			}
			args = append(args, ptree.NamedArg{Name: f.Name, Value: val})
		}
		return &ptree.NewExpr{Class: phpClassName(v.AgentName), Args: args}, nil
	case *aotir.AgentIntentCallExpr:
		// Phase 9: synchronous intent call returning a value:
		// `c.value()` becomes `$c->value()`. The class method directly
		// mutates the receiver instance, matching the in-place semantics.
		recv, err := l.lowerExpr(v.Receiver)
		if err != nil {
			return nil, err
		}
		args := make([]ptree.Expr, 0, len(v.Args))
		for _, a := range v.Args {
			lo, err := l.lowerExpr(a)
			if err != nil {
				return nil, err
			}
			args = append(args, lo)
		}
		return &ptree.MethodCallExpr{Receiver: recv, Method: v.IntentName, Args: args}, nil
	case *aotir.DatalogQueryExpr:
		// Phase 8: run the semi-naive bottom-up Datalog evaluator at
		// compile time (matching the BEAM backend's strategy) and emit
		// a static PHP array literal of result strings. This dodges
		// shipping a runtime engine and keeps the generated code tiny.
		results := datalogEval(v)
		elems := make([]ptree.Expr, 0, len(results))
		for _, r := range results {
			elems = append(elems, &ptree.StringLit{Value: r})
		}
		return &ptree.ArrayLit{Elems: elems}, nil
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
