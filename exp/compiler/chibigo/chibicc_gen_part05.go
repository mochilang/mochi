package main

import (
	"modernc.org/libc"
	"unsafe"
)

func parse_typedef(tls *libc.TLS, tok uintptr, basety uintptr) uintptr { /* parse.c:3131:14: */
	bp := tls.Alloc(8)
	defer tls.Free(8)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	var first uint8 = uint8(1)

	for !(consume(tls, bp, *(*uintptr)(unsafe.Pointer(bp)), ts+8876) != 0) {
		if !(first != 0) {
			*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8331)
		}
		first = uint8(0)

		var ty uintptr = declarator(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)), basety)
		if !(int32((*Type)(unsafe.Pointer(ty)).name) != 0) {
			error_tok(tls, (*Type)(unsafe.Pointer(ty)).name_pos, ts+9998, 0)
		}
		(*VarScope)(unsafe.Pointer(push_scope(tls, get_ident(tls, (*Type)(unsafe.Pointer(ty)).name)))).type_def = ty
	}
	return *(*uintptr)(unsafe.Pointer(bp /* tok */))
}

func create_param_lvars(tls *libc.TLS, param uintptr) { /* parse.c:3147:13: */
	if param != 0 {
		create_param_lvars(tls, (*Type)(unsafe.Pointer(param)).next)
		if !(int32((*Type)(unsafe.Pointer(param)).name) != 0) {
			error_tok(tls, (*Type)(unsafe.Pointer(param)).name_pos, ts+10019, 0)
		}
		new_lvar(tls, get_ident(tls, (*Type)(unsafe.Pointer(param)).name), param)
	}
}

// This function matches gotos or labels-as-values with labels.
//
// We cannot resolve gotos as we parse a function because gotos
// can refer a label that appears later in the function.
// So, we need to do this after we parse the entire function.
func resolve_goto_labels(tls *libc.TLS) { /* parse.c:3161:13: */
	{
		var x uintptr = gotos
		for ; x != 0; x = (*Node)(unsafe.Pointer(x)).goto_next {
			{
				var y uintptr = labels
				for ; y != 0; y = (*Node)(unsafe.Pointer(y)).goto_next {
					if !(libc.Xstrcmp(tls, (*Node)(unsafe.Pointer(x)).label, (*Node)(unsafe.Pointer(y)).label) != 0) {
						(*Node)(unsafe.Pointer(x)).unique_label = (*Node)(unsafe.Pointer(y)).unique_label
						break
					}
				}
			}

			if (*Node)(unsafe.Pointer(x)).unique_label == uintptr(0) {
				error_tok(tls, (*Token)(unsafe.Pointer((*Node)(unsafe.Pointer(x)).tok)).next, ts+10042, 0)
			}
		}
	}

	gotos = libc.AssignPtrUintptr(uintptr(unsafe.Pointer(&labels)), uintptr(0))
}

func find_func(tls *libc.TLS, name uintptr) uintptr { /* parse.c:3177:12: */
	var sc uintptr = scope
	for (*Scope)(unsafe.Pointer(sc)).next != 0 {
		sc = (*Scope)(unsafe.Pointer(sc)).next
	}

	var sc2 uintptr = hashmap_get(tls, sc+8, name)
	if sc2 != 0 && (*VarScope)(unsafe.Pointer(sc2)).__var != 0 && (*Obj)(unsafe.Pointer((*VarScope)(unsafe.Pointer(sc2)).__var)).is_function != 0 {
		return (*VarScope)(unsafe.Pointer(sc2)).__var
	}
	return uintptr(0)
}

func mark_live(tls *libc.TLS, var1 uintptr) { /* parse.c:3188:13: */
	if !(int32((*Obj)(unsafe.Pointer(var1)).is_function) != 0) || (*Obj)(unsafe.Pointer(var1)).is_live != 0 {
		return
	}
	(*Obj)(unsafe.Pointer(var1)).is_live = uint8(1)

	{
		var i int32 = 0
		for ; i < (*Obj)(unsafe.Pointer(var1)).refs.len; i++ {
			var fn uintptr = find_func(tls, *(*uintptr)(unsafe.Pointer((*Obj)(unsafe.Pointer(var1)).refs.data + uintptr(i)*8)))
			if fn != 0 {
				mark_live(tls, fn)
			}
		}
	}
}

func function(tls *libc.TLS, tok uintptr, basety uintptr, attr uintptr) uintptr { /* parse.c:3200:14: */
	bp := tls.Alloc(16)
	defer tls.Free(16)
	*(*uintptr)(unsafe.Pointer(bp + 8)) = tok

	var ty uintptr = declarator(tls, bp+8, *(*uintptr)(unsafe.Pointer(bp + 8 /* tok */)), basety)
	if !(int32((*Type)(unsafe.Pointer(ty)).name) != 0) {
		error_tok(tls, (*Type)(unsafe.Pointer(ty)).name_pos, ts+10066, 0)
	}
	var name_str uintptr = get_ident(tls, (*Type)(unsafe.Pointer(ty)).name)

	var fn uintptr = find_func(tls, name_str)
	if fn != 0 {
		// Redeclaration
		if !(int32((*Obj)(unsafe.Pointer(fn)).is_function) != 0) {
			error_tok(tls, *(*uintptr)(unsafe.Pointer(bp + 8 /* tok */)), ts+10088, 0)
		}
		if (*Obj)(unsafe.Pointer(fn)).is_definition != 0 && equal(tls, *(*uintptr)(unsafe.Pointer(bp + 8)), ts+8837) != 0 {
			error_tok(tls, *(*uintptr)(unsafe.Pointer(bp + 8 /* tok */)), ts+10129, libc.VaList(bp, name_str))
		}
		if !(int32((*Obj)(unsafe.Pointer(fn)).is_static) != 0) && (*VarAttr)(unsafe.Pointer(attr)).is_static != 0 {
			error_tok(tls, *(*uintptr)(unsafe.Pointer(bp + 8 /* tok */)), ts+10148, 0)
		}
		(*Obj)(unsafe.Pointer(fn)).is_definition = uint8(libc.Bool32((*Obj)(unsafe.Pointer(fn)).is_definition != 0 || equal(tls, *(*uintptr)(unsafe.Pointer(bp + 8)), ts+8837) != 0))
	} else {
		fn = new_gvar(tls, name_str, ty)
		(*Obj)(unsafe.Pointer(fn)).is_function = uint8(1)
		(*Obj)(unsafe.Pointer(fn)).is_definition = equal(tls, *(*uintptr)(unsafe.Pointer(bp + 8 /* tok */)), ts+8837)
		(*Obj)(unsafe.Pointer(fn)).is_static = uint8(libc.Bool32((*VarAttr)(unsafe.Pointer(attr)).is_static != 0 || (*VarAttr)(unsafe.Pointer(attr)).is_inline != 0 && !(int32((*VarAttr)(unsafe.Pointer(attr)).is_extern) != 0)))
		(*Obj)(unsafe.Pointer(fn)).is_inline = (*VarAttr)(unsafe.Pointer(attr)).is_inline
	}

	(*Obj)(unsafe.Pointer(fn)).is_root = libc.BoolUint8(!((*Obj)(unsafe.Pointer(fn)).is_static != 0 && (*Obj)(unsafe.Pointer(fn)).is_inline != 0))

	if consume(tls, bp+8, *(*uintptr)(unsafe.Pointer(bp + 8)), ts+8876) != 0 {
		return *(*uintptr)(unsafe.Pointer(bp + 8 /* tok */))
	}

	current_fn1 = fn
	locals = uintptr(0)
	enter_scope(tls)
	create_param_lvars(tls, (*Type)(unsafe.Pointer(ty)).params)

	// A buffer for a struct/union return value is passed
	// as the hidden first parameter.
	var rty uintptr = (*Type)(unsafe.Pointer(ty)).return_ty
	if ((*Type)(unsafe.Pointer(rty)).kind == TY_STRUCT || (*Type)(unsafe.Pointer(rty)).kind == TY_UNION) && (*Type)(unsafe.Pointer(rty)).size > 16 {
		new_lvar(tls, ts+8875, pointer_to(tls, rty))
	}

	(*Obj)(unsafe.Pointer(fn)).params = locals

	if (*Type)(unsafe.Pointer(ty)).is_variadic != 0 {
		(*Obj)(unsafe.Pointer(fn)).va_area = new_lvar(tls, ts+10200, array_of(tls, ty_char, 136))
	}
	(*Obj)(unsafe.Pointer(fn)).alloca_bottom = new_lvar(tls, ts+10212, pointer_to(tls, ty_char))

	*(*uintptr)(unsafe.Pointer(bp + 8 /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp + 8 /* tok */)), ts+8837)

	// [https://www.sigbus.info/n1570#6.4.2.2p1] "__func__" is
	// automatically defined as a local variable containing the
	// current function name.
	(*VarScope)(unsafe.Pointer(push_scope(tls, ts+10228))).__var = new_string_literal(tls, (*Obj)(unsafe.Pointer(fn)).name, array_of(tls, ty_char, int32(libc.Xstrlen(tls, (*Obj)(unsafe.Pointer(fn)).name)+uint64(1))))

	// [GNU] __FUNCTION__ is yet another name of __func__.
	(*VarScope)(unsafe.Pointer(push_scope(tls, ts+10237))).__var = new_string_literal(tls, (*Obj)(unsafe.Pointer(fn)).name, array_of(tls, ty_char, int32(libc.Xstrlen(tls, (*Obj)(unsafe.Pointer(fn)).name)+uint64(1))))

	(*Obj)(unsafe.Pointer(fn)).body = compound_stmt(tls, bp+8, *(*uintptr)(unsafe.Pointer(bp + 8 /* tok */)))
	(*Obj)(unsafe.Pointer(fn)).locals = locals
	leave_scope(tls)
	resolve_goto_labels(tls)
	return *(*uintptr)(unsafe.Pointer(bp + 8 /* tok */))
}

func global_variable(tls *libc.TLS, tok uintptr, basety uintptr, attr uintptr) uintptr { /* parse.c:3265:14: */
	bp := tls.Alloc(8)
	defer tls.Free(8)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	var first uint8 = uint8(1)

	for !(consume(tls, bp, *(*uintptr)(unsafe.Pointer(bp)), ts+8876) != 0) {
		if !(first != 0) {
			*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8331)
		}
		first = uint8(0)

		var ty uintptr = declarator(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)), basety)
		if !(int32((*Type)(unsafe.Pointer(ty)).name) != 0) {
			error_tok(tls, (*Type)(unsafe.Pointer(ty)).name_pos, ts+8901, 0)
		}

		var var1 uintptr = new_gvar(tls, get_ident(tls, (*Type)(unsafe.Pointer(ty)).name), ty)
		(*Obj)(unsafe.Pointer(var1)).is_definition = libc.BoolUint8(!((*VarAttr)(unsafe.Pointer(attr)).is_extern != 0))
		(*Obj)(unsafe.Pointer(var1)).is_static = (*VarAttr)(unsafe.Pointer(attr)).is_static
		(*Obj)(unsafe.Pointer(var1)).is_tls = (*VarAttr)(unsafe.Pointer(attr)).is_tls
		if (*VarAttr)(unsafe.Pointer(attr)).align != 0 {
			(*Obj)(unsafe.Pointer(var1)).align = (*VarAttr)(unsafe.Pointer(attr)).align
		}

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8873) != 0 {
			gvar_initializer(tls, bp, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next, var1)
		} else if !(int32((*VarAttr)(unsafe.Pointer(attr)).is_extern) != 0) && !(int32((*VarAttr)(unsafe.Pointer(attr)).is_tls) != 0) {
			(*Obj)(unsafe.Pointer(var1)).is_tentative = uint8(1)
		}
	}
	return *(*uintptr)(unsafe.Pointer(bp /* tok */))
}

// Lookahead tokens and returns true if a given token is a start
// of a function definition or declaration.
func is_function(tls *libc.TLS, tok uintptr) uint8 { /* parse.c:3294:13: */
	bp := tls.Alloc(128)
	defer tls.Free(128)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8876) != 0 {
		return uint8(0)
	}

	*(*Type)(unsafe.Pointer(bp + 8 /* dummy */)) = Type{}
	var ty uintptr = declarator(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)), bp+8)
	return uint8(libc.Bool32((*Type)(unsafe.Pointer(ty)).kind == TY_FUNC))
}

// Remove redundant tentative definitions.
func scan_globals(tls *libc.TLS) { /* parse.c:3304:13: */
	bp := tls.Alloc(144)
	defer tls.Free(144)

	// var head Obj at bp, 144

	var cur uintptr = bp /* &head */

	{
		var var1 uintptr = globals
		for ; var1 != 0; var1 = (*Obj)(unsafe.Pointer(var1)).next {
			if !(int32((*Obj)(unsafe.Pointer(var1)).is_tentative) != 0) {
				cur = libc.AssignPtrUintptr(cur, var1)
				continue
			}

			// Find another definition of the same identifier.
			var var2 uintptr = globals
			for ; var2 != 0; var2 = (*Obj)(unsafe.Pointer(var2)).next {
				if var1 != var2 && (*Obj)(unsafe.Pointer(var2)).is_definition != 0 && !(libc.Xstrcmp(tls, (*Obj)(unsafe.Pointer(var1)).name, (*Obj)(unsafe.Pointer(var2)).name) != 0) {
					break
				}
			}

			// If there's another definition, the tentative definition
			// is redundant
			if !(var2 != 0) {
				cur = libc.AssignPtrUintptr(cur, var1)
			}
		}
	}

	(*Obj)(unsafe.Pointer(cur)).next = uintptr(0)
	globals = (*Obj)(unsafe.Pointer(bp /* &head */)).next
}

func declare_builtin_functions(tls *libc.TLS) { /* parse.c:3330:13: */
	var ty uintptr = func_type(tls, pointer_to(tls, ty_void))
	(*Type)(unsafe.Pointer(ty)).params = copy_type(tls, ty_int)
	builtin_alloca1 = new_gvar(tls, ts+4885, ty)
	(*Obj)(unsafe.Pointer(builtin_alloca1)).is_definition = uint8(0)
}

// program = (typedef | function-definition | global-variable)*
func parse(tls *libc.TLS, tok uintptr) uintptr { /* parse.c:3338:5: */
	bp := tls.Alloc(20)
	defer tls.Free(20)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	declare_builtin_functions(tls)
	globals = uintptr(0)

	for (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp)))).kind != TK_EOF {
		*(*VarAttr)(unsafe.Pointer(bp + 8 /* attr */)) = VarAttr{}
		var basety uintptr = declspec(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)), bp+8)

		// Typedef
		if (*VarAttr)(unsafe.Pointer(bp+8)).is_typedef != 0 {
			*(*uintptr)(unsafe.Pointer(bp /* tok */)) = parse_typedef(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), basety)
			continue
		}

		// Function
		if is_function(tls, *(*uintptr)(unsafe.Pointer(bp))) != 0 {
			*(*uintptr)(unsafe.Pointer(bp /* tok */)) = function(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), basety, bp+8)
			continue
		}

		// Global variable
		*(*uintptr)(unsafe.Pointer(bp /* tok */)) = global_variable(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), basety, bp+8)
	}

	{
		var var1 uintptr = globals
		for ; var1 != 0; var1 = (*Obj)(unsafe.Pointer(var1)).next {
			if (*Obj)(unsafe.Pointer(var1)).is_root != 0 {
				mark_live(tls, var1)
			}
		}
	}

	// Remove redundant tentative definitions.
	scan_globals(tls)
	return globals
}

type Hideset = Hideset1 /* chibicc.h:33:24 */

type MacroParam1 = struct {
	next uintptr
	name uintptr
} /* preprocess.c:27:9 */

type MacroParam = MacroParam1 /* preprocess.c:27:27 */

type MacroArg1 = struct {
	next       uintptr
	name       uintptr
	is_va_args uint8
	_          [7]byte
	tok        uintptr
} /* preprocess.c:33:9 */

type MacroArg = MacroArg1 /* preprocess.c:33:25 */

type Macro1 = struct {
	name         uintptr
	is_objlike   uint8
	_            [7]byte
	params       uintptr
	va_args_name uintptr
	body         uintptr
	handler      uintptr
} /* preprocess.c:43:9 */

type Macro = Macro1 /* preprocess.c:43:22 */

// `#if` can be nested, so we use a stack to manage nested `#if`s.
type CondIncl1 = struct {
	next     uintptr
	ctx      uint32
	_        [4]byte
	tok      uintptr
	included uint8
	_        [7]byte
} /* preprocess.c:54:9 */

// `#if` can be nested, so we use a stack to manage nested `#if`s.
type CondIncl = CondIncl1 /* preprocess.c:54:25 */
const (                   /* preprocess.c:55:1: */
	IN_THEN = 0
	IN_ELIF = 1
	IN_ELSE = 2
)

var macros HashMap         /* preprocess.c:68:16: */
var cond_incl uintptr      /* preprocess.c:69:17: */
var pragma_once HashMap    /* preprocess.c:70:16: */
var include_next_idx int32 /* preprocess.c:71:12: */

func is_hash(tls *libc.TLS, tok uintptr) uint8 { /* preprocess.c:76:13: */
	return uint8(libc.Bool32((*Token)(unsafe.Pointer(tok)).at_bol != 0 && equal(tls, tok, ts+10250) != 0))
}

// Some preprocessor directives such as #include allow extraneous
// tokens before newline. This function skips such tokens.
func skip_line(tls *libc.TLS, tok uintptr) uintptr { /* preprocess.c:82:14: */
	if (*Token)(unsafe.Pointer(tok)).at_bol != 0 {
		return tok
	}
	warn_tok(tls, tok, ts+10252, 0)
	for (*Token)(unsafe.Pointer(tok)).at_bol != 0 {
		tok = (*Token)(unsafe.Pointer(tok)).next
	}
	return tok
}

func copy_token(tls *libc.TLS, tok uintptr) uintptr { /* preprocess.c:91:14: */
	var t uintptr = libc.Xcalloc(tls, uint64(1), uint64(unsafe.Sizeof(Token{})))
	*(*Token)(unsafe.Pointer(t)) = *(*Token)(unsafe.Pointer(tok))
	(*Token)(unsafe.Pointer(t)).next = uintptr(0)
	return t
}

func new_eof(tls *libc.TLS, tok uintptr) uintptr { /* preprocess.c:98:14: */
	var t uintptr = copy_token(tls, tok)
	(*Token)(unsafe.Pointer(t)).kind = TK_EOF
	(*Token)(unsafe.Pointer(t)).len = 0
	return t
}

func new_hideset(tls *libc.TLS, name uintptr) uintptr { /* preprocess.c:105:16: */
	var hs uintptr = libc.Xcalloc(tls, uint64(1), uint64(unsafe.Sizeof(Hideset{})))
	(*Hideset)(unsafe.Pointer(hs)).name = name
	return hs
}

func hideset_union(tls *libc.TLS, hs1 uintptr, hs2 uintptr) uintptr { /* preprocess.c:111:16: */
	bp := tls.Alloc(16)
	defer tls.Free(16)

	*(*Hideset)(unsafe.Pointer(bp /* head */)) = Hideset{}
	var cur uintptr = bp /* &head */

	for ; hs1 != 0; hs1 = (*Hideset)(unsafe.Pointer(hs1)).next {
		cur = libc.AssignPtrUintptr(cur, new_hideset(tls, (*Hideset)(unsafe.Pointer(hs1)).name))
	}
	(*Hideset)(unsafe.Pointer(cur)).next = hs2
	return (*Hideset)(unsafe.Pointer(bp /* &head */)).next
}

func hideset_contains(tls *libc.TLS, hs uintptr, s uintptr, len int32) uint8 { /* preprocess.c:121:13: */
	for ; hs != 0; hs = (*Hideset)(unsafe.Pointer(hs)).next {
		if libc.Xstrlen(tls, (*Hideset)(unsafe.Pointer(hs)).name) == size_t(len) && !(libc.Xstrncmp(tls, (*Hideset)(unsafe.Pointer(hs)).name, s, uint64(len)) != 0) {
			return uint8(1)
		}
	}
	return uint8(0)
}

func hideset_intersection(tls *libc.TLS, hs1 uintptr, hs2 uintptr) uintptr { /* preprocess.c:128:16: */
	bp := tls.Alloc(16)
	defer tls.Free(16)

	*(*Hideset)(unsafe.Pointer(bp /* head */)) = Hideset{}
	var cur uintptr = bp /* &head */

	for ; hs1 != 0; hs1 = (*Hideset)(unsafe.Pointer(hs1)).next {
		if hideset_contains(tls, hs2, (*Hideset)(unsafe.Pointer(hs1)).name, int32(libc.Xstrlen(tls, (*Hideset)(unsafe.Pointer(hs1)).name))) != 0 {
			cur = libc.AssignPtrUintptr(cur, new_hideset(tls, (*Hideset)(unsafe.Pointer(hs1)).name))
		}
	}
	return (*Hideset)(unsafe.Pointer(bp /* &head */)).next
}

func add_hideset(tls *libc.TLS, tok uintptr, hs uintptr) uintptr { /* preprocess.c:138:14: */
	bp := tls.Alloc(112)
	defer tls.Free(112)

	*(*Token)(unsafe.Pointer(bp /* head */)) = Token{}
	var cur uintptr = bp /* &head */

	for ; tok != 0; tok = (*Token)(unsafe.Pointer(tok)).next {
		var t uintptr = copy_token(tls, tok)
		(*Token)(unsafe.Pointer(t)).hideset = hideset_union(tls, (*Token)(unsafe.Pointer(t)).hideset, hs)
		cur = libc.AssignPtrUintptr(cur+8, t)
	}
	return (*Token)(unsafe.Pointer(bp /* &head */)).next
}

// Append tok2 to the end of tok1.
func append(tls *libc.TLS, tok1 uintptr, tok2 uintptr) uintptr { /* preprocess.c:151:14: */
	bp := tls.Alloc(112)
	defer tls.Free(112)

	if (*Token)(unsafe.Pointer(tok1)).kind == TK_EOF {
		return tok2
	}

	*(*Token)(unsafe.Pointer(bp /* head */)) = Token{}
	var cur uintptr = bp /* &head */

	for ; (*Token)(unsafe.Pointer(tok1)).kind != TK_EOF; tok1 = (*Token)(unsafe.Pointer(tok1)).next {
		cur = libc.AssignPtrUintptr(cur+8, copy_token(tls, tok1))
	}
	(*Token)(unsafe.Pointer(cur)).next = tok2
	return (*Token)(unsafe.Pointer(bp /* &head */)).next
}

func skip_cond_incl2(tls *libc.TLS, tok uintptr) uintptr { /* preprocess.c:164:14: */
	for (*Token)(unsafe.Pointer(tok)).kind != TK_EOF {
		if is_hash(tls, tok) != 0 && (equal(tls, (*Token)(unsafe.Pointer(tok)).next, ts+9250) != 0 || equal(tls, (*Token)(unsafe.Pointer(tok)).next, ts+10264) != 0 || equal(tls, (*Token)(unsafe.Pointer(tok)).next, ts+10270) != 0) {
			tok = skip_cond_incl2(tls, (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(tok)).next)).next)
			continue
		}
		if is_hash(tls, tok) != 0 && equal(tls, (*Token)(unsafe.Pointer(tok)).next, ts+10277) != 0 {
			return (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(tok)).next)).next
		}
		tok = (*Token)(unsafe.Pointer(tok)).next
	}
	return tok
}

// Skip until next `#else`, `#elif` or `#endif`.
// Nested `#if` and `#endif` are skipped.
func skip_cond_incl(tls *libc.TLS, tok uintptr) uintptr { /* preprocess.c:181:14: */
	for (*Token)(unsafe.Pointer(tok)).kind != TK_EOF {
		if is_hash(tls, tok) != 0 && (equal(tls, (*Token)(unsafe.Pointer(tok)).next, ts+9250) != 0 || equal(tls, (*Token)(unsafe.Pointer(tok)).next, ts+10264) != 0 || equal(tls, (*Token)(unsafe.Pointer(tok)).next, ts+10270) != 0) {
			tok = skip_cond_incl2(tls, (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(tok)).next)).next)
			continue
		}

		if is_hash(tls, tok) != 0 && (equal(tls, (*Token)(unsafe.Pointer(tok)).next, ts+10283) != 0 || equal(tls, (*Token)(unsafe.Pointer(tok)).next, ts+9253) != 0 || equal(tls, (*Token)(unsafe.Pointer(tok)).next, ts+10277) != 0) {
			break
		}
		tok = (*Token)(unsafe.Pointer(tok)).next
	}
	return tok
}

// Double-quote a given string and returns it.
func quote_string(tls *libc.TLS, str uintptr) uintptr { /* preprocess.c:200:13: */
	var bufsize int32 = 3
	{
		var i int32 = 0
		for ; *(*int8)(unsafe.Pointer(str + uintptr(i))) != 0; i++ {
			if int32(*(*int8)(unsafe.Pointer(str + uintptr(i)))) == '\\' || int32(*(*int8)(unsafe.Pointer(str + uintptr(i)))) == '"' {
				bufsize++
			}
			bufsize++
		}
	}

	var buf uintptr = libc.Xcalloc(tls, uint64(1), uint64(bufsize))
	var p uintptr = buf
	*(*int8)(unsafe.Pointer(libc.PostIncUintptr(&p, 1))) = int8('"')
	{
		var i1 int32 = 0
		for ; *(*int8)(unsafe.Pointer(str + uintptr(i1))) != 0; i1++ {
			if int32(*(*int8)(unsafe.Pointer(str + uintptr(i1)))) == '\\' || int32(*(*int8)(unsafe.Pointer(str + uintptr(i1)))) == '"' {
				*(*int8)(unsafe.Pointer(libc.PostIncUintptr(&p, 1))) = int8('\\')
			}
			*(*int8)(unsafe.Pointer(libc.PostIncUintptr(&p, 1))) = *(*int8)(unsafe.Pointer(str + uintptr(i1)))
		}
	}
	*(*int8)(unsafe.Pointer(libc.PostIncUintptr(&p, 1))) = int8('"')
	*(*int8)(unsafe.Pointer(libc.PostIncUintptr(&p, 1))) = int8(0)
	return buf
}

func new_str_token(tls *libc.TLS, str uintptr, tmpl uintptr) uintptr { /* preprocess.c:221:14: */
	var buf uintptr = quote_string(tls, str)
	return tokenize(tls, new_file(tls, (*File)(unsafe.Pointer((*Token)(unsafe.Pointer(tmpl)).file)).name, (*File)(unsafe.Pointer((*Token)(unsafe.Pointer(tmpl)).file)).file_no, buf))
}

// Copy all tokens until the next newline, terminate them with
// an EOF token and then returns them. This function is used to
// create a new list of tokens for `#if` arguments.
func copy_line(tls *libc.TLS, rest uintptr, tok uintptr) uintptr { /* preprocess.c:229:14: */
	bp := tls.Alloc(112)
	defer tls.Free(112)

	*(*Token)(unsafe.Pointer(bp /* head */)) = Token{}
	var cur uintptr = bp /* &head */

	for ; !(int32((*Token)(unsafe.Pointer(tok)).at_bol) != 0); tok = (*Token)(unsafe.Pointer(tok)).next {
		cur = libc.AssignPtrUintptr(cur+8, copy_token(tls, tok))
	}

	(*Token)(unsafe.Pointer(cur)).next = new_eof(tls, tok)
	*(*uintptr)(unsafe.Pointer(rest)) = tok
	return (*Token)(unsafe.Pointer(bp /* &head */)).next
}

func new_num_token(tls *libc.TLS, val int32, tmpl uintptr) uintptr { /* preprocess.c:241:14: */
	bp := tls.Alloc(8)
	defer tls.Free(8)

	var buf uintptr = format(tls, ts+10288, libc.VaList(bp, val))
	return tokenize(tls, new_file(tls, (*File)(unsafe.Pointer((*Token)(unsafe.Pointer(tmpl)).file)).name, (*File)(unsafe.Pointer((*Token)(unsafe.Pointer(tmpl)).file)).file_no, buf))
}

func read_const_expr(tls *libc.TLS, rest uintptr, tok uintptr) uintptr { /* preprocess.c:246:14: */
	bp := tls.Alloc(120)
	defer tls.Free(120)
	*(*uintptr)(unsafe.Pointer(bp + 112)) = tok

	*(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)) = copy_line(tls, rest, *(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)))

	*(*Token)(unsafe.Pointer(bp /* head */)) = Token{}
	var cur uintptr = bp /* &head */

	for (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 112)))).kind != TK_EOF {
		// "defined(foo)" or "defined foo" becomes "1" if macro "foo"
		// is defined. Otherwise "0".
		if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 112)), ts+10292) != 0 {
			var start uintptr = *(*uintptr)(unsafe.Pointer(bp + 112 /* tok */))
			var has_paren uint8 = consume(tls, bp+112, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)))).next, ts+8666)

			if (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 112)))).kind != TK_IDENT {
				error_tok(tls, start, ts+10300, 0)
			}
			var m uintptr = find_macro(tls, *(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)))
			*(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)) = (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)))).next

			if has_paren != 0 {
				*(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)), ts+8668)
			}

			cur = libc.AssignPtrUintptr(cur+8, new_num_token(tls, func() int32 {
				if m != 0 {
					return 1
				}
				return 0
			}(), start))
			continue
		}

		cur = libc.AssignPtrUintptr(cur+8, *(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)))
		*(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)) = (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)))).next
	}

	(*Token)(unsafe.Pointer(cur)).next = *(*uintptr)(unsafe.Pointer(bp + 112 /* tok */))
	return (*Token)(unsafe.Pointer(bp /* &head */)).next
}

// Read and evaluate a constant expression.
func eval_const_expr(tls *libc.TLS, rest uintptr, tok uintptr) int64 { /* preprocess.c:280:13: */
	bp := tls.Alloc(8)
	defer tls.Free(8)

	var start uintptr = tok
	var expr uintptr = read_const_expr(tls, rest, (*Token)(unsafe.Pointer(tok)).next)
	expr = preprocess2(tls, expr)

	if (*Token)(unsafe.Pointer(expr)).kind == TK_EOF {
		error_tok(tls, start, ts+10333, 0)
	}

	// [https://www.sigbus.info/n1570#6.10.1p4] The standard requires
	// we replace remaining non-macro identifiers with "0" before
	// evaluating a constant expression. For example, `#if foo` is
	// equivalent to `#if 0` if foo is not defined.
	{
		var t uintptr = expr
		for ; (*Token)(unsafe.Pointer(t)).kind != TK_EOF; t = (*Token)(unsafe.Pointer(t)).next {
			if (*Token)(unsafe.Pointer(t)).kind == TK_IDENT {
				var next uintptr = (*Token)(unsafe.Pointer(t)).next
				*(*Token)(unsafe.Pointer(t)) = *(*Token)(unsafe.Pointer(new_num_token(tls, 0, t)))
				(*Token)(unsafe.Pointer(t)).next = next
			}
		}
	}

	// Convert pp-numbers to regular numbers
	convert_pp_tokens(tls, expr)
	// var rest2 uintptr at bp, 8

	var val int64 = const_expr(tls, bp, expr)
	if (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp)))).kind != TK_EOF {
		error_tok(tls, *(*uintptr)(unsafe.Pointer(bp /* rest2 */)), ts+10252, 0)
	}
	return val
}

func push_cond_incl(tls *libc.TLS, tok uintptr, included uint8) uintptr { /* preprocess.c:310:17: */
	var ci uintptr = libc.Xcalloc(tls, uint64(1), uint64(unsafe.Sizeof(CondIncl{})))
	(*CondIncl)(unsafe.Pointer(ci)).next = cond_incl
	(*CondIncl)(unsafe.Pointer(ci)).ctx = IN_THEN
	(*CondIncl)(unsafe.Pointer(ci)).tok = tok
	(*CondIncl)(unsafe.Pointer(ci)).included = included
	cond_incl = ci
	return ci
}

func find_macro(tls *libc.TLS, tok uintptr) uintptr { /* preprocess.c:320:14: */
	if (*Token)(unsafe.Pointer(tok)).kind != TK_IDENT {
		return uintptr(0)
	}
	return hashmap_get2(tls, uintptr(unsafe.Pointer(&macros)), (*Token)(unsafe.Pointer(tok)).loc, (*Token)(unsafe.Pointer(tok)).len)
}

func add_macro(tls *libc.TLS, name uintptr, is_objlike uint8, body uintptr) uintptr { /* preprocess.c:326:14: */
	var m uintptr = libc.Xcalloc(tls, uint64(1), uint64(unsafe.Sizeof(Macro{})))
	(*Macro)(unsafe.Pointer(m)).name = name
	(*Macro)(unsafe.Pointer(m)).is_objlike = is_objlike
	(*Macro)(unsafe.Pointer(m)).body = body
	hashmap_put(tls, uintptr(unsafe.Pointer(&macros)), name, m)
	return m
}

func read_macro_params(tls *libc.TLS, rest uintptr, tok uintptr, va_args_name uintptr) uintptr { /* preprocess.c:335:19: */
	bp := tls.Alloc(16)
	defer tls.Free(16)

	*(*MacroParam)(unsafe.Pointer(bp /* head */)) = MacroParam{}
	var cur uintptr = bp /* &head */

	for !(equal(tls, tok, ts+8668) != 0) {
		if cur != bp {
			tok = skip(tls, tok, ts+8331)
		}

		if equal(tls, tok, ts+8825) != 0 {
			*(*uintptr)(unsafe.Pointer(va_args_name)) = ts + 10347 /* "__VA_ARGS__" */
			*(*uintptr)(unsafe.Pointer(rest)) = skip(tls, (*Token)(unsafe.Pointer(tok)).next, ts+8668)
			return (*MacroParam)(unsafe.Pointer(bp /* &head */)).next
		}

		if (*Token)(unsafe.Pointer(tok)).kind != TK_IDENT {
			error_tok(tls, tok, ts+8368, 0)
		}

		if equal(tls, (*Token)(unsafe.Pointer(tok)).next, ts+8825) != 0 {
			*(*uintptr)(unsafe.Pointer(va_args_name)) = xstrndup(tls, (*Token)(unsafe.Pointer(tok)).loc, uint64((*Token)(unsafe.Pointer(tok)).len))
			*(*uintptr)(unsafe.Pointer(rest)) = skip(tls, (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(tok)).next)).next, ts+8668)
			return (*MacroParam)(unsafe.Pointer(bp /* &head */)).next
		}

		var m uintptr = libc.Xcalloc(tls, uint64(1), uint64(unsafe.Sizeof(MacroParam{})))
		(*MacroParam)(unsafe.Pointer(m)).name = xstrndup(tls, (*Token)(unsafe.Pointer(tok)).loc, uint64((*Token)(unsafe.Pointer(tok)).len))
		cur = libc.AssignPtrUintptr(cur, m)
		tok = (*Token)(unsafe.Pointer(tok)).next
	}

	*(*uintptr)(unsafe.Pointer(rest)) = (*Token)(unsafe.Pointer(tok)).next
	return (*MacroParam)(unsafe.Pointer(bp /* &head */)).next
}

func read_macro_definition(tls *libc.TLS, rest uintptr, tok uintptr) { /* preprocess.c:368:13: */
	bp := tls.Alloc(16)
	defer tls.Free(16)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	if (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp)))).kind != TK_IDENT {
		error_tok(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+10300, 0)
	}
	var name uintptr = xstrndup(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).loc, uint64((*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).len))
	*(*uintptr)(unsafe.Pointer(bp /* tok */)) = (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next

	if !(int32((*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp)))).has_space) != 0) && equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8666) != 0 {
		// Function-like macro
		*(*uintptr)(unsafe.Pointer(bp + 8 /* va_args_name */)) = uintptr(0)
		var params uintptr = read_macro_params(tls, bp, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next, bp+8)

		var m uintptr = add_macro(tls, name, uint8(0), copy_line(tls, rest, *(*uintptr)(unsafe.Pointer(bp /* tok */))))
		(*Macro)(unsafe.Pointer(m)).params = params
		(*Macro)(unsafe.Pointer(m)).va_args_name = *(*uintptr)(unsafe.Pointer(bp + 8 /* va_args_name */))
	} else {
		// Object-like macro
		add_macro(tls, name, uint8(1), copy_line(tls, rest, *(*uintptr)(unsafe.Pointer(bp /* tok */))))
	}
}

func read_macro_arg_one(tls *libc.TLS, rest uintptr, tok uintptr, read_rest uint8) uintptr { /* preprocess.c:388:17: */
	bp := tls.Alloc(112)
	defer tls.Free(112)

	*(*Token)(unsafe.Pointer(bp /* head */)) = Token{}
	var cur uintptr = bp /* &head */
	var level int32 = 0

	for {
		if level == 0 && equal(tls, tok, ts+8668) != 0 {
			break
		}
		if level == 0 && !(read_rest != 0) && equal(tls, tok, ts+8331) != 0 {
			break
		}

		if (*Token)(unsafe.Pointer(tok)).kind == TK_EOF {
			error_tok(tls, tok, ts+10359, 0)
		}

		if equal(tls, tok, ts+8666) != 0 {
			level++
		} else if equal(tls, tok, ts+8668) != 0 {
			level--
		}

		cur = libc.AssignPtrUintptr(cur+8, copy_token(tls, tok))
		tok = (*Token)(unsafe.Pointer(tok)).next
	}

	(*Token)(unsafe.Pointer(cur)).next = new_eof(tls, tok)

	var arg uintptr = libc.Xcalloc(tls, uint64(1), uint64(unsafe.Sizeof(MacroArg{})))
	(*MacroArg)(unsafe.Pointer(arg)).tok = (*Token)(unsafe.Pointer(bp /* &head */)).next
	*(*uintptr)(unsafe.Pointer(rest)) = tok
	return arg
}

func read_macro_args(tls *libc.TLS, rest uintptr, tok uintptr, params uintptr, va_args_name uintptr) uintptr { /* preprocess.c:419:17: */
	bp := tls.Alloc(40)
	defer tls.Free(40)
	*(*uintptr)(unsafe.Pointer(bp + 32)) = tok

	var start uintptr = *(*uintptr)(unsafe.Pointer(bp + 32 /* tok */))
	*(*uintptr)(unsafe.Pointer(bp + 32 /* tok */)) = (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 32 /* tok */)))).next)).next

	*(*MacroArg)(unsafe.Pointer(bp /* head */)) = MacroArg{}
	var cur uintptr = bp /* &head */

	var pp uintptr = params
	for ; pp != 0; pp = (*MacroParam)(unsafe.Pointer(pp)).next {
		if cur != bp {
			*(*uintptr)(unsafe.Pointer(bp + 32 /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp + 32 /* tok */)), ts+8331)
		}
		cur = libc.AssignPtrUintptr(cur, read_macro_arg_one(tls, bp+32, *(*uintptr)(unsafe.Pointer(bp + 32 /* tok */)), uint8(0)))
		(*MacroArg)(unsafe.Pointer(cur)).name = (*MacroParam)(unsafe.Pointer(pp)).name
	}

	if va_args_name != 0 {
		var arg uintptr
		if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 32)), ts+8668) != 0 {
			arg = libc.Xcalloc(tls, uint64(1), uint64(unsafe.Sizeof(MacroArg{})))
			(*MacroArg)(unsafe.Pointer(arg)).tok = new_eof(tls, *(*uintptr)(unsafe.Pointer(bp + 32 /* tok */)))
		} else {
			if pp != params {
				*(*uintptr)(unsafe.Pointer(bp + 32 /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp + 32 /* tok */)), ts+8331)
			}
			arg = read_macro_arg_one(tls, bp+32, *(*uintptr)(unsafe.Pointer(bp + 32 /* tok */)), uint8(1))
		}
		(*MacroArg)(unsafe.Pointer(arg)).name = va_args_name

		(*MacroArg)(unsafe.Pointer(arg)).is_va_args = uint8(1)
		cur = libc.AssignPtrUintptr(cur, arg)
	} else if pp != 0 {
		error_tok(tls, start, ts+9680, 0)
	}

	skip(tls, *(*uintptr)(unsafe.Pointer(bp + 32 /* tok */)), ts+8668)
	*(*uintptr)(unsafe.Pointer(rest)) = *(*uintptr)(unsafe.Pointer(bp + 32 /* tok */))
	return (*MacroArg)(unsafe.Pointer(bp /* &head */)).next
}

func find_arg(tls *libc.TLS, args uintptr, tok uintptr) uintptr { /* preprocess.c:457:17: */
	{
		var ap uintptr = args
		for ; ap != 0; ap = (*MacroArg)(unsafe.Pointer(ap)).next {
			if size_t((*Token)(unsafe.Pointer(tok)).len) == libc.Xstrlen(tls, (*MacroArg)(unsafe.Pointer(ap)).name) && !(libc.Xstrncmp(tls, (*Token)(unsafe.Pointer(tok)).loc, (*MacroArg)(unsafe.Pointer(ap)).name, uint64((*Token)(unsafe.Pointer(tok)).len)) != 0) {
				return ap
			}
		}
	}
	return uintptr(0)
}

// Concatenates all tokens in `tok` and returns a new string.
func join_tokens(tls *libc.TLS, tok uintptr, end uintptr) uintptr { /* preprocess.c:465:13: */
	// Compute the length of the resulting token.
	var len int32 = 1
	{
		var t uintptr = tok
		for ; t != end && (*Token)(unsafe.Pointer(t)).kind != TK_EOF; t = (*Token)(unsafe.Pointer(t)).next {
			if t != tok && (*Token)(unsafe.Pointer(t)).has_space != 0 {
				len++
			}
			len = len + (*Token)(unsafe.Pointer(t)).len
		}
	}

	var buf uintptr = libc.Xcalloc(tls, uint64(1), uint64(len))

	// Copy token texts.
	var pos int32 = 0
	{
		var t1 uintptr = tok
		for ; t1 != end && (*Token)(unsafe.Pointer(t1)).kind != TK_EOF; t1 = (*Token)(unsafe.Pointer(t1)).next {
			if t1 != tok && (*Token)(unsafe.Pointer(t1)).has_space != 0 {
				*(*int8)(unsafe.Pointer(buf + uintptr(libc.PostIncInt32(&pos, 1)))) = int8(' ')
			}
			libc.Xstrncpy(tls, buf+uintptr(pos), (*Token)(unsafe.Pointer(t1)).loc, uint64((*Token)(unsafe.Pointer(t1)).len))
			pos = pos + (*Token)(unsafe.Pointer(t1)).len
		}
	}
	*(*int8)(unsafe.Pointer(buf + uintptr(pos))) = int8(0)
	return buf
}

// Concatenates all tokens in `arg` and returns a new string token.
// This function is used for the stringizing operator (#).
func stringize(tls *libc.TLS, hash uintptr, arg uintptr) uintptr { /* preprocess.c:490:14: */
	// Create a new string token. We need to set some value to its
	// source location for error reporting function, so we use a macro
	// name token as a template.
	var s uintptr = join_tokens(tls, arg, uintptr(0))
	return new_str_token(tls, s, hash)
}

// Concatenate two tokens to create a new token.
func paste(tls *libc.TLS, lhs uintptr, rhs uintptr) uintptr { /* preprocess.c:499:14: */
	bp := tls.Alloc(40)
	defer tls.Free(40)

	// Paste the two tokens.
	var buf uintptr = format(tls, ts+10382, libc.VaList(bp, (*Token)(unsafe.Pointer(lhs)).len, (*Token)(unsafe.Pointer(lhs)).loc, (*Token)(unsafe.Pointer(rhs)).len, (*Token)(unsafe.Pointer(rhs)).loc))

	// Tokenize the resulting string.
	var tok uintptr = tokenize(tls, new_file(tls, (*File)(unsafe.Pointer((*Token)(unsafe.Pointer(lhs)).file)).name, (*File)(unsafe.Pointer((*Token)(unsafe.Pointer(lhs)).file)).file_no, buf))
	if (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(tok)).next)).kind != TK_EOF {
		error_tok(tls, lhs, ts+10391, libc.VaList(bp+32, buf))
	}
	return tok
}

func has_varargs(tls *libc.TLS, args uintptr) uint8 { /* preprocess.c:510:13: */
	{
		var ap uintptr = args
		for ; ap != 0; ap = (*MacroArg)(unsafe.Pointer(ap)).next {
			if !(libc.Xstrcmp(tls, (*MacroArg)(unsafe.Pointer(ap)).name, ts+10347) != 0) {
				return uint8(libc.Bool32((*Token)(unsafe.Pointer((*MacroArg)(unsafe.Pointer(ap)).tok)).kind != TK_EOF))
			}
		}
	}
	return uint8(0)
}

// Replace func-like macro parameters with given arguments.
func subst(tls *libc.TLS, tok uintptr, args uintptr) uintptr { /* preprocess.c:518:14: */
	bp := tls.Alloc(120)
	defer tls.Free(120)
	*(*uintptr)(unsafe.Pointer(bp + 112)) = tok

	*(*Token)(unsafe.Pointer(bp /* head */)) = Token{}
	var cur uintptr = bp /* &head */

	for (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 112)))).kind != TK_EOF {
		// "#" followed by a parameter is replaced with stringized actuals.
		if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 112)), ts+10250) != 0 {
			var arg uintptr = find_arg(tls, args, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)))).next)
			if !(arg != 0) {
				error_tok(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)))).next, ts+10428, 0)
			}
			cur = libc.AssignPtrUintptr(cur+8, stringize(tls, *(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)), (*MacroArg)(unsafe.Pointer(arg)).tok))
			*(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)) = (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)))).next)).next
			continue
		}

		// [GNU] If __VA_ARG__ is empty, `,##__VA_ARGS__` is expanded
		// to the empty token list. Otherwise, its expaned to `,` and
		// __VA_ARGS__.
		if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 112)), ts+8331) != 0 && equal(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 112)))).next, ts+10469) != 0 {
			var arg uintptr = find_arg(tls, args, (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)))).next)).next)
			if arg != 0 && (*MacroArg)(unsafe.Pointer(arg)).is_va_args != 0 {
				if (*Token)(unsafe.Pointer((*MacroArg)(unsafe.Pointer(arg)).tok)).kind == TK_EOF {
					*(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)) = (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)))).next)).next)).next
				} else {
					cur = libc.AssignPtrUintptr(cur+8, copy_token(tls, *(*uintptr)(unsafe.Pointer(bp + 112 /* tok */))))
					*(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)) = (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)))).next)).next
				}
				continue
			}
		}

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 112)), ts+10469) != 0 {
			if cur == bp {
				error_tok(tls, *(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)), ts+10472, 0)
			}

			if (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 112)))).next)).kind == TK_EOF {
				error_tok(tls, *(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)), ts+10519, 0)
			}

			var arg uintptr = find_arg(tls, args, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)))).next)
			if arg != 0 {
				if (*Token)(unsafe.Pointer((*MacroArg)(unsafe.Pointer(arg)).tok)).kind != TK_EOF {
					*(*Token)(unsafe.Pointer(cur)) = *(*Token)(unsafe.Pointer(paste(tls, cur, (*MacroArg)(unsafe.Pointer(arg)).tok)))
					{
						var t uintptr = (*Token)(unsafe.Pointer((*MacroArg)(unsafe.Pointer(arg)).tok)).next
						for ; (*Token)(unsafe.Pointer(t)).kind != TK_EOF; t = (*Token)(unsafe.Pointer(t)).next {
							cur = libc.AssignPtrUintptr(cur+8, copy_token(tls, t))
						}
					}
				}
				*(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)) = (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)))).next)).next
				continue
			}

			*(*Token)(unsafe.Pointer(cur)) = *(*Token)(unsafe.Pointer(paste(tls, cur, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)))).next)))
			*(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)) = (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)))).next)).next
			continue
		}

		var arg uintptr = find_arg(tls, args, *(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)))

		if arg != 0 && equal(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 112)))).next, ts+10469) != 0 {
			var rhs uintptr = (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)))).next)).next

			if (*Token)(unsafe.Pointer((*MacroArg)(unsafe.Pointer(arg)).tok)).kind == TK_EOF {
				var arg2 uintptr = find_arg(tls, args, rhs)
				if arg2 != 0 {
					{
						var t uintptr = (*MacroArg)(unsafe.Pointer(arg2)).tok
						for ; (*Token)(unsafe.Pointer(t)).kind != TK_EOF; t = (*Token)(unsafe.Pointer(t)).next {
							cur = libc.AssignPtrUintptr(cur+8, copy_token(tls, t))
						}
					}
				} else {
					cur = libc.AssignPtrUintptr(cur+8, copy_token(tls, rhs))
				}
				*(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)) = (*Token)(unsafe.Pointer(rhs)).next
				continue
			}

			{
				var t uintptr = (*MacroArg)(unsafe.Pointer(arg)).tok
				for ; (*Token)(unsafe.Pointer(t)).kind != TK_EOF; t = (*Token)(unsafe.Pointer(t)).next {
					cur = libc.AssignPtrUintptr(cur+8, copy_token(tls, t))
				}
			}
			*(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)) = (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)))).next
			continue
		}

		// If __VA_ARG__ is empty, __VA_OPT__(x) is expanded to the
		// empty token list. Otherwise, __VA_OPT__(x) is expanded to x.
		if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 112)), ts+10564) != 0 && equal(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 112)))).next, ts+8666) != 0 {
			var arg uintptr = read_macro_arg_one(tls, bp+112, (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)))).next)).next, uint8(1))
			if has_varargs(tls, args) != 0 {
				var t uintptr = (*MacroArg)(unsafe.Pointer(arg)).tok
				for ; (*Token)(unsafe.Pointer(t)).kind != TK_EOF; t = (*Token)(unsafe.Pointer(t)).next {
					cur = libc.AssignPtrUintptr(cur+8, t)
				}
			}
			*(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)), ts+8668)
			continue
		}

		// Handle a macro token. Macro arguments are completely macro-expanded
		// before they are substituted into a macro body.
		if arg != 0 {
			var t uintptr = preprocess2(tls, (*MacroArg)(unsafe.Pointer(arg)).tok)
			(*Token)(unsafe.Pointer(t)).at_bol = (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)))).at_bol
			(*Token)(unsafe.Pointer(t)).has_space = (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)))).has_space
			for ; (*Token)(unsafe.Pointer(t)).kind != TK_EOF; t = (*Token)(unsafe.Pointer(t)).next {
				cur = libc.AssignPtrUintptr(cur+8, copy_token(tls, t))
			}
			*(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)) = (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)))).next
			continue
		}

		// Handle a non-macro token.
		cur = libc.AssignPtrUintptr(cur+8, copy_token(tls, *(*uintptr)(unsafe.Pointer(bp + 112 /* tok */))))
		*(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)) = (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 112 /* tok */)))).next
		continue
	}

	(*Token)(unsafe.Pointer(cur)).next = *(*uintptr)(unsafe.Pointer(bp + 112 /* tok */))
	return (*Token)(unsafe.Pointer(bp /* &head */)).next
}

// If tok is a macro, expand it and return true.
// Otherwise, do nothing and return false.
func expand_macro(tls *libc.TLS, rest uintptr, tok uintptr) uint8 { /* preprocess.c:632:13: */
	bp := tls.Alloc(8)
	defer tls.Free(8)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	if hideset_contains(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp)))).hideset, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp)))).loc, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp)))).len) != 0 {
		return uint8(0)
	}

	var m uintptr = find_macro(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
	if !(m != 0) {
		return uint8(0)
	}

	// Built-in dynamic macro application such as __LINE__
	if (*Macro)(unsafe.Pointer(m)).handler != 0 {
		*(*uintptr)(unsafe.Pointer(rest)) = (*struct {
			f func(*libc.TLS, uintptr) uintptr
		})(unsafe.Pointer(&struct{ uintptr }{(*Macro)(unsafe.Pointer(m)).handler})).f(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		(*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(rest)))).next = (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next
		return uint8(1)
	}

	// Object-like macro application
	if (*Macro)(unsafe.Pointer(m)).is_objlike != 0 {
		var hs uintptr = hideset_union(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).hideset, new_hideset(tls, (*Macro)(unsafe.Pointer(m)).name))
		var body uintptr = add_hideset(tls, (*Macro)(unsafe.Pointer(m)).body, hs)
		{
			var t uintptr = body
			for ; (*Token)(unsafe.Pointer(t)).kind != TK_EOF; t = (*Token)(unsafe.Pointer(t)).next {
				(*Token)(unsafe.Pointer(t)).origin = *(*uintptr)(unsafe.Pointer(bp /* tok */))
			}
		}
		*(*uintptr)(unsafe.Pointer(rest)) = append(tls, body, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next)
		(*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(rest)))).at_bol = (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).at_bol
		(*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(rest)))).has_space = (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).has_space
		return uint8(1)
	}

	// If a funclike macro token is not followed by an argument list,
	// treat it as a normal identifier.
	if !(equal(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp)))).next, ts+8666) != 0) {
		return uint8(0)
	}

	// Function-like macro application
	var macro_token uintptr = *(*uintptr)(unsafe.Pointer(bp /* tok */))
	var args uintptr = read_macro_args(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)), (*Macro)(unsafe.Pointer(m)).params, (*Macro)(unsafe.Pointer(m)).va_args_name)
	var rparen uintptr = *(*uintptr)(unsafe.Pointer(bp /* tok */))

	// Tokens that consist a func-like macro invocation may have different
	// hidesets, and if that's the case, it's not clear what the hideset
	// for the new tokens should be. We take the interesection of the
	// macro token and the closing parenthesis and use it as a new hideset
	// as explained in the Dave Prossor's algorithm.
	var hs uintptr = hideset_intersection(tls, (*Token)(unsafe.Pointer(macro_token)).hideset, (*Token)(unsafe.Pointer(rparen)).hideset)
	hs = hideset_union(tls, hs, new_hideset(tls, (*Macro)(unsafe.Pointer(m)).name))

	var body uintptr = subst(tls, (*Macro)(unsafe.Pointer(m)).body, args)
	body = add_hideset(tls, body, hs)
	{
		var t uintptr = body
		for ; (*Token)(unsafe.Pointer(t)).kind != TK_EOF; t = (*Token)(unsafe.Pointer(t)).next {
			(*Token)(unsafe.Pointer(t)).origin = macro_token
		}
	}
	*(*uintptr)(unsafe.Pointer(rest)) = append(tls, body, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next)
	(*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(rest)))).at_bol = (*Token)(unsafe.Pointer(macro_token)).at_bol
	(*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(rest)))).has_space = (*Token)(unsafe.Pointer(macro_token)).has_space
	return uint8(1)
}

func search_include_paths(tls *libc.TLS, filename uintptr) uintptr { /* preprocess.c:687:6: */
	bp := tls.Alloc(16)
	defer tls.Free(16)

	if int32(*(*int8)(unsafe.Pointer(filename))) == '/' {
		return filename
	}
	var cached uintptr = hashmap_get(tls, uintptr(unsafe.Pointer(&cache)), filename)
	if cached != 0 {
		return cached
	}

	// Search a file from the include paths.
	{
		var i int32 = 0
		for ; i < include_paths.len; i++ {
			var path uintptr = format(tls, ts+10575, libc.VaList(bp, *(*uintptr)(unsafe.Pointer(include_paths.data + uintptr(i)*8)), filename))
			if !(file_exists(tls, path) != 0) {
				continue
			}
			hashmap_put(tls, uintptr(unsafe.Pointer(&cache)), filename, path)
			include_next_idx = i + 1
			return path
		}
	}
	return uintptr(0)
}

var cache HashMap /* preprocess.c:691:18: */

func search_include_next(tls *libc.TLS, filename uintptr) uintptr { /* preprocess.c:708:13: */
	bp := tls.Alloc(16)
	defer tls.Free(16)

	for ; include_next_idx < include_paths.len; include_next_idx++ {
		var path uintptr = format(tls, ts+10575, libc.VaList(bp, *(*uintptr)(unsafe.Pointer(include_paths.data + uintptr(include_next_idx)*8)), filename))
		if file_exists(tls, path) != 0 {
			return path
		}
	}
	return uintptr(0)
}

// Read an #include argument.
func read_include_filename(tls *libc.TLS, rest uintptr, tok uintptr, is_dquote uintptr) uintptr { /* preprocess.c:718:13: */
	bp := tls.Alloc(8)
	defer tls.Free(8)

	// Pattern 1: #include "foo.h"
	if (*Token)(unsafe.Pointer(tok)).kind == TK_STR {
		// A double-quoted filename for #include is a special kind of
		// token, and we don't want to interpret any escape sequences in it.
		// For example, "\f" in "C:\foo" is not a formfeed character but
		// just two non-control characters, backslash and f.
		// So we don't want to use token->str.
		*(*uint8)(unsafe.Pointer(is_dquote)) = uint8(1)
		*(*uintptr)(unsafe.Pointer(rest)) = skip_line(tls, (*Token)(unsafe.Pointer(tok)).next)
		return xstrndup(tls, (*Token)(unsafe.Pointer(tok)).loc+uintptr(1), uint64((*Token)(unsafe.Pointer(tok)).len-2))
	}

	// Pattern 2: #include <foo.h>
	if equal(tls, tok, ts+9496) != 0 {
		// Reconstruct a filename from a sequence of tokens between
		// "<" and ">".
		var start uintptr = tok

		// Find closing ">".
		for ; !(equal(tls, tok, ts+9501) != 0); tok = (*Token)(unsafe.Pointer(tok)).next {
			if (*Token)(unsafe.Pointer(tok)).at_bol != 0 || (*Token)(unsafe.Pointer(tok)).kind == TK_EOF {
				error_tok(tls, tok, ts+10581, 0)
			}
		}

		*(*uint8)(unsafe.Pointer(is_dquote)) = uint8(0)
		*(*uintptr)(unsafe.Pointer(rest)) = skip_line(tls, (*Token)(unsafe.Pointer(tok)).next)
		return join_tokens(tls, (*Token)(unsafe.Pointer(start)).next, tok)
	}

	// Pattern 3: #include FOO
	// In this case FOO must be macro-expanded to either
	// a single string token or a sequence of "<" ... ">".
	if (*Token)(unsafe.Pointer(tok)).kind == TK_IDENT {
		*(*uintptr)(unsafe.Pointer(bp /* tok2 */)) = preprocess2(tls, copy_line(tls, rest, tok))
		return read_include_filename(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok2 */)), is_dquote)
	}

	error_tok(tls, tok, ts+10594, 0)
	return uintptr(0)
}

// Detect the following "include guard" pattern.
//
//	#ifndef FOO_H
//	#define FOO_H
//	...
//	#endif
func detect_include_guard(tls *libc.TLS, tok uintptr) uintptr { /* preprocess.c:764:13: */
	// Detect the first two lines.
	if !(is_hash(tls, tok) != 0) || !(equal(tls, (*Token)(unsafe.Pointer(tok)).next, ts+10270) != 0) {
		return uintptr(0)
	}
	tok = (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(tok)).next)).next

	if (*Token)(unsafe.Pointer(tok)).kind != TK_IDENT {
		return uintptr(0)
	}

	var macro uintptr = xstrndup(tls, (*Token)(unsafe.Pointer(tok)).loc, uint64((*Token)(unsafe.Pointer(tok)).len))
	tok = (*Token)(unsafe.Pointer(tok)).next

	if !(is_hash(tls, tok) != 0) || !(equal(tls, (*Token)(unsafe.Pointer(tok)).next, ts+10614) != 0) || !(equal(tls, (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(tok)).next)).next, macro) != 0) {
		return uintptr(0)
	}

	// Read until the end of the file.
	for (*Token)(unsafe.Pointer(tok)).kind != TK_EOF {
		if !(is_hash(tls, tok) != 0) {
			tok = (*Token)(unsafe.Pointer(tok)).next
			continue
		}

		if equal(tls, (*Token)(unsafe.Pointer(tok)).next, ts+10277) != 0 && (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(tok)).next)).next)).kind == TK_EOF {
			return macro
		}

		if equal(tls, tok, ts+9250) != 0 || equal(tls, tok, ts+10264) != 0 || equal(tls, tok, ts+10270) != 0 {
			tok = skip_cond_incl(tls, (*Token)(unsafe.Pointer(tok)).next)
		} else {
			tok = (*Token)(unsafe.Pointer(tok)).next
		}
	}
	return uintptr(0)
}

func include_file(tls *libc.TLS, tok uintptr, path uintptr, filename_tok uintptr) uintptr { /* preprocess.c:797:14: */
	bp := tls.Alloc(16)
	defer tls.Free(16)

	// Check for "#pragma once"
	if hashmap_get(tls, uintptr(unsafe.Pointer(&pragma_once)), path) != 0 {
		return tok
	}
	var guard_name uintptr = hashmap_get(tls, uintptr(unsafe.Pointer(&include_guards)), path)
	if guard_name != 0 && hashmap_get(tls, uintptr(unsafe.Pointer(&macros)), guard_name) != 0 {
		return tok
	}

	var tok2 uintptr = tokenize_file(tls, path)
	if !(tok2 != 0) {
		error_tok(tls, filename_tok, ts+10621, libc.VaList(bp, path, libc.Xstrerror(tls, *(*int32)(unsafe.Pointer(libc.X__errno_location(tls))))))
	}

	guard_name = detect_include_guard(tls, tok2)
	if guard_name != 0 {
		hashmap_put(tls, uintptr(unsafe.Pointer(&include_guards)), path, guard_name)
	}

	return append(tls, tok2, tok)
}

var include_guards HashMap /* preprocess.c:805:18: */

// Read #line arguments
func read_line_marker(tls *libc.TLS, rest uintptr, tok uintptr) { /* preprocess.c:822:13: */
	var start uintptr = tok
	tok = preprocess(tls, copy_line(tls, rest, tok))

	if (*Token)(unsafe.Pointer(tok)).kind != TK_NUM || (*Type)(unsafe.Pointer((*Token)(unsafe.Pointer(tok)).ty)).kind != TY_INT {
		error_tok(tls, tok, ts+10646, 0)
	}
	(*File)(unsafe.Pointer((*Token)(unsafe.Pointer(start)).file)).line_delta = int32((*Token)(unsafe.Pointer(tok)).val - int64_t((*Token)(unsafe.Pointer(start)).line_no))

	tok = (*Token)(unsafe.Pointer(tok)).next
	if (*Token)(unsafe.Pointer(tok)).kind == TK_EOF {
		return
	}

	if (*Token)(unsafe.Pointer(tok)).kind != TK_STR {
		error_tok(tls, tok, ts+10666, 0)
	}
	(*File)(unsafe.Pointer((*Token)(unsafe.Pointer(start)).file)).display_name = (*Token)(unsafe.Pointer(tok)).str
}

// Visit all tokens in `tok` while evaluating preprocessing
// macros and directives.
func preprocess2(tls *libc.TLS, tok uintptr) uintptr { /* preprocess.c:841:14: */
	bp := tls.Alloc(138)
	defer tls.Free(138)
	*(*uintptr)(unsafe.Pointer(bp + 128)) = tok

	*(*Token)(unsafe.Pointer(bp + 16 /* head */)) = Token{}
	var cur uintptr = bp + 16 /* &head */

	for (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 128)))).kind != TK_EOF {
		// If it is a macro, expand it.
		if expand_macro(tls, bp+128, *(*uintptr)(unsafe.Pointer(bp + 128))) != 0 {
			continue
		}

		// Pass through if it is not a "#".
		if !(is_hash(tls, *(*uintptr)(unsafe.Pointer(bp + 128))) != 0) {
			(*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)))).line_delta = (*File)(unsafe.Pointer((*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)))).file)).line_delta
			(*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)))).filename = (*File)(unsafe.Pointer((*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)))).file)).display_name
			cur = libc.AssignPtrUintptr(cur+8, *(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)))
			*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)) = (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)))).next
			continue
		}

		var start uintptr = *(*uintptr)(unsafe.Pointer(bp + 128 /* tok */))
		*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)) = (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)))).next

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 128)), ts+10684) != 0 {
			// var is_dquote uint8 at bp+136, 1

			var filename uintptr = read_include_filename(tls, bp+128, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)))).next, bp+136)

			if int32(*(*int8)(unsafe.Pointer(filename))) != '/' && *(*uint8)(unsafe.Pointer(bp + 136)) != 0 {
				var path uintptr = format(tls, ts+10575, libc.VaList(bp, xdirname(tls, (*File)(unsafe.Pointer((*Token)(unsafe.Pointer(start)).file)).name), filename))
				if file_exists(tls, path) != 0 {
					*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)) = include_file(tls, *(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)), path, (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(start)).next)).next)
					continue
				}
			}

			var path uintptr = search_include_paths(tls, filename)
			*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)) = include_file(tls, *(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)), func() uintptr {
				if path != 0 {
					return path
				}
				return filename
			}(), (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(start)).next)).next)
			continue
		}

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 128)), ts+10692) != 0 {
			// var ignore uint8 at bp+137, 1

			var filename uintptr = read_include_filename(tls, bp+128, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)))).next, bp+137)
			var path uintptr = search_include_next(tls, filename)
			*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)) = include_file(tls, *(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)), func() uintptr {
				if path != 0 {
					return path
				}
				return filename
			}(), (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(start)).next)).next)
			continue
		}

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 128)), ts+10614) != 0 {
			read_macro_definition(tls, bp+128, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)))).next)
			continue
		}

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 128)), ts+10705) != 0 {
			*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)) = (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)))).next
			if (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 128)))).kind != TK_IDENT {
				error_tok(tls, *(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)), ts+10300, 0)
			}
			undef_macro(tls, xstrndup(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)))).loc, uint64((*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)))).len)))
			*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)) = skip_line(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)))).next)
			continue
		}

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 128)), ts+9250) != 0 {
			var val int64 = eval_const_expr(tls, bp+128, *(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)))
			push_cond_incl(tls, start, uint8(val))
			if !(val != 0) {
				*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)) = skip_cond_incl(tls, *(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)))
			}
			continue
		}

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 128)), ts+10264) != 0 {
			var defined uint8 = uint8(find_macro(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)))).next))
			push_cond_incl(tls, *(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)), defined)
			*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)) = skip_line(tls, (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)))).next)).next)
			if !(defined != 0) {
				*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)) = skip_cond_incl(tls, *(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)))
			}
			continue
		}

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 128)), ts+10270) != 0 {
			var defined uint8 = uint8(find_macro(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)))).next))
			push_cond_incl(tls, *(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)), libc.BoolUint8(!(defined != 0)))
			*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)) = skip_line(tls, (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)))).next)).next)
			if defined != 0 {
				*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)) = skip_cond_incl(tls, *(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)))
			}
			continue
		}

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 128)), ts+10283) != 0 {
			if !(cond_incl != 0) || (*CondIncl)(unsafe.Pointer(cond_incl)).ctx == IN_ELSE {
				error_tok(tls, start, ts+10711, 0)
			}
			(*CondIncl)(unsafe.Pointer(cond_incl)).ctx = IN_ELIF

			if !(int32((*CondIncl)(unsafe.Pointer(cond_incl)).included) != 0) && eval_const_expr(tls, bp+128, *(*uintptr)(unsafe.Pointer(bp + 128))) != 0 {
				(*CondIncl)(unsafe.Pointer(cond_incl)).included = uint8(1)
			} else {
				*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)) = skip_cond_incl(tls, *(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)))
			}
			continue
		}

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 128)), ts+9253) != 0 {
			if !(cond_incl != 0) || (*CondIncl)(unsafe.Pointer(cond_incl)).ctx == IN_ELSE {
				error_tok(tls, start, ts+10723, 0)
			}
			(*CondIncl)(unsafe.Pointer(cond_incl)).ctx = IN_ELSE
			*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)) = skip_line(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)))).next)

			if (*CondIncl)(unsafe.Pointer(cond_incl)).included != 0 {
				*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)) = skip_cond_incl(tls, *(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)))
			}
			continue
		}

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 128)), ts+10277) != 0 {
			if !(cond_incl != 0) {
				error_tok(tls, start, ts+10735, 0)
			}
			cond_incl = (*CondIncl)(unsafe.Pointer(cond_incl)).next
			*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)) = skip_line(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)))).next)
			continue
		}

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 128)), ts+10748) != 0 {
			read_line_marker(tls, bp+128, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)))).next)
			continue
		}

		if (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 128)))).kind == TK_PP_NUM {
			read_line_marker(tls, bp+128, *(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)))
			continue
		}

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 128)), ts+10753) != 0 && equal(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 128)))).next, ts+10760) != 0 {
			hashmap_put(tls, uintptr(unsafe.Pointer(&pragma_once)), (*File)(unsafe.Pointer((*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)))).file)).name, uintptr(1))
			*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)) = skip_line(tls, (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)))).next)).next)
			continue
		}

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 128)), ts+10753) != 0 {
			for __ccgo := true; __ccgo; __ccgo = !(int32((*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 128)))).at_bol) != 0) {
				*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)) = (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)))).next
			}
			continue
		}

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 128)), ts+10765) != 0 {
			error_tok(tls, *(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)), ts+10765, 0)
		}

		// `#`-only line is legal. It's called a null directive.
		if (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 128)))).at_bol != 0 {
			continue
		}

		error_tok(tls, *(*uintptr)(unsafe.Pointer(bp + 128 /* tok */)), ts+10771, 0)
	}

	(*Token)(unsafe.Pointer(cur)).next = *(*uintptr)(unsafe.Pointer(bp + 128 /* tok */))
	return (*Token)(unsafe.Pointer(bp + 16 /* &head */)).next
}

func define_macro(tls *libc.TLS, name uintptr, buf uintptr) { /* preprocess.c:995:6: */
	var tok uintptr = tokenize(tls, new_file(tls, ts+10802, 1, buf))
	add_macro(tls, name, uint8(1), tok)
}

func undef_macro(tls *libc.TLS, name uintptr) { /* preprocess.c:1000:6: */
	hashmap_delete(tls, uintptr(unsafe.Pointer(&macros)), name)
}

func add_builtin(tls *libc.TLS, name uintptr, fn uintptr) uintptr { /* preprocess.c:1004:14: */
	var m uintptr = add_macro(tls, name, uint8(1), uintptr(0))
	(*Macro)(unsafe.Pointer(m)).handler = fn
	return m
}

func file_macro(tls *libc.TLS, tmpl uintptr) uintptr { /* preprocess.c:1010:14: */
	for (*Token)(unsafe.Pointer(tmpl)).origin != 0 {
		tmpl = (*Token)(unsafe.Pointer(tmpl)).origin
	}
	return new_str_token(tls, (*File)(unsafe.Pointer((*Token)(unsafe.Pointer(tmpl)).file)).display_name, tmpl)
}

func line_macro(tls *libc.TLS, tmpl uintptr) uintptr { /* preprocess.c:1016:14: */
	for (*Token)(unsafe.Pointer(tmpl)).origin != 0 {
		tmpl = (*Token)(unsafe.Pointer(tmpl)).origin
	}
	var i int32 = (*Token)(unsafe.Pointer(tmpl)).line_no + (*File)(unsafe.Pointer((*Token)(unsafe.Pointer(tmpl)).file)).line_delta
	return new_num_token(tls, i, tmpl)
}

// __COUNTER__ is expanded to serial values starting from 0.
func counter_macro(tls *libc.TLS, tmpl uintptr) uintptr { /* preprocess.c:1024:14: */
	return new_num_token(tls, libc.PostIncInt32(&i1, 1), tmpl)
}

var i1 int32 = 0 /* preprocess.c:1025:14 */

// __TIMESTAMP__ is expanded to a string describing the last
// modification time of the current file. E.g.
// "Fri Jul 24 01:32:50 2020"
func timestamp_macro(tls *libc.TLS, tmpl uintptr) uintptr { /* preprocess.c:1032:14: */
	bp := tls.Alloc(174)
	defer tls.Free(174)

	// var st stat at bp, 144

	if libc.Xstat(tls, (*File)(unsafe.Pointer((*Token)(unsafe.Pointer(tmpl)).file)).name, bp) != 0 {
		return new_str_token(tls, ts+10813, tmpl)
	}
	// var buf [30]int8 at bp+144, 30

	libc.Xctime_r(tls, bp+88, bp+144)
	*(*int8)(unsafe.Pointer(bp + 144 + 24)) = int8(0)
	return new_str_token(tls, bp+144, tmpl)
}

func base_file_macro(tls *libc.TLS, tmpl uintptr) uintptr { /* preprocess.c:1043:14: */
	return new_str_token(tls, base_file, tmpl)
}

// __DATE__ is expanded to the current date, e.g. "May 17 2020".
func format_date(tls *libc.TLS, tm1 uintptr) uintptr { /* preprocess.c:1048:13: */
	bp := tls.Alloc(24)
	defer tls.Free(24)

	return format(tls, ts+10838, libc.VaList(bp, uintptr(unsafe.Pointer(&mon))+uintptr((*tm)(unsafe.Pointer(tm1)).tm_mon)*4, (*tm)(unsafe.Pointer(tm1)).tm_mday, (*tm)(unsafe.Pointer(tm1)).tm_year+1900))
}

var mon = [12][4]int8{
	*(*[4]int8)(unsafe.Pointer(ts + 10850)), *(*[4]int8)(unsafe.Pointer(ts + 10854)), *(*[4]int8)(unsafe.Pointer(ts + 10858)), *(*[4]int8)(unsafe.Pointer(ts + 10862)), *(*[4]int8)(unsafe.Pointer(ts + 10866)), *(*[4]int8)(unsafe.Pointer(ts + 10870)),
	*(*[4]int8)(unsafe.Pointer(ts + 10874)), *(*[4]int8)(unsafe.Pointer(ts + 10878)), *(*[4]int8)(unsafe.Pointer(ts + 10882)), *(*[4]int8)(unsafe.Pointer(ts + 10886)), *(*[4]int8)(unsafe.Pointer(ts + 10890)), *(*[4]int8)(unsafe.Pointer(ts + 10894)),
} /* preprocess.c:1049:15 */

// __TIME__ is expanded to the current time, e.g. "13:34:03".
func format_time(tls *libc.TLS, tm1 uintptr) uintptr { /* preprocess.c:1058:13: */
	bp := tls.Alloc(24)
	defer tls.Free(24)

	return format(tls, ts+10898, libc.VaList(bp, (*tm)(unsafe.Pointer(tm1)).tm_hour, (*tm)(unsafe.Pointer(tm1)).tm_min, (*tm)(unsafe.Pointer(tm1)).tm_sec))
}

func init_macros(tls *libc.TLS) { /* preprocess.c:1062:6: */
	bp := tls.Alloc(8)
	defer tls.Free(8)

	// Define predefined macros
	define_macro(tls, ts+10915, ts+7032)
	define_macro(tls, ts+10921, ts+7032)
	define_macro(tls, ts+10946, ts+7032)
	define_macro(tls, ts+10954, ts+7032)
	define_macro(tls, ts+10963, ts+10981)
	define_macro(tls, ts+10983, ts+11000)
	define_macro(tls, ts+11002, ts+11000)
	define_macro(tls, ts+11017, ts+10981)
	define_macro(tls, ts+11040, ts+10981)
	define_macro(tls, ts+11061, ts+10981)
	define_macro(tls, ts+11077, ts+10981)
	define_macro(tls, ts+11096, ts+10981)
	define_macro(tls, ts+11117, ts+11134)
	define_macro(tls, ts+11136, ts+10981)
	define_macro(tls, ts+11154, ts+11168)
	define_macro(tls, ts+11182, ts+7032)
	define_macro(tls, ts+11198, ts+7032)
	define_macro(tls, ts+11218, ts+7032)
	define_macro(tls, ts+11234, ts+7032)
	define_macro(tls, ts+11250, ts+11267)
	define_macro(tls, ts+11275, ts+7032)
	define_macro(tls, ts+11284, ts+8875)
	define_macro(tls, ts+11306, ts+9801)
	define_macro(tls, ts+11318, ts+7032)
	define_macro(tls, ts+11326, ts+7032)
	define_macro(tls, ts+11336, ts+7032)
	define_macro(tls, ts+11348, ts+8586)
	define_macro(tls, ts+11358, ts+7032)
	define_macro(tls, ts+11372, ts+8413)
	define_macro(tls, ts+11383, ts+7032)
	define_macro(tls, ts+11391, ts+7032)
	define_macro(tls, ts+11401, ts+8788)
	define_macro(tls, ts+11412, ts+8737)
	define_macro(tls, ts+11423, ts+7032)
	define_macro(tls, ts+11430, ts+7032)
	define_macro(tls, ts+11439, ts+8592)
	define_macro(tls, ts+11452, ts+7032)
	define_macro(tls, ts+11461, ts+7032)
	define_macro(tls, ts+11472, ts+7032)
	define_macro(tls, ts+11478, ts+7032)

	add_builtin(tls, ts+11483, *(*uintptr)(unsafe.Pointer(&struct {
		f func(*libc.TLS, uintptr) uintptr
	}{file_macro})))
	add_builtin(tls, ts+11492, *(*uintptr)(unsafe.Pointer(&struct {
		f func(*libc.TLS, uintptr) uintptr
	}{line_macro})))
	add_builtin(tls, ts+11501, *(*uintptr)(unsafe.Pointer(&struct {
		f func(*libc.TLS, uintptr) uintptr
	}{counter_macro})))
	add_builtin(tls, ts+11513, *(*uintptr)(unsafe.Pointer(&struct {
		f func(*libc.TLS, uintptr) uintptr
	}{timestamp_macro})))
	add_builtin(tls, ts+11527, *(*uintptr)(unsafe.Pointer(&struct {
		f func(*libc.TLS, uintptr) uintptr
	}{base_file_macro})))

	*(*time_t)(unsafe.Pointer(bp /* now */)) = libc.Xtime(tls, uintptr(0))
	var tm1 uintptr = libc.Xlocaltime(tls, bp)
	define_macro(tls, ts+11541, format_date(tls, tm1))
	define_macro(tls, ts+11550, format_time(tls, tm1))
}

type StringKind = uint32 /* preprocess.c:1119:3 */

func getStringKind(tls *libc.TLS, tok uintptr) StringKind { /* preprocess.c:1121:19: */
	bp := tls.Alloc(16)
	defer tls.Free(16)

	if !(libc.Xstrcmp(tls, (*Token)(unsafe.Pointer(tok)).loc, ts+11559) != 0) {
		return STR_UTF8
	}

	switch int32(*(*int8)(unsafe.Pointer((*Token)(unsafe.Pointer(tok)).loc))) {
	case '"':
		return STR_NONE
	case 'u':
		return STR_UTF16
	case 'U':
		return STR_UTF32
	case 'L':
		return STR_WIDE
	}
	error(tls, ts+217, libc.VaList(bp, ts+11562, 1131))
	return StringKind(0)
}

// Concatenate adjacent string literals into a single string literal
// as per the C spec.
func join_adjacent_string_literals(tls *libc.TLS, tok uintptr) { /* preprocess.c:1136:13: */
	// First pass: If regular string literals are adjacent to wide
	// string literals, regular string literals are converted to a wide
	// type before concatenation. In this pass, we do the conversion.
	{
		var tok1 uintptr = tok
		for (*Token)(unsafe.Pointer(tok1)).kind != TK_EOF {
			if (*Token)(unsafe.Pointer(tok1)).kind != TK_STR || (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(tok1)).next)).kind != TK_STR {
				tok1 = (*Token)(unsafe.Pointer(tok1)).next
				continue
			}

			var kind StringKind = getStringKind(tls, tok1)
			var basety uintptr = (*Type)(unsafe.Pointer((*Token)(unsafe.Pointer(tok1)).ty)).base

			{
				var t uintptr = (*Token)(unsafe.Pointer(tok1)).next
				for ; (*Token)(unsafe.Pointer(t)).kind == TK_STR; t = (*Token)(unsafe.Pointer(t)).next {
					var k StringKind = getStringKind(tls, t)
					if kind == STR_NONE {
						kind = k
						basety = (*Type)(unsafe.Pointer((*Token)(unsafe.Pointer(t)).ty)).base
					} else if k != STR_NONE && kind != k {
						error_tok(tls, t, ts+11575, 0)
					}
				}
			}

			if (*Type)(unsafe.Pointer(basety)).size > 1 {
				var t uintptr = tok1
				for ; (*Token)(unsafe.Pointer(t)).kind == TK_STR; t = (*Token)(unsafe.Pointer(t)).next {
					if (*Type)(unsafe.Pointer((*Type)(unsafe.Pointer((*Token)(unsafe.Pointer(t)).ty)).base)).size == 1 {
						*(*Token)(unsafe.Pointer(t)) = *(*Token)(unsafe.Pointer(tokenize_string_literal(tls, t, basety)))
					}
				}
			}

			for (*Token)(unsafe.Pointer(tok1)).kind == TK_STR {
				tok1 = (*Token)(unsafe.Pointer(tok1)).next
			}
		}
	}

	// Second pass: concatenate adjacent string literals.
	{
		var tok11 uintptr = tok
		for (*Token)(unsafe.Pointer(tok11)).kind != TK_EOF {
			if (*Token)(unsafe.Pointer(tok11)).kind != TK_STR || (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(tok11)).next)).kind != TK_STR {
				tok11 = (*Token)(unsafe.Pointer(tok11)).next
				continue
			}

			var tok2 uintptr = (*Token)(unsafe.Pointer(tok11)).next
			for (*Token)(unsafe.Pointer(tok2)).kind == TK_STR {
				tok2 = (*Token)(unsafe.Pointer(tok2)).next
			}

			var len int32 = (*Type)(unsafe.Pointer((*Token)(unsafe.Pointer(tok11)).ty)).array_len
			{
				var t uintptr = (*Token)(unsafe.Pointer(tok11)).next
				for ; t != tok2; t = (*Token)(unsafe.Pointer(t)).next {
					len = len + (*Type)(unsafe.Pointer((*Token)(unsafe.Pointer(t)).ty)).array_len - 1
				}
			}

			var buf uintptr = libc.Xcalloc(tls, uint64((*Type)(unsafe.Pointer((*Type)(unsafe.Pointer((*Token)(unsafe.Pointer(tok11)).ty)).base)).size), uint64(len))

			var i int32 = 0
			{
				var t1 uintptr = tok11
				for ; t1 != tok2; t1 = (*Token)(unsafe.Pointer(t1)).next {
					libc.Xmemcpy(tls, buf+uintptr(i), (*Token)(unsafe.Pointer(t1)).str, uint64((*Type)(unsafe.Pointer((*Token)(unsafe.Pointer(t1)).ty)).size))
					i = i + (*Type)(unsafe.Pointer((*Token)(unsafe.Pointer(t1)).ty)).size - (*Type)(unsafe.Pointer((*Type)(unsafe.Pointer((*Token)(unsafe.Pointer(t1)).ty)).base)).size
				}
			}

			*(*Token)(unsafe.Pointer(tok11)) = *(*Token)(unsafe.Pointer(copy_token(tls, tok11)))
			(*Token)(unsafe.Pointer(tok11)).ty = array_of(tls, (*Type)(unsafe.Pointer((*Token)(unsafe.Pointer(tok11)).ty)).base, len)
			(*Token)(unsafe.Pointer(tok11)).str = buf
			(*Token)(unsafe.Pointer(tok11)).next = tok2
			tok11 = tok2
		}
	}
}

// Entry point function of the preprocessor.
func preprocess(tls *libc.TLS, tok uintptr) uintptr { /* preprocess.c:1202:7: */
	tok = preprocess2(tls, tok)
	if cond_incl != 0 {
		error_tok(tls, (*CondIncl)(unsafe.Pointer(cond_incl)).tok, ts+11633, 0)
	}
	convert_pp_tokens(tls, tok)
	join_adjacent_string_literals(tls, tok)

	{
		var t uintptr = tok
		for ; t != 0; t = (*Token)(unsafe.Pointer(t)).next {
			*(*int32)(unsafe.Pointer(t + 80)) += (*Token)(unsafe.Pointer(t)).line_delta
		}
	}
	return tok
}

func strarray_push(tls *libc.TLS, arr uintptr, s uintptr) { /* strings.c:3:6: */
	if !(int32((*StringArray)(unsafe.Pointer(arr)).data) != 0) {
		(*StringArray)(unsafe.Pointer(arr)).data = libc.Xcalloc(tls, uint64(8), uint64(unsafe.Sizeof(uintptr(0))))
		(*StringArray)(unsafe.Pointer(arr)).capacity = 8
	}

	if (*StringArray)(unsafe.Pointer(arr)).capacity == (*StringArray)(unsafe.Pointer(arr)).len {
		(*StringArray)(unsafe.Pointer(arr)).data = libc.Xrealloc(tls, (*StringArray)(unsafe.Pointer(arr)).data, uint64(unsafe.Sizeof(uintptr(0)))*uint64((*StringArray)(unsafe.Pointer(arr)).capacity)*uint64(2))
		*(*int32)(unsafe.Pointer(arr + 8)) *= 2
		{
			var i int32 = (*StringArray)(unsafe.Pointer(arr)).len
			for ; i < (*StringArray)(unsafe.Pointer(arr)).capacity; i++ {
				*(*uintptr)(unsafe.Pointer((*StringArray)(unsafe.Pointer(arr)).data + uintptr(i)*8)) = uintptr(0)
			}
		}
	}

	*(*uintptr)(unsafe.Pointer((*StringArray)(unsafe.Pointer(arr)).data + uintptr(libc.PostIncInt32(&(*StringArray)(unsafe.Pointer(arr)).len, 1))*8)) = s
}

// Takes a printf-style format string and returns a formatted string.
func format(tls *libc.TLS, fmt uintptr, va uintptr) uintptr { /* strings.c:20:6: */
	var ap va_list
	_ = ap
	ap = va
	var len int32 = libc.Xvsnprintf(tls, uintptr(0), uint64(0), fmt, ap)
	_ = ap

	var buf uintptr = libc.Xcalloc(tls, uint64(1), uint64(len+1))

	ap = va
	libc.Xvsnprintf(tls, buf, uint64(len+1), fmt, ap)
	_ = ap
	return buf
}

// Input file
var current_file uintptr /* tokenize.c:4:13: */

// A list of all input files.
var input_files uintptr /* tokenize.c:7:13: */

// True if the current position is at the beginning of a line
var at_bol uint8 /* tokenize.c:10:13: */

// True if the current position follows a space character
var has_space uint8 /* tokenize.c:13:13: */

// Reports an error and exit.
func error(tls *libc.TLS, fmt uintptr, va uintptr) { /* tokenize.c:16:6: */
	var ap va_list
	_ = ap
	ap = va
	libc.Xvfprintf(tls, libc.Xstderr, fmt, ap)
	libc.Xfprintf(tls, libc.Xstderr, ts+112, 0)
	libc.Xexit(tls, 1)
}

// Reports an error message in the following format.
//
// foo.c:10: x = y + 1;
//
//	^ <error message here>
func verror_at(tls *libc.TLS, filename uintptr, input uintptr, line_no int32, loc uintptr, fmt uintptr, ap va_list) { /* tokenize.c:28:13: */
	bp := tls.Alloc(48)
	defer tls.Free(48)

	// Find a line containing `loc`.
	var line uintptr = loc
	for input < line && int32(*(*int8)(unsafe.Pointer(line + libc.UintptrFromInt32(-1)))) != '\n' {
		line--
	}

	var end uintptr = loc
	for *(*int8)(unsafe.Pointer(end)) != 0 && int32(*(*int8)(unsafe.Pointer(end))) != '\n' {
		end++
	}

	// Print out the line.
	var indent int32 = libc.Xfprintf(tls, libc.Xstderr, ts+11668, libc.VaList(bp, filename, line_no))
	libc.Xfprintf(tls, libc.Xstderr, ts+11676, libc.VaList(bp+16, int32((int64(end)-int64(line))/1), line))

	// Show the error message.
	var pos int32 = display_width(tls, line, int32((int64(loc)-int64(line))/1)) + indent

	libc.Xfprintf(tls, libc.Xstderr, ts+11682, libc.VaList(bp+32, pos, ts+8875)) // print pos spaces.
	libc.Xfprintf(tls, libc.Xstderr, ts+11686, 0)
	libc.Xvfprintf(tls, libc.Xstderr, fmt, ap)
	libc.Xfprintf(tls, libc.Xstderr, ts+112, 0)
}

func error_at(tls *libc.TLS, loc uintptr, fmt uintptr, va uintptr) { /* tokenize.c:52:6: */
	var line_no int32 = 1
	{
		var p uintptr = (*File)(unsafe.Pointer(current_file)).contents
		for ; p < loc; p++ {
			if int32(*(*int8)(unsafe.Pointer(p))) == '\n' {
				line_no++
			}
		}
	}
	var ap va_list
	_ = ap
	ap = va
	verror_at(tls, (*File)(unsafe.Pointer(current_file)).name, (*File)(unsafe.Pointer(current_file)).contents, line_no, loc, fmt, ap)
	libc.Xexit(tls, 1)
}

func error_tok(tls *libc.TLS, tok uintptr, fmt uintptr, va uintptr) { /* tokenize.c:64:6: */
	var ap va_list
	_ = ap
	ap = va
	verror_at(tls, (*File)(unsafe.Pointer((*Token)(unsafe.Pointer(tok)).file)).name, (*File)(unsafe.Pointer((*Token)(unsafe.Pointer(tok)).file)).contents, (*Token)(unsafe.Pointer(tok)).line_no, (*Token)(unsafe.Pointer(tok)).loc, fmt, ap)
	libc.Xexit(tls, 1)
}

func warn_tok(tls *libc.TLS, tok uintptr, fmt uintptr, va uintptr) { /* tokenize.c:71:6: */
	var ap va_list
	_ = ap
	ap = va
	verror_at(tls, (*File)(unsafe.Pointer((*Token)(unsafe.Pointer(tok)).file)).name, (*File)(unsafe.Pointer((*Token)(unsafe.Pointer(tok)).file)).contents, (*Token)(unsafe.Pointer(tok)).line_no, (*Token)(unsafe.Pointer(tok)).loc, fmt, ap)
	_ = ap
}

// Consumes the current token if it matches `op`.
func equal(tls *libc.TLS, tok uintptr, op uintptr) uint8 { /* tokenize.c:79:6: */
	return uint8(libc.Bool32(libc.Xmemcmp(tls, (*Token)(unsafe.Pointer(tok)).loc, op, uint64((*Token)(unsafe.Pointer(tok)).len)) == 0 && int32(*(*int8)(unsafe.Pointer(op + uintptr((*Token)(unsafe.Pointer(tok)).len)))) == 0))
}

// Ensure that the current token is `op`.
func skip(tls *libc.TLS, tok uintptr, op uintptr) uintptr { /* tokenize.c:84:7: */
	bp := tls.Alloc(8)
	defer tls.Free(8)

	if !(equal(tls, tok, op) != 0) {
		error_tok(tls, tok, ts+11689, libc.VaList(bp, op))
	}
	return (*Token)(unsafe.Pointer(tok)).next
}

func consume(tls *libc.TLS, rest uintptr, tok uintptr, str uintptr) uint8 { /* tokenize.c:90:6: */
	if equal(tls, tok, str) != 0 {
		*(*uintptr)(unsafe.Pointer(rest)) = (*Token)(unsafe.Pointer(tok)).next
		return uint8(1)
	}
	*(*uintptr)(unsafe.Pointer(rest)) = tok
	return uint8(0)
}

// Create a new token.
func new_token(tls *libc.TLS, kind TokenKind, start uintptr, end uintptr) uintptr { /* tokenize.c:100:14: */
	var tok uintptr = libc.Xcalloc(tls, uint64(1), uint64(unsafe.Sizeof(Token{})))
	(*Token)(unsafe.Pointer(tok)).kind = kind
	(*Token)(unsafe.Pointer(tok)).loc = start
	(*Token)(unsafe.Pointer(tok)).len = int32((int64(end) - int64(start)) / 1)
	(*Token)(unsafe.Pointer(tok)).file = current_file
	(*Token)(unsafe.Pointer(tok)).filename = (*File)(unsafe.Pointer(current_file)).display_name
	(*Token)(unsafe.Pointer(tok)).at_bol = at_bol
	(*Token)(unsafe.Pointer(tok)).has_space = has_space

	at_bol = libc.AssignPtrUint8(uintptr(unsafe.Pointer(&has_space)), uint8(0))
	return tok
}

func startswith(tls *libc.TLS, p uintptr, q uintptr) uint8 { /* tokenize.c:114:13: */
	return uint8(libc.Bool32(libc.Xstrncmp(tls, p, q, libc.Xstrlen(tls, q)) == 0))
}

// Read an identifier and returns the length of it.
// If p does not point to a valid identifier, 0 is returned.
func read_ident(tls *libc.TLS, start uintptr) int32 { /* tokenize.c:120:12: */
	bp := tls.Alloc(16)
	defer tls.Free(16)

	*(*uintptr)(unsafe.Pointer(bp /* p */)) = start
	var c uint32_t = decode_utf8(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* p */)))
	if !(is_ident1(tls, c) != 0) {
		return 0
	}

	for {
		// var q uintptr at bp+8, 8

		c = decode_utf8(tls, bp+8, *(*uintptr)(unsafe.Pointer(bp /* p */)))
		if !(is_ident2(tls, c) != 0) {
			return int32((int64(*(*uintptr)(unsafe.Pointer(bp))) - int64(start)) / 1)
		}
		*(*uintptr)(unsafe.Pointer(bp /* p */)) = *(*uintptr)(unsafe.Pointer(bp + 8 /* q */))
	}
	return int32(0)
}

func from_hex(tls *libc.TLS, c int8) int32 { /* tokenize.c:135:12: */
	if '0' <= int32(c) && int32(c) <= '9' {
		return int32(c) - '0'
	}
	if 'a' <= int32(c) && int32(c) <= 'f' {
		return int32(c) - 'a' + 10
	}
	return int32(c) - 'A' + 10
}

// Read a punctuator token from p and returns its length.
func read_punct(tls *libc.TLS, p uintptr) int32 { /* tokenize.c:144:12: */

	{
		var i int32 = 0
		for ; uint64(i) < uint64(unsafe.Sizeof(kw1))/uint64(unsafe.Sizeof(uintptr(0))); i++ {
			if startswith(tls, p, kw1[i]) != 0 {
				return int32(libc.Xstrlen(tls, kw1[i]))
			}
		}
	}

	if int32(*(*uint16)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(libc.X__ctype_b_loc(tls))) + uintptr(int32(*(*int8)(unsafe.Pointer(p))))*2)))&int32(_ISpunct) != 0 {
		return 1
	}
	return 0
}

var kw1 = [23]uintptr{
	ts + 9468, ts + 9472, ts + 8825, ts + 9490, ts + 9493, ts + 9498, ts + 9503, ts + 9662, ts + 9444,
	ts + 9447, ts + 9450, ts + 9453, ts + 9569, ts + 9572, ts + 9456, ts + 9459, ts + 9462, ts + 9465, ts + 9481,
	ts + 9478, ts + 9506, ts + 9509, ts + 10469,
} /* tokenize.c:145:15 */

func is_keyword(tls *libc.TLS, tok uintptr) uint8 { /* tokenize.c:158:13: */

	if map2.capacity == 0 {

		{
			var i int32 = 0
			for ; uint64(i) < uint64(unsafe.Sizeof(kw2))/uint64(unsafe.Sizeof(uintptr(0))); i++ {
				hashmap_put(tls, uintptr(unsafe.Pointer(&map2)), kw2[i], uintptr(1))
			}
		}
	}

	return uint8(hashmap_get2(tls, uintptr(unsafe.Pointer(&map2)), (*Token)(unsafe.Pointer(tok)).loc, (*Token)(unsafe.Pointer(tok)).len))
}

var map2 HashMap /* tokenize.c:159:18: */
var kw2 = [45]uintptr{
	ts + 9243, ts + 9250, ts + 9253, ts + 9332, ts + 9336, ts + 8766, ts + 9794, ts + 8755,
	ts + 8719, ts + 8726, ts + 8760, ts + 8770, ts + 8744, ts + 8391, ts + 8749,
	ts + 8732, ts + 8399, ts + 9349, ts + 9354, ts + 9372, ts + 9258, ts + 9265,
	ts + 9310, ts + 8406, ts + 9801, ts + 8670, ts + 9342, ts + 8788,
	ts + 8795, ts + 8586, ts + 8592, ts + 8601, ts + 8606, ts + 8615,
	ts + 8624, ts + 8635, ts + 8648, ts + 8775, ts + 8781,
	ts + 8737, ts + 9345, ts + 8420, ts + 8434, ts + 8658,
	ts + 9575,
} /* tokenize.c:162:17 */

func read_escaped_char(tls *libc.TLS, new_pos uintptr, p uintptr) int32 { /* tokenize.c:180:12: */
	if '0' <= int32(*(*int8)(unsafe.Pointer(p))) && int32(*(*int8)(unsafe.Pointer(p))) <= '7' {
		// Read an octal number.
		var c int32 = int32(*(*int8)(unsafe.Pointer(libc.PostIncUintptr(&p, 1)))) - '0'
		if '0' <= int32(*(*int8)(unsafe.Pointer(p))) && int32(*(*int8)(unsafe.Pointer(p))) <= '7' {
			c = c<<3 + (int32(*(*int8)(unsafe.Pointer(libc.PostIncUintptr(&p, 1)))) - '0')
			if '0' <= int32(*(*int8)(unsafe.Pointer(p))) && int32(*(*int8)(unsafe.Pointer(p))) <= '7' {
				c = c<<3 + (int32(*(*int8)(unsafe.Pointer(libc.PostIncUintptr(&p, 1)))) - '0')
			}
		}
		*(*uintptr)(unsafe.Pointer(new_pos)) = p
		return c
	}

	if int32(*(*int8)(unsafe.Pointer(p))) == 'x' {
		// Read a hexadecimal number.
		p++
		if !(int32(*(*uint16)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(libc.X__ctype_b_loc(tls))) + uintptr(int32(*(*int8)(unsafe.Pointer(p))))*2)))&int32(_ISxdigit) != 0) {
			error_at(tls, p, ts+11703, 0)
		}

		var c int32 = 0
		for ; int32(*(*uint16)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(libc.X__ctype_b_loc(tls))) + uintptr(int32(*(*int8)(unsafe.Pointer(p))))*2)))&int32(_ISxdigit) != 0; p++ {
			c = c<<4 + from_hex(tls, *(*int8)(unsafe.Pointer(p)))
		}
		*(*uintptr)(unsafe.Pointer(new_pos)) = p
		return c
	}

	*(*uintptr)(unsafe.Pointer(new_pos)) = p + uintptr(1)

	// Escape sequences are defined using themselves here. E.g.
	// '\n' is implemented using '\n'. This tautological definition
	// works because the compiler that compiles our compiler knows
	// what '\n' actually is. In other words, we "inherit" the ASCII
	// code of '\n' from the compiler that compiles our compiler,
	// so we don't have to teach the actual code here.
	//
	// This fact has huge implications not only for the correctness
	// of the compiler but also for the security of the generated code.
	// For more info, read "Reflections on Trusting Trust" by Ken Thompson.
	// https://github.com/rui314/chibicc/wiki/thompson1984.pdf
	switch int32(*(*int8)(unsafe.Pointer(p))) {
	case 'a':
		return '\a'
	case 'b':
		return '\b'
	case 't':
		return '\t'
	case 'n':
		return '\n'
	case 'v':
		return '\v'
	case 'f':
		return '\f'
	case 'r':
		return '\r'
	// [GNU] \e for the ASCII escape character is a GNU C extension.
	case 'e':
		return 27
	default:
		return int32(*(*int8)(unsafe.Pointer(p)))
	}
	return int32(0)
}

// Find a closing double-quote.
func string_literal_end(tls *libc.TLS, p uintptr) uintptr { /* tokenize.c:234:13: */
	var start uintptr = p
	for ; int32(*(*int8)(unsafe.Pointer(p))) != '"'; p++ {
		if int32(*(*int8)(unsafe.Pointer(p))) == '\n' || int32(*(*int8)(unsafe.Pointer(p))) == 0 {
			error_at(tls, start, ts+11731, 0)
		}
		if int32(*(*int8)(unsafe.Pointer(p))) == '\\' {
			p++
		}
	}
	return p
}

func read_string_literal(tls *libc.TLS, start uintptr, quote uintptr) uintptr { /* tokenize.c:245:14: */
	bp := tls.Alloc(8)
	defer tls.Free(8)

	var end uintptr = string_literal_end(tls, quote+uintptr(1))
	var buf uintptr = libc.Xcalloc(tls, uint64(1), uint64((int64(end)-int64(quote))/1))
	var len int32 = 0

	{
		*(*uintptr)(unsafe.Pointer(bp /* p */)) = quote + uintptr(1)
		for *(*uintptr)(unsafe.Pointer(bp)) < end {
			if int32(*(*int8)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp))))) == '\\' {
				*(*int8)(unsafe.Pointer(buf + uintptr(libc.PostIncInt32(&len, 1)))) = int8(read_escaped_char(tls, bp, *(*uintptr)(unsafe.Pointer(bp))+uintptr(1)))
			} else {
				*(*int8)(unsafe.Pointer(buf + uintptr(libc.PostIncInt32(&len, 1)))) = *(*int8)(unsafe.Pointer(libc.PostIncUintptr(&*(*uintptr)(unsafe.Pointer(bp /* p */)), 1)))
			}
		}
	}

	var tok uintptr = new_token(tls, TK_STR, start, end+uintptr(1))
	(*Token)(unsafe.Pointer(tok)).ty = array_of(tls, ty_char, len+1)
	(*Token)(unsafe.Pointer(tok)).str = buf
	return tok
}

// Read a UTF-8-encoded string literal and transcode it in UTF-16.
//
// UTF-16 is yet another variable-width encoding for Unicode. Code
// points smaller than U+10000 are encoded in 2 bytes. Code points
// equal to or larger than that are encoded in 4 bytes. Each 2 bytes
// in the 4 byte sequence is called "surrogate", and a 4 byte sequence
// is called a "surrogate pair".
func read_utf16_string_literal(tls *libc.TLS, start uintptr, quote uintptr) uintptr { /* tokenize.c:270:14: */
	bp := tls.Alloc(8)
	defer tls.Free(8)

	var end uintptr = string_literal_end(tls, quote+uintptr(1))
	var buf uintptr = libc.Xcalloc(tls, uint64(2), uint64((int64(end)-int64(start))/1))
	var len int32 = 0

	{
		*(*uintptr)(unsafe.Pointer(bp /* p */)) = quote + uintptr(1)
		for *(*uintptr)(unsafe.Pointer(bp)) < end {
			if int32(*(*int8)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp))))) == '\\' {
				*(*uint16_t)(unsafe.Pointer(buf + uintptr(libc.PostIncInt32(&len, 1))*2)) = uint16_t(read_escaped_char(tls, bp, *(*uintptr)(unsafe.Pointer(bp))+uintptr(1)))
				continue
			}

			var c uint32_t = decode_utf8(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* p */)))
			if c < uint32_t(0x10000) {
				// Encode a code point in 2 bytes.
				*(*uint16_t)(unsafe.Pointer(buf + uintptr(libc.PostIncInt32(&len, 1))*2)) = uint16_t(c)
			} else {
				// Encode a code point in 4 bytes.
				c = c - uint32_t(0x10000)
				*(*uint16_t)(unsafe.Pointer(buf + uintptr(libc.PostIncInt32(&len, 1))*2)) = uint16_t(uint32_t(0xd800) + c>>10&uint32_t(0x3ff))
				*(*uint16_t)(unsafe.Pointer(buf + uintptr(libc.PostIncInt32(&len, 1))*2)) = uint16_t(uint32_t(0xdc00) + c&uint32_t(0x3ff))
			}
		}
	}

	var tok uintptr = new_token(tls, TK_STR, start, end+uintptr(1))
	(*Token)(unsafe.Pointer(tok)).ty = array_of(tls, ty_ushort, len+1)
	(*Token)(unsafe.Pointer(tok)).str = buf
	return tok
}

// Read a UTF-8-encoded string literal and transcode it in UTF-32.
//
// UTF-32 is a fixed-width encoding for Unicode. Each code point is
// encoded in 4 bytes.
func read_utf32_string_literal(tls *libc.TLS, start uintptr, quote uintptr, ty uintptr) uintptr { /* tokenize.c:303:14: */
	bp := tls.Alloc(8)
	defer tls.Free(8)

	var end uintptr = string_literal_end(tls, quote+uintptr(1))
	var buf uintptr = libc.Xcalloc(tls, uint64(4), uint64((int64(end)-int64(quote))/1))
	var len int32 = 0

	{
		*(*uintptr)(unsafe.Pointer(bp /* p */)) = quote + uintptr(1)
		for *(*uintptr)(unsafe.Pointer(bp)) < end {
			if int32(*(*int8)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp))))) == '\\' {
				*(*uint32_t)(unsafe.Pointer(buf + uintptr(libc.PostIncInt32(&len, 1))*4)) = uint32_t(read_escaped_char(tls, bp, *(*uintptr)(unsafe.Pointer(bp))+uintptr(1)))
			} else {
				*(*uint32_t)(unsafe.Pointer(buf + uintptr(libc.PostIncInt32(&len, 1))*4)) = decode_utf8(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* p */)))
			}
		}
	}

	var tok uintptr = new_token(tls, TK_STR, start, end+uintptr(1))
	(*Token)(unsafe.Pointer(tok)).ty = array_of(tls, ty, len+1)
	(*Token)(unsafe.Pointer(tok)).str = buf
	return tok
}

func read_char_literal(tls *libc.TLS, start uintptr, quote uintptr, ty uintptr) uintptr { /* tokenize.c:321:14: */
	bp := tls.Alloc(8)
	defer tls.Free(8)

	*(*uintptr)(unsafe.Pointer(bp /* p */)) = quote + uintptr(1)
	if int32(*(*int8)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp))))) == 0 {
		error_at(tls, start, ts+11755, 0)
	}
	var c int32
	if int32(*(*int8)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp))))) == '\\' {
		c = read_escaped_char(tls, bp, *(*uintptr)(unsafe.Pointer(bp))+uintptr(1))
	} else {
		c = int32(decode_utf8(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* p */))))
	}

	var end uintptr = libc.Xstrchr(tls, *(*uintptr)(unsafe.Pointer(bp /* p */)), '\'')
	if !(end != 0) {
		error_at(tls, *(*uintptr)(unsafe.Pointer(bp /* p */)), ts+11755, 0)
	}

	var tok uintptr = new_token(tls, TK_NUM, start, end+uintptr(1))
	(*Token)(unsafe.Pointer(tok)).val = int64_t(c)
	(*Token)(unsafe.Pointer(tok)).ty = ty
	return tok
}

