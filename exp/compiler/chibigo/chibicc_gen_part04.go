package main

import (
	"modernc.org/libc"
	"unsafe"
)

func struct_initializer1(tls *libc.TLS, rest uintptr, tok uintptr, init1 uintptr) { /* parse.c:1157:13: */
	bp := tls.Alloc(8)
	defer tls.Free(8)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8837)

	var mem uintptr = (*Type)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).ty)).members
	var first uint8 = uint8(1)

	for !(consume_end(tls, rest, *(*uintptr)(unsafe.Pointer(bp))) != 0) {
		if !(first != 0) {
			*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8331)
		}
		first = uint8(0)

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+6708) != 0 {
			mem = struct_designator(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)), (*Initializer)(unsafe.Pointer(init1)).ty)
			designation(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)), *(*uintptr)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).children + uintptr((*Member)(unsafe.Pointer(mem)).idx)*8)))
			mem = (*Member)(unsafe.Pointer(mem)).next
			continue
		}

		if mem != 0 {
			initializer2(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)), *(*uintptr)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).children + uintptr((*Member)(unsafe.Pointer(mem)).idx)*8)))
			mem = (*Member)(unsafe.Pointer(mem)).next
		} else {
			*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip_excess_element(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		}
	}
}

// struct-initializer2 = initializer ("," initializer)*
func struct_initializer2(tls *libc.TLS, rest uintptr, tok uintptr, init1 uintptr, mem uintptr) { /* parse.c:1185:13: */
	bp := tls.Alloc(8)
	defer tls.Free(8)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	var first uint8 = uint8(1)

	for ; mem != 0 && !(is_end(tls, *(*uintptr)(unsafe.Pointer(bp))) != 0); mem = (*Member)(unsafe.Pointer(mem)).next {
		var start uintptr = *(*uintptr)(unsafe.Pointer(bp /* tok */))

		if !(first != 0) {
			*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8331)
		}
		first = uint8(0)

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8831) != 0 || equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+6708) != 0 {
			*(*uintptr)(unsafe.Pointer(rest)) = start
			return
		}

		initializer2(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)), *(*uintptr)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).children + uintptr((*Member)(unsafe.Pointer(mem)).idx)*8)))
	}
	*(*uintptr)(unsafe.Pointer(rest)) = *(*uintptr)(unsafe.Pointer(bp /* tok */))
}

func union_initializer(tls *libc.TLS, rest uintptr, tok uintptr, init1 uintptr) { /* parse.c:1205:13: */
	bp := tls.Alloc(8)
	defer tls.Free(8)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	// Unlike structs, union initializers take only one initializer,
	// and that initializes the first union member by default.
	// You can initialize other member using a designated initializer.
	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8837) != 0 && equal(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp)))).next, ts+6708) != 0 {
		var mem uintptr = struct_designator(tls, bp, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next, (*Initializer)(unsafe.Pointer(init1)).ty)
		(*Initializer)(unsafe.Pointer(init1)).mem = mem
		designation(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)), *(*uintptr)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).children + uintptr((*Member)(unsafe.Pointer(mem)).idx)*8)))
		*(*uintptr)(unsafe.Pointer(rest)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8835)
		return
	}

	(*Initializer)(unsafe.Pointer(init1)).mem = (*Type)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).ty)).members

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8837) != 0 {
		initializer2(tls, bp, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next, *(*uintptr)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).children)))
		consume(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8331)
		*(*uintptr)(unsafe.Pointer(rest)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8835)
	} else {
		initializer2(tls, rest, *(*uintptr)(unsafe.Pointer(bp /* tok */)), *(*uintptr)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).children)))
	}
}

// initializer = string-initializer | array-initializer
//
//	| struct-initializer | union-initializer
//	| assign
func initializer2(tls *libc.TLS, rest uintptr, tok uintptr, init1 uintptr) { /* parse.c:1231:13: */
	bp := tls.Alloc(8)
	defer tls.Free(8)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	if (*Type)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).ty)).kind == TY_ARRAY && (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp)))).kind == TK_STR {
		string_initializer(tls, rest, *(*uintptr)(unsafe.Pointer(bp /* tok */)), init1)
		return
	}

	if (*Type)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).ty)).kind == TY_ARRAY {
		if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8837) != 0 {
			array_initializer1(tls, rest, *(*uintptr)(unsafe.Pointer(bp /* tok */)), init1)
		} else {
			array_initializer2(tls, rest, *(*uintptr)(unsafe.Pointer(bp /* tok */)), init1, 0)
		}
		return
	}

	if (*Type)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).ty)).kind == TY_STRUCT {
		if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8837) != 0 {
			struct_initializer1(tls, rest, *(*uintptr)(unsafe.Pointer(bp /* tok */)), init1)
			return
		}

		// A struct can be initialized with another struct. E.g.
		// `struct T x = y;` where y is a variable of type `struct T`.
		// Handle that case first.
		var expr uintptr = assign(tls, rest, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		add_type(tls, expr)
		if (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(expr)).ty)).kind == TY_STRUCT {
			(*Initializer)(unsafe.Pointer(init1)).expr = expr
			return
		}

		struct_initializer2(tls, rest, *(*uintptr)(unsafe.Pointer(bp /* tok */)), init1, (*Type)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).ty)).members)
		return
	}

	if (*Type)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).ty)).kind == TY_UNION {
		union_initializer(tls, rest, *(*uintptr)(unsafe.Pointer(bp /* tok */)), init1)
		return
	}

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8837) != 0 {
		// An initializer for a scalar variable can be surrounded by
		// braces. E.g. `int x = {3};`. Handle that case.
		initializer2(tls, bp, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next, init1)
		*(*uintptr)(unsafe.Pointer(rest)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8835)
		return
	}

	(*Initializer)(unsafe.Pointer(init1)).expr = assign(tls, rest, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
}

func copy_struct_type(tls *libc.TLS, ty uintptr) uintptr { /* parse.c:1281:13: */
	bp := tls.Alloc(56)
	defer tls.Free(56)

	ty = copy_type(tls, ty)

	*(*Member)(unsafe.Pointer(bp /* head */)) = Member{}
	var cur uintptr = bp /* &head */
	{
		var mem uintptr = (*Type)(unsafe.Pointer(ty)).members
		for ; mem != 0; mem = (*Member)(unsafe.Pointer(mem)).next {
			var m uintptr = libc.Xcalloc(tls, uint64(1), uint64(unsafe.Sizeof(Member{})))
			*(*Member)(unsafe.Pointer(m)) = *(*Member)(unsafe.Pointer(mem))
			cur = libc.AssignPtrUintptr(cur, m)
		}
	}

	(*Type)(unsafe.Pointer(ty)).members = (*Member)(unsafe.Pointer(bp /* &head */)).next
	return ty
}

func initializer(tls *libc.TLS, rest uintptr, tok uintptr, ty uintptr, new_ty uintptr) uintptr { /* parse.c:1296:20: */
	var init1 uintptr = new_initializer(tls, ty, uint8(1))
	initializer2(tls, rest, tok, init1)

	if ((*Type)(unsafe.Pointer(ty)).kind == TY_STRUCT || (*Type)(unsafe.Pointer(ty)).kind == TY_UNION) && (*Type)(unsafe.Pointer(ty)).is_flexible != 0 {
		ty = copy_struct_type(tls, ty)

		var mem uintptr = (*Type)(unsafe.Pointer(ty)).members
		for (*Member)(unsafe.Pointer(mem)).next != 0 {
			mem = (*Member)(unsafe.Pointer(mem)).next
		}
		(*Member)(unsafe.Pointer(mem)).ty = (*Initializer)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).children + uintptr((*Member)(unsafe.Pointer(mem)).idx)*8)))).ty
		*(*int32)(unsafe.Pointer(ty + 4)) += (*Type)(unsafe.Pointer((*Member)(unsafe.Pointer(mem)).ty)).size

		*(*uintptr)(unsafe.Pointer(new_ty)) = ty
		return init1
	}

	*(*uintptr)(unsafe.Pointer(new_ty)) = (*Initializer)(unsafe.Pointer(init1)).ty
	return init1
}

func init_desg_expr(tls *libc.TLS, desg uintptr, tok uintptr) uintptr { /* parse.c:1317:13: */
	if (*InitDesg)(unsafe.Pointer(desg)).__var != 0 {
		return new_var_node(tls, (*InitDesg)(unsafe.Pointer(desg)).__var, tok)
	}

	if (*InitDesg)(unsafe.Pointer(desg)).member != 0 {
		var node uintptr = new_unary(tls, ND_MEMBER, init_desg_expr(tls, (*InitDesg)(unsafe.Pointer(desg)).next, tok), tok)
		(*Node)(unsafe.Pointer(node)).member = (*InitDesg)(unsafe.Pointer(desg)).member
		return node
	}

	var lhs uintptr = init_desg_expr(tls, (*InitDesg)(unsafe.Pointer(desg)).next, tok)
	var rhs uintptr = new_num(tls, int64((*InitDesg)(unsafe.Pointer(desg)).idx), tok)
	return new_unary(tls, ND_DEREF, new_add(tls, lhs, rhs, tok), tok)
}

func create_lvar_init(tls *libc.TLS, init1 uintptr, ty uintptr, desg uintptr, tok uintptr) uintptr { /* parse.c:1332:13: */
	bp := tls.Alloc(96)
	defer tls.Free(96)

	if (*Type)(unsafe.Pointer(ty)).kind == TY_ARRAY {
		var node uintptr = new_node(tls, ND_NULL_EXPR, tok)
		{
			var i int32 = 0
			for ; i < (*Type)(unsafe.Pointer(ty)).array_len; i++ {
				*(*InitDesg)(unsafe.Pointer(bp /* desg2 */)) = InitDesg{next: desg, idx: i}
				var rhs uintptr = create_lvar_init(tls, *(*uintptr)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).children + uintptr(i)*8)), (*Type)(unsafe.Pointer(ty)).base, bp, tok)
				node = new_binary(tls, ND_COMMA, node, rhs, tok)
			}
		}
		return node
	}

	if (*Type)(unsafe.Pointer(ty)).kind == TY_STRUCT && !(int32((*Initializer)(unsafe.Pointer(init1)).expr) != 0) {
		var node uintptr = new_node(tls, ND_NULL_EXPR, tok)

		{
			var mem uintptr = (*Type)(unsafe.Pointer(ty)).members
			for ; mem != 0; mem = (*Member)(unsafe.Pointer(mem)).next {
				*(*InitDesg)(unsafe.Pointer(bp + 32 /* desg2 */)) = InitDesg{next: desg, member: mem}
				var rhs uintptr = create_lvar_init(tls, *(*uintptr)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).children + uintptr((*Member)(unsafe.Pointer(mem)).idx)*8)), (*Member)(unsafe.Pointer(mem)).ty, bp+32, tok)
				node = new_binary(tls, ND_COMMA, node, rhs, tok)
			}
		}
		return node
	}

	if (*Type)(unsafe.Pointer(ty)).kind == TY_UNION {
		var mem uintptr
		if (*Initializer)(unsafe.Pointer(init1)).mem != 0 {
			mem = (*Initializer)(unsafe.Pointer(init1)).mem
		} else {
			mem = (*Type)(unsafe.Pointer(ty)).members
		}
		*(*InitDesg)(unsafe.Pointer(bp + 64 /* desg2 */)) = InitDesg{next: desg, member: mem}
		return create_lvar_init(tls, *(*uintptr)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).children + uintptr((*Member)(unsafe.Pointer(mem)).idx)*8)), (*Member)(unsafe.Pointer(mem)).ty, bp+64, tok)
	}

	if !(int32((*Initializer)(unsafe.Pointer(init1)).expr) != 0) {
		return new_node(tls, ND_NULL_EXPR, tok)
	}

	var lhs uintptr = init_desg_expr(tls, desg, tok)
	return new_binary(tls, ND_ASSIGN, lhs, (*Initializer)(unsafe.Pointer(init1)).expr, tok)
}

// A variable definition with an initializer is a shorthand notation
// for a variable definition followed by assignments. This function
// generates assignment expressions for an initializer. For example,
// `int x[2][2] = {{6, 7}, {8, 9}}` is converted to the following
// expressions:
//
//	x[0][0] = 6;
//	x[0][1] = 7;
//	x[1][0] = 8;
//	x[1][1] = 9;
func lvar_initializer(tls *libc.TLS, rest uintptr, tok uintptr, var1 uintptr) uintptr { /* parse.c:1377:13: */
	bp := tls.Alloc(32)
	defer tls.Free(32)

	var init1 uintptr = initializer(tls, rest, tok, (*Obj)(unsafe.Pointer(var1)).ty, var1+16)
	*(*InitDesg)(unsafe.Pointer(bp /* desg */)) = InitDesg{__var: var1}

	// If a partial initializer list is given, the standard requires
	// that unspecified elements are set to 0. Here, we simply
	// zero-initialize the entire memory region of a variable before
	// initializing it with user-supplied values.
	var lhs uintptr = new_node(tls, ND_MEMZERO, tok)
	(*Node)(unsafe.Pointer(lhs)).__var = var1

	var rhs uintptr = create_lvar_init(tls, init1, (*Obj)(unsafe.Pointer(var1)).ty, bp, tok)
	return new_binary(tls, ND_COMMA, lhs, rhs, tok)
}

func read_buf(tls *libc.TLS, buf uintptr, sz int32) uint64_t { /* parse.c:1392:17: */
	bp := tls.Alloc(16)
	defer tls.Free(16)

	if sz == 1 {
		return uint64_t(*(*int8)(unsafe.Pointer(buf)))
	}
	if sz == 2 {
		return uint64_t(*(*uint16_t)(unsafe.Pointer(buf)))
	}
	if sz == 4 {
		return uint64_t(*(*uint32_t)(unsafe.Pointer(buf)))
	}
	if sz == 8 {
		return *(*uint64_t)(unsafe.Pointer(buf))
	}
	error(tls, ts+217, libc.VaList(bp, ts+8804, 1401))
	return uint64_t(0)
}

func write_buf(tls *libc.TLS, buf uintptr, val uint64_t, sz int32) { /* parse.c:1404:13: */
	bp := tls.Alloc(16)
	defer tls.Free(16)

	if sz == 1 {
		*(*int8)(unsafe.Pointer(buf)) = int8(val)
	} else if sz == 2 {
		*(*uint16_t)(unsafe.Pointer(buf)) = uint16_t(val)
	} else if sz == 4 {
		*(*uint32_t)(unsafe.Pointer(buf)) = uint32_t(val)
	} else if sz == 8 {
		*(*uint64_t)(unsafe.Pointer(buf)) = val
	} else {
		error(tls, ts+217, libc.VaList(bp, ts+8804, 1414))
	}
}

func write_gvar_data(tls *libc.TLS, cur uintptr, init1 uintptr, ty uintptr, buf uintptr, offset int32) uintptr { /* parse.c:1417:19: */
	bp := tls.Alloc(8)
	defer tls.Free(8)

	if (*Type)(unsafe.Pointer(ty)).kind == TY_ARRAY {
		var sz int32 = (*Type)(unsafe.Pointer((*Type)(unsafe.Pointer(ty)).base)).size
		{
			var i int32 = 0
			for ; i < (*Type)(unsafe.Pointer(ty)).array_len; i++ {
				cur = write_gvar_data(tls, cur, *(*uintptr)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).children + uintptr(i)*8)), (*Type)(unsafe.Pointer(ty)).base, buf, offset+sz*i)
			}
		}
		return cur
	}

	if (*Type)(unsafe.Pointer(ty)).kind == TY_STRUCT {
		{
			var mem uintptr = (*Type)(unsafe.Pointer(ty)).members
			for ; mem != 0; mem = (*Member)(unsafe.Pointer(mem)).next {
				if (*Member)(unsafe.Pointer(mem)).is_bitfield != 0 {
					var expr uintptr = (*Initializer)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).children + uintptr((*Member)(unsafe.Pointer(mem)).idx)*8)))).expr
					if !(expr != 0) {
						break
					}

					var loc uintptr = buf + uintptr(offset) + uintptr((*Member)(unsafe.Pointer(mem)).offset)
					var oldval uint64_t = read_buf(tls, loc, (*Type)(unsafe.Pointer((*Member)(unsafe.Pointer(mem)).ty)).size)
					var newval uint64_t = uint64_t(eval(tls, expr))
					var mask uint64_t = uint64_t(int64(1)<<(*Member)(unsafe.Pointer(mem)).bit_width - int64(1))
					var combined uint64_t = oldval | newval&mask<<(*Member)(unsafe.Pointer(mem)).bit_offset
					write_buf(tls, loc, combined, (*Type)(unsafe.Pointer((*Member)(unsafe.Pointer(mem)).ty)).size)
				} else {
					cur = write_gvar_data(tls, cur, *(*uintptr)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).children + uintptr((*Member)(unsafe.Pointer(mem)).idx)*8)), (*Member)(unsafe.Pointer(mem)).ty, buf,
						offset+(*Member)(unsafe.Pointer(mem)).offset)
				}
			}
		}
		return cur
	}

	if (*Type)(unsafe.Pointer(ty)).kind == TY_UNION {
		if !(int32((*Initializer)(unsafe.Pointer(init1)).mem) != 0) {
			return cur
		}
		return write_gvar_data(tls, cur, *(*uintptr)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).children + uintptr((*Member)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).mem)).idx)*8)),
			(*Member)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).mem)).ty, buf, offset)
	}

	if !(int32((*Initializer)(unsafe.Pointer(init1)).expr) != 0) {
		return cur
	}

	if (*Type)(unsafe.Pointer(ty)).kind == TY_FLOAT {
		*(*float32)(unsafe.Pointer(buf + uintptr(offset))) = float32(eval_double(tls, (*Initializer)(unsafe.Pointer(init1)).expr))
		return cur
	}

	if (*Type)(unsafe.Pointer(ty)).kind == TY_DOUBLE {
		*(*float64)(unsafe.Pointer(buf + uintptr(offset))) = eval_double(tls, (*Initializer)(unsafe.Pointer(init1)).expr)
		return cur
	}

	*(*uintptr)(unsafe.Pointer(bp /* label */)) = uintptr(0)
	var val uint64_t = uint64_t(eval2(tls, (*Initializer)(unsafe.Pointer(init1)).expr, bp))

	if !(*(*uintptr)(unsafe.Pointer(bp)) != 0) {
		write_buf(tls, buf+uintptr(offset), val, (*Type)(unsafe.Pointer(ty)).size)
		return cur
	}

	var rel uintptr = libc.Xcalloc(tls, uint64(1), uint64(unsafe.Sizeof(Relocation{})))
	(*Relocation)(unsafe.Pointer(rel)).offset = offset
	(*Relocation)(unsafe.Pointer(rel)).label = *(*uintptr)(unsafe.Pointer(bp /* label */))
	(*Relocation)(unsafe.Pointer(rel)).addend = int64(val)
	(*Relocation)(unsafe.Pointer(cur)).next = rel
	return (*Relocation)(unsafe.Pointer(cur)).next
}

// Initializers for global variables are evaluated at compile-time and
// embedded to .data section. This function serializes Initializer
// objects to a flat byte array. It is a compile error if an
// initializer list contains a non-constant expression.
func gvar_initializer(tls *libc.TLS, rest uintptr, tok uintptr, var1 uintptr) { /* parse.c:1487:13: */
	bp := tls.Alloc(32)
	defer tls.Free(32)

	var init1 uintptr = initializer(tls, rest, tok, (*Obj)(unsafe.Pointer(var1)).ty, var1+16)

	*(*Relocation)(unsafe.Pointer(bp /* head */)) = Relocation{}
	var buf uintptr = libc.Xcalloc(tls, uint64(1), uint64((*Type)(unsafe.Pointer((*Obj)(unsafe.Pointer(var1)).ty)).size))
	write_gvar_data(tls, bp, init1, (*Obj)(unsafe.Pointer(var1)).ty, buf, 0)
	(*Obj)(unsafe.Pointer(var1)).init_data = buf
	(*Obj)(unsafe.Pointer(var1)).rel = (*Relocation)(unsafe.Pointer(bp /* &head */)).next
}

// Returns true if a given token represents a type.
func is_typename(tls *libc.TLS, tok uintptr) uint8 { /* parse.c:1498:13: */

	if map1.capacity == 0 {

		{
			var i int32 = 0
			for ; uint64(i) < uint64(unsafe.Sizeof(kw))/uint64(unsafe.Sizeof(uintptr(0))); i++ {
				hashmap_put(tls, uintptr(unsafe.Pointer(&map1)), kw[i], uintptr(1))
			}
		}
	}

	return uint8(libc.Bool32(hashmap_get2(tls, uintptr(unsafe.Pointer(&map1)), (*Token)(unsafe.Pointer(tok)).loc, (*Token)(unsafe.Pointer(tok)).len) != 0 || find_typedef(tls, tok) != 0))
}

var map1 HashMap /* parse.c:1499:18: */
var kw = [30]uintptr{
	ts + 8744, ts + 8749, ts + 8755, ts + 8760, ts + 8766, ts + 8770, ts + 8719, ts + 8726,
	ts + 8391, ts + 8732, ts + 8399, ts + 8406, ts + 8670, ts + 8788, ts + 8795,
	ts + 8586, ts + 8592, ts + 8601, ts + 8606, ts + 8615, ts + 8624,
	ts + 8635, ts + 8648, ts + 8775, ts + 8781, ts + 8737, ts + 8413,
	ts + 8420, ts + 8434, ts + 8658,
} /* parse.c:1502:17 */

// asm-stmt = "asm" ("volatile" | "inline")* "(" string-literal ")"
func asm_stmt(tls *libc.TLS, rest uintptr, tok uintptr) uintptr { /* parse.c:1518:13: */
	var node uintptr = new_node(tls, ND_ASM, tok)
	tok = (*Token)(unsafe.Pointer(tok)).next

	for equal(tls, tok, ts+8592) != 0 || equal(tls, tok, ts+8413) != 0 {
		tok = (*Token)(unsafe.Pointer(tok)).next
	}

	tok = skip(tls, tok, ts+8666)
	if (*Token)(unsafe.Pointer(tok)).kind != TK_STR || (*Type)(unsafe.Pointer((*Type)(unsafe.Pointer((*Token)(unsafe.Pointer(tok)).ty)).base)).kind != TY_CHAR {
		error_tok(tls, tok, ts+9219, 0)
	}
	(*Node)(unsafe.Pointer(node)).asm_str = (*Token)(unsafe.Pointer(tok)).str
	*(*uintptr)(unsafe.Pointer(rest)) = skip(tls, (*Token)(unsafe.Pointer(tok)).next, ts+8668)
	return node
}

// stmt = "return" expr? ";"
//
//	| "if" "(" expr ")" stmt ("else" stmt)?
//	| "switch" "(" expr ")" stmt
//	| "case" const-expr ("..." const-expr)? ":" stmt
//	| "default" ":" stmt
//	| "for" "(" expr-stmt expr? ";" expr? ")" stmt
//	| "while" "(" expr ")" stmt
//	| "do" stmt "while" "(" expr ")" ";"
//	| "asm" asm-stmt
//	| "goto" (ident | "*" expr) ";"
//	| "break" ";"
//	| "continue" ";"
//	| ident ":" stmt
//	| "{" compound-stmt
//	| expr-stmt
func stmt(tls *libc.TLS, rest uintptr, tok uintptr) uintptr { /* parse.c:1548:13: */
	bp := tls.Alloc(8)
	defer tls.Free(8)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9243) != 0 {
		var node uintptr = new_node(tls, ND_RETURN, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		if consume(tls, rest, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp)))).next, ts+8876) != 0 {
			return node
		}

		var exp uintptr = expr(tls, bp, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next)
		*(*uintptr)(unsafe.Pointer(rest)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8876)

		add_type(tls, exp)
		var ty uintptr = (*Type)(unsafe.Pointer((*Obj)(unsafe.Pointer(current_fn1)).ty)).return_ty
		if (*Type)(unsafe.Pointer(ty)).kind != TY_STRUCT && (*Type)(unsafe.Pointer(ty)).kind != TY_UNION {
			exp = new_cast(tls, exp, (*Type)(unsafe.Pointer((*Obj)(unsafe.Pointer(current_fn1)).ty)).return_ty)
		}

		(*Node)(unsafe.Pointer(node)).lhs = exp
		return node
	}

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9250) != 0 {
		var node uintptr = new_node(tls, ND_IF, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next, ts+8666)
		(*Node)(unsafe.Pointer(node)).cond = expr(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8668)
		(*Node)(unsafe.Pointer(node)).then = stmt(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9253) != 0 {
			(*Node)(unsafe.Pointer(node)).els = stmt(tls, bp, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next)
		}
		*(*uintptr)(unsafe.Pointer(rest)) = *(*uintptr)(unsafe.Pointer(bp /* tok */))
		return node
	}

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9258) != 0 {
		var node uintptr = new_node(tls, ND_SWITCH, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next, ts+8666)
		(*Node)(unsafe.Pointer(node)).cond = expr(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8668)

		var sw uintptr = current_switch
		current_switch = node

		var brk uintptr = brk_label
		brk_label = libc.AssignPtrUintptr(node+88, new_unique_name(tls))

		(*Node)(unsafe.Pointer(node)).then = stmt(tls, rest, *(*uintptr)(unsafe.Pointer(bp /* tok */)))

		current_switch = sw
		brk_label = brk
		return node
	}

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9265) != 0 {
		if !(current_switch != 0) {
			error_tok(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+9270, 0)
		}

		var node uintptr = new_node(tls, ND_CASE, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		var begin int32 = int32(const_expr(tls, bp, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next))
		var end int32

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8825) != 0 {
			// [GNU] Case ranges, e.g. "case 1 ... 5:"
			end = int32(const_expr(tls, bp, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next))
			if end < begin {
				error_tok(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+9281, 0)
			}
		} else {
			end = begin
		}

		*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+9308)
		(*Node)(unsafe.Pointer(node)).label = new_unique_name(tls)
		(*Node)(unsafe.Pointer(node)).lhs = stmt(tls, rest, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		(*Node)(unsafe.Pointer(node)).begin = int64(begin)
		(*Node)(unsafe.Pointer(node)).end = int64(end)
		(*Node)(unsafe.Pointer(node)).case_next = (*Node)(unsafe.Pointer(current_switch)).case_next
		(*Node)(unsafe.Pointer(current_switch)).case_next = node
		return node
	}

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9310) != 0 {
		if !(current_switch != 0) {
			error_tok(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+9318, 0)
		}

		var node uintptr = new_node(tls, ND_CASE, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next, ts+9308)
		(*Node)(unsafe.Pointer(node)).label = new_unique_name(tls)
		(*Node)(unsafe.Pointer(node)).lhs = stmt(tls, rest, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		(*Node)(unsafe.Pointer(current_switch)).default_case = node
		return node
	}

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9332) != 0 {
		var node uintptr = new_node(tls, ND_FOR, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next, ts+8666)

		enter_scope(tls)

		var brk uintptr = brk_label
		var cont uintptr = cont_label
		brk_label = libc.AssignPtrUintptr(node+88, new_unique_name(tls))
		cont_label = libc.AssignPtrUintptr(node+96, new_unique_name(tls))

		if is_typename(tls, *(*uintptr)(unsafe.Pointer(bp))) != 0 {
			var basety uintptr = declspec(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)), uintptr(0))
			(*Node)(unsafe.Pointer(node)).init = declaration(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)), basety, uintptr(0))
		} else {
			(*Node)(unsafe.Pointer(node)).init = expr_stmt(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		}

		if !(equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8876) != 0) {
			(*Node)(unsafe.Pointer(node)).cond = expr(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		}
		*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8876)

		if !(equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8668) != 0) {
			(*Node)(unsafe.Pointer(node)).inc = expr(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		}
		*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8668)

		(*Node)(unsafe.Pointer(node)).then = stmt(tls, rest, *(*uintptr)(unsafe.Pointer(bp /* tok */)))

		leave_scope(tls)
		brk_label = brk
		cont_label = cont
		return node
	}

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9336) != 0 {
		var node uintptr = new_node(tls, ND_FOR, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next, ts+8666)
		(*Node)(unsafe.Pointer(node)).cond = expr(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8668)

		var brk uintptr = brk_label
		var cont uintptr = cont_label
		brk_label = libc.AssignPtrUintptr(node+88, new_unique_name(tls))
		cont_label = libc.AssignPtrUintptr(node+96, new_unique_name(tls))

		(*Node)(unsafe.Pointer(node)).then = stmt(tls, rest, *(*uintptr)(unsafe.Pointer(bp /* tok */)))

		brk_label = brk
		cont_label = cont
		return node
	}

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9342) != 0 {
		var node uintptr = new_node(tls, ND_DO, *(*uintptr)(unsafe.Pointer(bp /* tok */)))

		var brk uintptr = brk_label
		var cont uintptr = cont_label
		brk_label = libc.AssignPtrUintptr(node+88, new_unique_name(tls))
		cont_label = libc.AssignPtrUintptr(node+96, new_unique_name(tls))

		(*Node)(unsafe.Pointer(node)).then = stmt(tls, bp, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next)

		brk_label = brk
		cont_label = cont

		*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+9336)
		*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8666)
		(*Node)(unsafe.Pointer(node)).cond = expr(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8668)
		*(*uintptr)(unsafe.Pointer(rest)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8876)
		return node
	}

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9345) != 0 {
		return asm_stmt(tls, rest, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
	}

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9349) != 0 {
		if equal(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp)))).next, ts+8833) != 0 {
			// [GNU] `goto *ptr` jumps to the address specified by `ptr`.
			var node uintptr = new_node(tls, ND_GOTO_EXPR, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
			(*Node)(unsafe.Pointer(node)).lhs = expr(tls, bp, (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next)).next)
			*(*uintptr)(unsafe.Pointer(rest)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8876)
			return node
		}

		var node uintptr = new_node(tls, ND_GOTO, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		(*Node)(unsafe.Pointer(node)).label = get_ident(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next)
		(*Node)(unsafe.Pointer(node)).goto_next = gotos
		gotos = node
		*(*uintptr)(unsafe.Pointer(rest)) = skip(tls, (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next)).next, ts+8876)
		return node
	}

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9354) != 0 {
		if !(brk_label != 0) {
			error_tok(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+9360, 0)
		}
		var node uintptr = new_node(tls, ND_GOTO, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		(*Node)(unsafe.Pointer(node)).unique_label = brk_label
		*(*uintptr)(unsafe.Pointer(rest)) = skip(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next, ts+8876)
		return node
	}

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9372) != 0 {
		if !(cont_label != 0) {
			error_tok(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+9381, 0)
		}
		var node uintptr = new_node(tls, ND_GOTO, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		(*Node)(unsafe.Pointer(node)).unique_label = cont_label
		*(*uintptr)(unsafe.Pointer(rest)) = skip(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next, ts+8876)
		return node
	}

	if (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp)))).kind == TK_IDENT && equal(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp)))).next, ts+9308) != 0 {
		var node uintptr = new_node(tls, ND_LABEL, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		(*Node)(unsafe.Pointer(node)).label = xstrndup(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).loc, uint64((*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).len))
		(*Node)(unsafe.Pointer(node)).unique_label = new_unique_name(tls)
		(*Node)(unsafe.Pointer(node)).lhs = stmt(tls, rest, (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next)).next)
		(*Node)(unsafe.Pointer(node)).goto_next = labels
		labels = node
		return node
	}

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8837) != 0 {
		return compound_stmt(tls, rest, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next)
	}

	return expr_stmt(tls, rest, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
}

// compound-stmt = (typedef | declaration | stmt)* "}"
func compound_stmt(tls *libc.TLS, rest uintptr, tok uintptr) uintptr { /* parse.c:1764:13: */
	bp := tls.Alloc(300)
	defer tls.Free(300)
	*(*uintptr)(unsafe.Pointer(bp + 280)) = tok

	var node uintptr = new_node(tls, ND_BLOCK, *(*uintptr)(unsafe.Pointer(bp + 280 /* tok */)))
	*(*Node)(unsafe.Pointer(bp /* head */)) = Node{}
	var cur uintptr = bp /* &head */

	enter_scope(tls)

	for !(equal(tls, *(*uintptr)(unsafe.Pointer(bp + 280)), ts+8835) != 0) {
		if is_typename(tls, *(*uintptr)(unsafe.Pointer(bp + 280))) != 0 && !(equal(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 280)))).next, ts+9308) != 0) {
			*(*VarAttr)(unsafe.Pointer(bp + 288 /* attr */)) = VarAttr{}
			var basety uintptr = declspec(tls, bp+280, *(*uintptr)(unsafe.Pointer(bp + 280 /* tok */)), bp+288)

			if (*VarAttr)(unsafe.Pointer(bp+288)).is_typedef != 0 {
				*(*uintptr)(unsafe.Pointer(bp + 280 /* tok */)) = parse_typedef(tls, *(*uintptr)(unsafe.Pointer(bp + 280 /* tok */)), basety)
				continue
			}

			if is_function(tls, *(*uintptr)(unsafe.Pointer(bp + 280))) != 0 {
				*(*uintptr)(unsafe.Pointer(bp + 280 /* tok */)) = function(tls, *(*uintptr)(unsafe.Pointer(bp + 280 /* tok */)), basety, bp+288)
				continue
			}

			if (*VarAttr)(unsafe.Pointer(bp+288)).is_extern != 0 {
				*(*uintptr)(unsafe.Pointer(bp + 280 /* tok */)) = global_variable(tls, *(*uintptr)(unsafe.Pointer(bp + 280 /* tok */)), basety, bp+288)
				continue
			}

			cur = libc.AssignPtrUintptr(cur+8, declaration(tls, bp+280, *(*uintptr)(unsafe.Pointer(bp + 280 /* tok */)), basety, bp+288))
		} else {
			cur = libc.AssignPtrUintptr(cur+8, stmt(tls, bp+280, *(*uintptr)(unsafe.Pointer(bp + 280 /* tok */))))
		}
		add_type(tls, cur)
	}

	leave_scope(tls)

	(*Node)(unsafe.Pointer(node)).body = (*Node)(unsafe.Pointer(bp /* &head */)).next
	*(*uintptr)(unsafe.Pointer(rest)) = (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 280 /* tok */)))).next
	return node
}

// expr-stmt = expr? ";"
func expr_stmt(tls *libc.TLS, rest uintptr, tok uintptr) uintptr { /* parse.c:1806:13: */
	bp := tls.Alloc(8)
	defer tls.Free(8)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8876) != 0 {
		*(*uintptr)(unsafe.Pointer(rest)) = (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next
		return new_node(tls, ND_BLOCK, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
	}

	var node uintptr = new_node(tls, ND_EXPR_STMT, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
	(*Node)(unsafe.Pointer(node)).lhs = expr(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
	*(*uintptr)(unsafe.Pointer(rest)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8876)
	return node
}

// expr = assign ("," expr)?
func expr(tls *libc.TLS, rest uintptr, tok uintptr) uintptr { /* parse.c:1819:13: */
	bp := tls.Alloc(8)
	defer tls.Free(8)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	var node uintptr = assign(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)))

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8331) != 0 {
		return new_binary(tls, ND_COMMA, node, expr(tls, rest, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next), *(*uintptr)(unsafe.Pointer(bp /* tok */)))
	}

	*(*uintptr)(unsafe.Pointer(rest)) = *(*uintptr)(unsafe.Pointer(bp /* tok */))
	return node
}

func eval(tls *libc.TLS, node uintptr) int64_t { /* parse.c:1829:16: */
	return eval2(tls, node, uintptr(0))
}

// Evaluate a given node as a constant expression.
//
// A constant expression is either just a number or ptr+n where ptr
// is a pointer to a global variable and n is a postiive/negative
// number. The latter form is accepted only as an initialization
// expression for a global variable.
func eval2(tls *libc.TLS, node uintptr, label uintptr) int64_t { /* parse.c:1839:16: */
	add_type(tls, node)

	if is_flonum(tls, (*Node)(unsafe.Pointer(node)).ty) != 0 {
		return libc.Int64FromFloat64(eval_double(tls, node))
	}

	switch (*Node)(unsafe.Pointer(node)).kind {
	case ND_ADD:
		return eval2(tls, (*Node)(unsafe.Pointer(node)).lhs, label) + eval(tls, (*Node)(unsafe.Pointer(node)).rhs)
	case ND_SUB:
		return eval2(tls, (*Node)(unsafe.Pointer(node)).lhs, label) - eval(tls, (*Node)(unsafe.Pointer(node)).rhs)
	case ND_MUL:
		return eval(tls, (*Node)(unsafe.Pointer(node)).lhs) * eval(tls, (*Node)(unsafe.Pointer(node)).rhs)
	case ND_DIV:
		if (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).ty)).is_unsigned != 0 {
			return int64_t(uint64_t(eval(tls, (*Node)(unsafe.Pointer(node)).lhs)) / uint64_t(eval(tls, (*Node)(unsafe.Pointer(node)).rhs)))
		}
		return eval(tls, (*Node)(unsafe.Pointer(node)).lhs) / eval(tls, (*Node)(unsafe.Pointer(node)).rhs)
	case ND_NEG:
		return -eval(tls, (*Node)(unsafe.Pointer(node)).lhs)
	case ND_MOD:
		if (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).ty)).is_unsigned != 0 {
			return int64_t(uint64_t(eval(tls, (*Node)(unsafe.Pointer(node)).lhs)) % uint64_t(eval(tls, (*Node)(unsafe.Pointer(node)).rhs)))
		}
		return eval(tls, (*Node)(unsafe.Pointer(node)).lhs) % eval(tls, (*Node)(unsafe.Pointer(node)).rhs)
	case ND_BITAND:
		return eval(tls, (*Node)(unsafe.Pointer(node)).lhs) & eval(tls, (*Node)(unsafe.Pointer(node)).rhs)
	case ND_BITOR:
		return eval(tls, (*Node)(unsafe.Pointer(node)).lhs) | eval(tls, (*Node)(unsafe.Pointer(node)).rhs)
	case ND_BITXOR:
		return eval(tls, (*Node)(unsafe.Pointer(node)).lhs) ^ eval(tls, (*Node)(unsafe.Pointer(node)).rhs)
	case ND_SHL:
		return eval(tls, (*Node)(unsafe.Pointer(node)).lhs) << eval(tls, (*Node)(unsafe.Pointer(node)).rhs)
	case ND_SHR:
		if (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).ty)).is_unsigned != 0 && (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).ty)).size == 8 {
			return int64_t(uint64_t(eval(tls, (*Node)(unsafe.Pointer(node)).lhs)) >> eval(tls, (*Node)(unsafe.Pointer(node)).rhs))
		}
		return eval(tls, (*Node)(unsafe.Pointer(node)).lhs) >> eval(tls, (*Node)(unsafe.Pointer(node)).rhs)
	case ND_EQ:
		return int64_t(libc.Bool32(eval(tls, (*Node)(unsafe.Pointer(node)).lhs) == eval(tls, (*Node)(unsafe.Pointer(node)).rhs)))
	case ND_NE:
		return int64_t(libc.Bool32(eval(tls, (*Node)(unsafe.Pointer(node)).lhs) != eval(tls, (*Node)(unsafe.Pointer(node)).rhs)))
	case ND_LT:
		if (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).lhs)).ty)).is_unsigned != 0 {
			return int64_t(libc.Bool32(uint64_t(eval(tls, (*Node)(unsafe.Pointer(node)).lhs)) < uint64_t(eval(tls, (*Node)(unsafe.Pointer(node)).rhs))))
		}
		return int64_t(libc.Bool32(eval(tls, (*Node)(unsafe.Pointer(node)).lhs) < eval(tls, (*Node)(unsafe.Pointer(node)).rhs)))
	case ND_LE:
		if (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).lhs)).ty)).is_unsigned != 0 {
			return int64_t(libc.Bool32(uint64_t(eval(tls, (*Node)(unsafe.Pointer(node)).lhs)) <= uint64_t(eval(tls, (*Node)(unsafe.Pointer(node)).rhs))))
		}
		return int64_t(libc.Bool32(eval(tls, (*Node)(unsafe.Pointer(node)).lhs) <= eval(tls, (*Node)(unsafe.Pointer(node)).rhs)))
	case ND_COND:
		if eval(tls, (*Node)(unsafe.Pointer(node)).cond) != 0 {
			return eval2(tls, (*Node)(unsafe.Pointer(node)).then, label)
		}
		return eval2(tls, (*Node)(unsafe.Pointer(node)).els, label)
	case ND_COMMA:
		return eval2(tls, (*Node)(unsafe.Pointer(node)).rhs, label)
	case ND_NOT:
		return libc.BoolInt64(!(eval(tls, (*Node)(unsafe.Pointer(node)).lhs) != 0))
	case ND_BITNOT:
		return ^eval(tls, (*Node)(unsafe.Pointer(node)).lhs)
	case ND_LOGAND:
		return int64_t(libc.Bool32(eval(tls, (*Node)(unsafe.Pointer(node)).lhs) != 0 && eval(tls, (*Node)(unsafe.Pointer(node)).rhs) != 0))
	case ND_LOGOR:
		return int64_t(libc.Bool32(eval(tls, (*Node)(unsafe.Pointer(node)).lhs) != 0 || eval(tls, (*Node)(unsafe.Pointer(node)).rhs) != 0))
	case ND_CAST:
		{
			var val int64_t = eval2(tls, (*Node)(unsafe.Pointer(node)).lhs, label)
			if is_integer(tls, (*Node)(unsafe.Pointer(node)).ty) != 0 {
				switch (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).ty)).size {
				case 1:
					if (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).ty)).is_unsigned != 0 {
						return int64(uint8_t(val))
					}
					return int64(int8_t(val))
					fallthrough
				case 2:
					if (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).ty)).is_unsigned != 0 {
						return int64(uint16_t(val))
					}
					return int64(int16_t(val))
					fallthrough
				case 4:
					if (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).ty)).is_unsigned != 0 {
						return int64(uint32_t(val))
					}
					return int64(int32_t(val))
				}
			}
			return val

		}
	case ND_ADDR:
		return eval_rval(tls, (*Node)(unsafe.Pointer(node)).lhs, label)
	case ND_LABEL_VAL:
		*(*uintptr)(unsafe.Pointer(label)) = node + 160
		return int64(0)
	case ND_MEMBER:
		if !(label != 0) {
			error_tok(tls, (*Node)(unsafe.Pointer(node)).tok, ts+9396, 0)
		}
		if (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).ty)).kind != TY_ARRAY {
			error_tok(tls, (*Node)(unsafe.Pointer(node)).tok, ts+9424, 0)
		}
		return eval_rval(tls, (*Node)(unsafe.Pointer(node)).lhs, label) + int64_t((*Member)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).member)).offset)
	case ND_VAR:
		if !(label != 0) {
			error_tok(tls, (*Node)(unsafe.Pointer(node)).tok, ts+9396, 0)
		}
		if (*Type)(unsafe.Pointer((*Obj)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).__var)).ty)).kind != TY_ARRAY && (*Type)(unsafe.Pointer((*Obj)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).__var)).ty)).kind != TY_FUNC {
			error_tok(tls, (*Node)(unsafe.Pointer(node)).tok, ts+9424, 0)
		}
		*(*uintptr)(unsafe.Pointer(label)) = (*Node)(unsafe.Pointer(node)).__var + 8
		return int64(0)
	case ND_NUM:
		return (*Node)(unsafe.Pointer(node)).val
	}

	error_tok(tls, (*Node)(unsafe.Pointer(node)).tok, ts+9396, 0)
	return int64_t(0)
}

func eval_rval(tls *libc.TLS, node uintptr, label uintptr) int64_t { /* parse.c:1934:16: */
	switch (*Node)(unsafe.Pointer(node)).kind {
	case ND_VAR:
		if (*Obj)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).__var)).is_local != 0 {
			error_tok(tls, (*Node)(unsafe.Pointer(node)).tok, ts+9396, 0)
		}
		*(*uintptr)(unsafe.Pointer(label)) = (*Node)(unsafe.Pointer(node)).__var + 8
		return int64(0)
	case ND_DEREF:
		return eval2(tls, (*Node)(unsafe.Pointer(node)).lhs, label)
	case ND_MEMBER:
		return eval_rval(tls, (*Node)(unsafe.Pointer(node)).lhs, label) + int64_t((*Member)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).member)).offset)
	}

	error_tok(tls, (*Node)(unsafe.Pointer(node)).tok, ts+9424, 0)
	return int64_t(0)
}

func is_const_expr(tls *libc.TLS, node uintptr) uint8 { /* parse.c:1950:13: */
	add_type(tls, node)

	switch (*Node)(unsafe.Pointer(node)).kind {
	case ND_ADD:
		fallthrough
	case ND_SUB:
		fallthrough
	case ND_MUL:
		fallthrough
	case ND_DIV:
		fallthrough
	case ND_BITAND:
		fallthrough
	case ND_BITOR:
		fallthrough
	case ND_BITXOR:
		fallthrough
	case ND_SHL:
		fallthrough
	case ND_SHR:
		fallthrough
	case ND_EQ:
		fallthrough
	case ND_NE:
		fallthrough
	case ND_LT:
		fallthrough
	case ND_LE:
		fallthrough
	case ND_LOGAND:
		fallthrough
	case ND_LOGOR:
		return uint8(libc.Bool32(is_const_expr(tls, (*Node)(unsafe.Pointer(node)).lhs) != 0 && is_const_expr(tls, (*Node)(unsafe.Pointer(node)).rhs) != 0))
	case ND_COND:
		if !(is_const_expr(tls, (*Node)(unsafe.Pointer(node)).cond) != 0) {
			return uint8(0)
		}
		return is_const_expr(tls, func() uintptr {
			if eval(tls, (*Node)(unsafe.Pointer(node)).cond) != 0 {
				return (*Node)(unsafe.Pointer(node)).then
			}
			return (*Node)(unsafe.Pointer(node)).els
		}())
	case ND_COMMA:
		return is_const_expr(tls, (*Node)(unsafe.Pointer(node)).rhs)
	case ND_NEG:
		fallthrough
	case ND_NOT:
		fallthrough
	case ND_BITNOT:
		fallthrough
	case ND_CAST:
		return is_const_expr(tls, (*Node)(unsafe.Pointer(node)).lhs)
	case ND_NUM:
		return uint8(1)
	}

	return uint8(0)
}

func const_expr(tls *libc.TLS, rest uintptr, tok uintptr) int64_t { /* parse.c:1988:9: */
	var node uintptr = conditional(tls, rest, tok)
	return eval(tls, node)
}

func eval_double(tls *libc.TLS, node uintptr) float64 { /* parse.c:1993:15: */
	add_type(tls, node)

	if is_integer(tls, (*Node)(unsafe.Pointer(node)).ty) != 0 {
		if (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).ty)).is_unsigned != 0 {
			return float64(uint64(eval(tls, node)))
		}
		return float64(eval(tls, node))
	}

	switch (*Node)(unsafe.Pointer(node)).kind {
	case ND_ADD:
		return eval_double(tls, (*Node)(unsafe.Pointer(node)).lhs) + eval_double(tls, (*Node)(unsafe.Pointer(node)).rhs)
	case ND_SUB:
		return eval_double(tls, (*Node)(unsafe.Pointer(node)).lhs) - eval_double(tls, (*Node)(unsafe.Pointer(node)).rhs)
	case ND_MUL:
		return eval_double(tls, (*Node)(unsafe.Pointer(node)).lhs) * eval_double(tls, (*Node)(unsafe.Pointer(node)).rhs)
	case ND_DIV:
		return eval_double(tls, (*Node)(unsafe.Pointer(node)).lhs) / eval_double(tls, (*Node)(unsafe.Pointer(node)).rhs)
	case ND_NEG:
		return -eval_double(tls, (*Node)(unsafe.Pointer(node)).lhs)
	case ND_COND:
		if eval_double(tls, (*Node)(unsafe.Pointer(node)).cond) != 0 {
			return eval_double(tls, (*Node)(unsafe.Pointer(node)).then)
		}
		return eval_double(tls, (*Node)(unsafe.Pointer(node)).els)
	case ND_COMMA:
		return eval_double(tls, (*Node)(unsafe.Pointer(node)).rhs)
	case ND_CAST:
		if is_flonum(tls, (*Node)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).lhs)).ty) != 0 {
			return eval_double(tls, (*Node)(unsafe.Pointer(node)).lhs)
		}
		return float64(eval(tls, (*Node)(unsafe.Pointer(node)).lhs))
	case ND_NUM:
		return (*Node)(unsafe.Pointer(node)).fval
	}

	error_tok(tls, (*Node)(unsafe.Pointer(node)).tok, ts+9396, 0)
	return float64(0)
}

// Convert op= operators to expressions containing an assignment.
//
// In general, `A op= C` is converted to “tmp = &A, *tmp = *tmp op B`.
// However, if a given expression is of form `A.x op= C`, the input is
// converted to `tmp = &A, (*tmp).x = (*tmp).x op C` to handle assignments
// to bitfields.
func to_assign(tls *libc.TLS, binary uintptr) uintptr { /* parse.c:2034:13: */
	bp := tls.Alloc(280)
	defer tls.Free(280)

	add_type(tls, (*Node)(unsafe.Pointer(binary)).lhs)
	add_type(tls, (*Node)(unsafe.Pointer(binary)).rhs)
	var tok uintptr = (*Node)(unsafe.Pointer(binary)).tok

	// Convert `A.x op= C` to `tmp = &A, (*tmp).x = (*tmp).x op C`.
	if (*Node)(unsafe.Pointer((*Node)(unsafe.Pointer(binary)).lhs)).kind == ND_MEMBER {
		var var1 uintptr = new_lvar(tls, ts+8875, pointer_to(tls, (*Node)(unsafe.Pointer((*Node)(unsafe.Pointer((*Node)(unsafe.Pointer(binary)).lhs)).lhs)).ty))

		var expr1 uintptr = new_binary(tls, ND_ASSIGN, new_var_node(tls, var1, tok),
			new_unary(tls, ND_ADDR, (*Node)(unsafe.Pointer((*Node)(unsafe.Pointer(binary)).lhs)).lhs, tok), tok)

		var expr2 uintptr = new_unary(tls, ND_MEMBER,
			new_unary(tls, ND_DEREF, new_var_node(tls, var1, tok), tok),
			tok)
		(*Node)(unsafe.Pointer(expr2)).member = (*Node)(unsafe.Pointer((*Node)(unsafe.Pointer(binary)).lhs)).member

		var expr3 uintptr = new_unary(tls, ND_MEMBER,
			new_unary(tls, ND_DEREF, new_var_node(tls, var1, tok), tok),
			tok)
		(*Node)(unsafe.Pointer(expr3)).member = (*Node)(unsafe.Pointer((*Node)(unsafe.Pointer(binary)).lhs)).member

		var expr4 uintptr = new_binary(tls, ND_ASSIGN, expr2,
			new_binary(tls, (*Node)(unsafe.Pointer(binary)).kind, expr3, (*Node)(unsafe.Pointer(binary)).rhs, tok),
			tok)

		return new_binary(tls, ND_COMMA, expr1, expr4, tok)
	}

	// If A is an atomic type, Convert `A op= B` to
	//
	// ({
	//   T1 *addr = &A; T2 val = (B); T1 old = *addr; T1 new;
	//   do {
	//    new = old op val;
	//   } while (!atomic_compare_exchange_strong(addr, &old, new));
	//   new;
	// })
	if (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer((*Node)(unsafe.Pointer(binary)).lhs)).ty)).is_atomic != 0 {
		*(*Node)(unsafe.Pointer(bp /* head */)) = Node{}
		var cur uintptr = bp /* &head */

		var addr uintptr = new_lvar(tls, ts+8875, pointer_to(tls, (*Node)(unsafe.Pointer((*Node)(unsafe.Pointer(binary)).lhs)).ty))
		var val uintptr = new_lvar(tls, ts+8875, (*Node)(unsafe.Pointer((*Node)(unsafe.Pointer(binary)).rhs)).ty)
		var old uintptr = new_lvar(tls, ts+8875, (*Node)(unsafe.Pointer((*Node)(unsafe.Pointer(binary)).lhs)).ty)
		var new uintptr = new_lvar(tls, ts+8875, (*Node)(unsafe.Pointer((*Node)(unsafe.Pointer(binary)).lhs)).ty)

		cur = libc.AssignPtrUintptr(cur+8, new_unary(tls, ND_EXPR_STMT,
			new_binary(tls, ND_ASSIGN, new_var_node(tls, addr, tok),
				new_unary(tls, ND_ADDR, (*Node)(unsafe.Pointer(binary)).lhs, tok), tok),
			tok))

		cur = libc.AssignPtrUintptr(cur+8, new_unary(tls, ND_EXPR_STMT,
			new_binary(tls, ND_ASSIGN, new_var_node(tls, val, tok), (*Node)(unsafe.Pointer(binary)).rhs, tok),
			tok))

		cur = libc.AssignPtrUintptr(cur+8, new_unary(tls, ND_EXPR_STMT,
			new_binary(tls, ND_ASSIGN, new_var_node(tls, old, tok),
				new_unary(tls, ND_DEREF, new_var_node(tls, addr, tok), tok), tok),
			tok))

		var loop uintptr = new_node(tls, ND_DO, tok)
		(*Node)(unsafe.Pointer(loop)).brk_label = new_unique_name(tls)
		(*Node)(unsafe.Pointer(loop)).cont_label = new_unique_name(tls)

		var body uintptr = new_binary(tls, ND_ASSIGN,
			new_var_node(tls, new, tok),
			new_binary(tls, (*Node)(unsafe.Pointer(binary)).kind, new_var_node(tls, old, tok),
				new_var_node(tls, val, tok), tok),
			tok)

		(*Node)(unsafe.Pointer(loop)).then = new_node(tls, ND_BLOCK, tok)
		(*Node)(unsafe.Pointer((*Node)(unsafe.Pointer(loop)).then)).body = new_unary(tls, ND_EXPR_STMT, body, tok)

		var cas uintptr = new_node(tls, ND_CAS, tok)
		(*Node)(unsafe.Pointer(cas)).cas_addr = new_var_node(tls, addr, tok)
		(*Node)(unsafe.Pointer(cas)).cas_old = new_unary(tls, ND_ADDR, new_var_node(tls, old, tok), tok)
		(*Node)(unsafe.Pointer(cas)).cas_new = new_var_node(tls, new, tok)
		(*Node)(unsafe.Pointer(loop)).cond = new_unary(tls, ND_NOT, cas, tok)

		cur = libc.AssignPtrUintptr(cur+8, loop)
		cur = libc.AssignPtrUintptr(cur+8, new_unary(tls, ND_EXPR_STMT, new_var_node(tls, new, tok), tok))

		var node uintptr = new_node(tls, ND_STMT_EXPR, tok)
		(*Node)(unsafe.Pointer(node)).body = (*Node)(unsafe.Pointer(bp /* &head */)).next
		return node
	}

	// Convert `A op= B` to ``tmp = &A, *tmp = *tmp op B`.
	var var1 uintptr = new_lvar(tls, ts+8875, pointer_to(tls, (*Node)(unsafe.Pointer((*Node)(unsafe.Pointer(binary)).lhs)).ty))

	var expr1 uintptr = new_binary(tls, ND_ASSIGN, new_var_node(tls, var1, tok),
		new_unary(tls, ND_ADDR, (*Node)(unsafe.Pointer(binary)).lhs, tok), tok)

	var expr2 uintptr = new_binary(tls, ND_ASSIGN,
		new_unary(tls, ND_DEREF, new_var_node(tls, var1, tok), tok),
		new_binary(tls, (*Node)(unsafe.Pointer(binary)).kind,
			new_unary(tls, ND_DEREF, new_var_node(tls, var1, tok), tok),
			(*Node)(unsafe.Pointer(binary)).rhs,
			tok),
		tok)

	return new_binary(tls, ND_COMMA, expr1, expr2, tok)
}

// assign    = conditional (assign-op assign)?
// assign-op = "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^="
//
//	| "<<=" | ">>="
func assign(tls *libc.TLS, rest uintptr, tok uintptr) uintptr { /* parse.c:2146:13: */
	bp := tls.Alloc(8)
	defer tls.Free(8)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	var node uintptr = conditional(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)))

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8873) != 0 {
		return new_binary(tls, ND_ASSIGN, node, assign(tls, rest, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next), *(*uintptr)(unsafe.Pointer(bp /* tok */)))
	}

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9444) != 0 {
		return to_assign(tls, new_add(tls, node, assign(tls, rest, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next), *(*uintptr)(unsafe.Pointer(bp /* tok */))))
	}

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9447) != 0 {
		return to_assign(tls, new_sub(tls, node, assign(tls, rest, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next), *(*uintptr)(unsafe.Pointer(bp /* tok */))))
	}

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9450) != 0 {
		return to_assign(tls, new_binary(tls, ND_MUL, node, assign(tls, rest, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next), *(*uintptr)(unsafe.Pointer(bp /* tok */))))
	}

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9453) != 0 {
		return to_assign(tls, new_binary(tls, ND_DIV, node, assign(tls, rest, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next), *(*uintptr)(unsafe.Pointer(bp /* tok */))))
	}

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9456) != 0 {
		return to_assign(tls, new_binary(tls, ND_MOD, node, assign(tls, rest, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next), *(*uintptr)(unsafe.Pointer(bp /* tok */))))
	}

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9459) != 0 {
		return to_assign(tls, new_binary(tls, ND_BITAND, node, assign(tls, rest, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next), *(*uintptr)(unsafe.Pointer(bp /* tok */))))
	}

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9462) != 0 {
		return to_assign(tls, new_binary(tls, ND_BITOR, node, assign(tls, rest, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next), *(*uintptr)(unsafe.Pointer(bp /* tok */))))
	}

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9465) != 0 {
		return to_assign(tls, new_binary(tls, ND_BITXOR, node, assign(tls, rest, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next), *(*uintptr)(unsafe.Pointer(bp /* tok */))))
	}

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9468) != 0 {
		return to_assign(tls, new_binary(tls, ND_SHL, node, assign(tls, rest, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next), *(*uintptr)(unsafe.Pointer(bp /* tok */))))
	}

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9472) != 0 {
		return to_assign(tls, new_binary(tls, ND_SHR, node, assign(tls, rest, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next), *(*uintptr)(unsafe.Pointer(bp /* tok */))))
	}

	*(*uintptr)(unsafe.Pointer(rest)) = *(*uintptr)(unsafe.Pointer(bp /* tok */))
	return node
}

// conditional = logor ("?" expr? ":" conditional)?
func conditional(tls *libc.TLS, rest uintptr, tok uintptr) uintptr { /* parse.c:2187:13: */
	bp := tls.Alloc(8)
	defer tls.Free(8)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	var cond uintptr = logor(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)))

	if !(equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9476) != 0) {
		*(*uintptr)(unsafe.Pointer(rest)) = *(*uintptr)(unsafe.Pointer(bp /* tok */))
		return cond
	}

	if equal(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp)))).next, ts+9308) != 0 {
		// [GNU] Compile `a ?: b` as `tmp = a, tmp ? tmp : b`.
		add_type(tls, cond)
		var var1 uintptr = new_lvar(tls, ts+8875, (*Node)(unsafe.Pointer(cond)).ty)
		var lhs uintptr = new_binary(tls, ND_ASSIGN, new_var_node(tls, var1, *(*uintptr)(unsafe.Pointer(bp /* tok */))), cond, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		var rhs uintptr = new_node(tls, ND_COND, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		(*Node)(unsafe.Pointer(rhs)).cond = new_var_node(tls, var1, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		(*Node)(unsafe.Pointer(rhs)).then = new_var_node(tls, var1, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		(*Node)(unsafe.Pointer(rhs)).els = conditional(tls, rest, (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next)).next)
		return new_binary(tls, ND_COMMA, lhs, rhs, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
	}

	var node uintptr = new_node(tls, ND_COND, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
	(*Node)(unsafe.Pointer(node)).cond = cond
	(*Node)(unsafe.Pointer(node)).then = expr(tls, bp, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next)
	*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+9308)
	(*Node)(unsafe.Pointer(node)).els = conditional(tls, rest, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
	return node
}

// logor = logand ("||" logand)*
func logor(tls *libc.TLS, rest uintptr, tok uintptr) uintptr { /* parse.c:2216:13: */
	bp := tls.Alloc(8)
	defer tls.Free(8)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	var node uintptr = logand(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
	for equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9478) != 0 {
		var start uintptr = *(*uintptr)(unsafe.Pointer(bp /* tok */))
		node = new_binary(tls, ND_LOGOR, node, logand(tls, bp, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next), start)
	}
	*(*uintptr)(unsafe.Pointer(rest)) = *(*uintptr)(unsafe.Pointer(bp /* tok */))
	return node
}

// logand = bitor ("&&" bitor)*
func logand(tls *libc.TLS, rest uintptr, tok uintptr) uintptr { /* parse.c:2227:13: */
	bp := tls.Alloc(8)
	defer tls.Free(8)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	var node uintptr = bitor(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
	for equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9481) != 0 {
		var start uintptr = *(*uintptr)(unsafe.Pointer(bp /* tok */))
		node = new_binary(tls, ND_LOGAND, node, bitor(tls, bp, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next), start)
	}
	*(*uintptr)(unsafe.Pointer(rest)) = *(*uintptr)(unsafe.Pointer(bp /* tok */))
	return node
}

// bitor = bitxor ("|" bitxor)*
func bitor(tls *libc.TLS, rest uintptr, tok uintptr) uintptr { /* parse.c:2238:13: */
	bp := tls.Alloc(8)
	defer tls.Free(8)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	var node uintptr = bitxor(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
	for equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9484) != 0 {
		var start uintptr = *(*uintptr)(unsafe.Pointer(bp /* tok */))
		node = new_binary(tls, ND_BITOR, node, bitxor(tls, bp, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next), start)
	}
	*(*uintptr)(unsafe.Pointer(rest)) = *(*uintptr)(unsafe.Pointer(bp /* tok */))
	return node
}

// bitxor = bitand ("^" bitand)*
func bitxor(tls *libc.TLS, rest uintptr, tok uintptr) uintptr { /* parse.c:2249:13: */
	bp := tls.Alloc(8)
	defer tls.Free(8)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	var node uintptr = bitand(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
	for equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9486) != 0 {
		var start uintptr = *(*uintptr)(unsafe.Pointer(bp /* tok */))
		node = new_binary(tls, ND_BITXOR, node, bitand(tls, bp, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next), start)
	}
	*(*uintptr)(unsafe.Pointer(rest)) = *(*uintptr)(unsafe.Pointer(bp /* tok */))
	return node
}

// bitand = equality ("&" equality)*
func bitand(tls *libc.TLS, rest uintptr, tok uintptr) uintptr { /* parse.c:2260:13: */
	bp := tls.Alloc(8)
	defer tls.Free(8)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	var node uintptr = equality(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
	for equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9488) != 0 {
		var start uintptr = *(*uintptr)(unsafe.Pointer(bp /* tok */))
		node = new_binary(tls, ND_BITAND, node, equality(tls, bp, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next), start)
	}
	*(*uintptr)(unsafe.Pointer(rest)) = *(*uintptr)(unsafe.Pointer(bp /* tok */))
	return node
}

// equality = relational ("==" relational | "!=" relational)*
func equality(tls *libc.TLS, rest uintptr, tok uintptr) uintptr { /* parse.c:2271:13: */
	bp := tls.Alloc(8)
	defer tls.Free(8)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	var node uintptr = relational(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)))

	for {
		var start uintptr = *(*uintptr)(unsafe.Pointer(bp /* tok */))

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9490) != 0 {
			node = new_binary(tls, ND_EQ, node, relational(tls, bp, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next), start)
			continue
		}

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9493) != 0 {
			node = new_binary(tls, ND_NE, node, relational(tls, bp, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next), start)
			continue
		}

		*(*uintptr)(unsafe.Pointer(rest)) = *(*uintptr)(unsafe.Pointer(bp /* tok */))
		return node
	}
	return uintptr(0)
}

// relational = shift ("<" shift | "<=" shift | ">" shift | ">=" shift)*
func relational(tls *libc.TLS, rest uintptr, tok uintptr) uintptr { /* parse.c:2293:13: */
	bp := tls.Alloc(8)
	defer tls.Free(8)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	var node uintptr = shift(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)))

	for {
		var start uintptr = *(*uintptr)(unsafe.Pointer(bp /* tok */))

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9496) != 0 {
			node = new_binary(tls, ND_LT, node, shift(tls, bp, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next), start)
			continue
		}

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9498) != 0 {
			node = new_binary(tls, ND_LE, node, shift(tls, bp, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next), start)
			continue
		}

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9501) != 0 {
			node = new_binary(tls, ND_LT, shift(tls, bp, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next), node, start)
			continue
		}

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9503) != 0 {
			node = new_binary(tls, ND_LE, shift(tls, bp, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next), node, start)
			continue
		}

		*(*uintptr)(unsafe.Pointer(rest)) = *(*uintptr)(unsafe.Pointer(bp /* tok */))
		return node
	}
	return uintptr(0)
}

// shift = add ("<<" add | ">>" add)*
func shift(tls *libc.TLS, rest uintptr, tok uintptr) uintptr { /* parse.c:2325:13: */
	bp := tls.Alloc(8)
	defer tls.Free(8)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	var node uintptr = add(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)))

	for {
		var start uintptr = *(*uintptr)(unsafe.Pointer(bp /* tok */))

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9506) != 0 {
			node = new_binary(tls, ND_SHL, node, add(tls, bp, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next), start)
			continue
		}

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9509) != 0 {
			node = new_binary(tls, ND_SHR, node, add(tls, bp, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next), start)
			continue
		}

		*(*uintptr)(unsafe.Pointer(rest)) = *(*uintptr)(unsafe.Pointer(bp /* tok */))
		return node
	}
	return uintptr(0)
}

// In C, `+` operator is overloaded to perform the pointer arithmetic.
// If p is a pointer, p+n adds not n but sizeof(*p)*n to the value of p,
// so that p+n points to the location n elements (not bytes) ahead of p.
// In other words, we need to scale an integer value before adding to a
// pointer value. This function takes care of the scaling.
func new_add(tls *libc.TLS, lhs uintptr, rhs uintptr, tok uintptr) uintptr { /* parse.c:2351:13: */
	add_type(tls, lhs)
	add_type(tls, rhs)

	// num + num
	if is_numeric(tls, (*Node)(unsafe.Pointer(lhs)).ty) != 0 && is_numeric(tls, (*Node)(unsafe.Pointer(rhs)).ty) != 0 {
		return new_binary(tls, ND_ADD, lhs, rhs, tok)
	}

	if (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(lhs)).ty)).base != 0 && (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(rhs)).ty)).base != 0 {
		error_tok(tls, tok, ts+9512, 0)
	}

	// Canonicalize `num + ptr` to `ptr + num`.
	if !(int32((*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(lhs)).ty)).base) != 0) && (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(rhs)).ty)).base != 0 {
		var tmp uintptr = lhs
		lhs = rhs
		rhs = tmp
	}

	// VLA + num
	if (*Type)(unsafe.Pointer((*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(lhs)).ty)).base)).kind == TY_VLA {
		rhs = new_binary(tls, ND_MUL, rhs, new_var_node(tls, (*Type)(unsafe.Pointer((*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(lhs)).ty)).base)).vla_size, tok), tok)
		return new_binary(tls, ND_ADD, lhs, rhs, tok)
	}

	// ptr + num
	rhs = new_binary(tls, ND_MUL, rhs, new_long(tls, int64((*Type)(unsafe.Pointer((*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(lhs)).ty)).base)).size), tok), tok)
	return new_binary(tls, ND_ADD, lhs, rhs, tok)
}

// Like `+`, `-` is overloaded for the pointer type.
func new_sub(tls *libc.TLS, lhs uintptr, rhs uintptr, tok uintptr) uintptr { /* parse.c:2381:13: */
	add_type(tls, lhs)
	add_type(tls, rhs)

	// num - num
	if is_numeric(tls, (*Node)(unsafe.Pointer(lhs)).ty) != 0 && is_numeric(tls, (*Node)(unsafe.Pointer(rhs)).ty) != 0 {
		return new_binary(tls, ND_SUB, lhs, rhs, tok)
	}

	// VLA + num
	if (*Type)(unsafe.Pointer((*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(lhs)).ty)).base)).kind == TY_VLA {
		rhs = new_binary(tls, ND_MUL, rhs, new_var_node(tls, (*Type)(unsafe.Pointer((*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(lhs)).ty)).base)).vla_size, tok), tok)
		add_type(tls, rhs)
		var node uintptr = new_binary(tls, ND_SUB, lhs, rhs, tok)
		(*Node)(unsafe.Pointer(node)).ty = (*Node)(unsafe.Pointer(lhs)).ty
		return node
	}

	// ptr - num
	if (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(lhs)).ty)).base != 0 && is_integer(tls, (*Node)(unsafe.Pointer(rhs)).ty) != 0 {
		rhs = new_binary(tls, ND_MUL, rhs, new_long(tls, int64((*Type)(unsafe.Pointer((*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(lhs)).ty)).base)).size), tok), tok)
		add_type(tls, rhs)
		var node uintptr = new_binary(tls, ND_SUB, lhs, rhs, tok)
		(*Node)(unsafe.Pointer(node)).ty = (*Node)(unsafe.Pointer(lhs)).ty
		return node
	}

	// ptr - ptr, which returns how many elements are between the two.
	if (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(lhs)).ty)).base != 0 && (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(rhs)).ty)).base != 0 {
		var node uintptr = new_binary(tls, ND_SUB, lhs, rhs, tok)
		(*Node)(unsafe.Pointer(node)).ty = ty_long
		return new_binary(tls, ND_DIV, node, new_num(tls, int64((*Type)(unsafe.Pointer((*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(lhs)).ty)).base)).size), tok), tok)
	}

	error_tok(tls, tok, ts+9512, 0)
	return uintptr(0)
}

// add = mul ("+" mul | "-" mul)*
func add(tls *libc.TLS, rest uintptr, tok uintptr) uintptr { /* parse.c:2418:13: */
	bp := tls.Alloc(8)
	defer tls.Free(8)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	var node uintptr = mul(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)))

	for {
		var start uintptr = *(*uintptr)(unsafe.Pointer(bp /* tok */))

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9529) != 0 {
			node = new_add(tls, node, mul(tls, bp, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next), start)
			continue
		}

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+7420) != 0 {
			node = new_sub(tls, node, mul(tls, bp, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next), start)
			continue
		}

		*(*uintptr)(unsafe.Pointer(rest)) = *(*uintptr)(unsafe.Pointer(bp /* tok */))
		return node
	}
	return uintptr(0)
}

// mul = cast ("*" cast | "/" cast | "%" cast)*
func mul(tls *libc.TLS, rest uintptr, tok uintptr) uintptr { /* parse.c:2440:13: */
	bp := tls.Alloc(8)
	defer tls.Free(8)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	var node uintptr = cast1(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)))

	for {
		var start uintptr = *(*uintptr)(unsafe.Pointer(bp /* tok */))

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8833) != 0 {
			node = new_binary(tls, ND_MUL, node, cast1(tls, bp, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next), start)
			continue
		}

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+6710) != 0 {
			node = new_binary(tls, ND_DIV, node, cast1(tls, bp, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next), start)
			continue
		}

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9531) != 0 {
			node = new_binary(tls, ND_MOD, node, cast1(tls, bp, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next), start)
			continue
		}

		*(*uintptr)(unsafe.Pointer(rest)) = *(*uintptr)(unsafe.Pointer(bp /* tok */))
		return node
	}
	return uintptr(0)
}

// cast = "(" type-name ")" cast | unary
func cast1(tls *libc.TLS, rest uintptr, tok uintptr) uintptr { /* parse.c:2467:13: */
	bp := tls.Alloc(8)
	defer tls.Free(8)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8666) != 0 && is_typename(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp)))).next) != 0 {
		var start uintptr = *(*uintptr)(unsafe.Pointer(bp /* tok */))
		var ty uintptr = typename(tls, bp, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next)
		*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8668)

		// compound literal
		if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8837) != 0 {
			return unary(tls, rest, start)
		}

		// type cast
		var node uintptr = new_cast(tls, cast1(tls, rest, *(*uintptr)(unsafe.Pointer(bp /* tok */))), ty)
		(*Node)(unsafe.Pointer(node)).tok = start
		return node
	}

	return unary(tls, rest, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
}

// unary = ("+" | "-" | "*" | "&" | "!" | "~") cast
//
//	| ("++" | "--") unary
//	| "&&" ident
//	| postfix
func unary(tls *libc.TLS, rest uintptr, tok uintptr) uintptr { /* parse.c:2490:13: */
	if equal(tls, tok, ts+9529) != 0 {
		return cast1(tls, rest, (*Token)(unsafe.Pointer(tok)).next)
	}

	if equal(tls, tok, ts+7420) != 0 {
		return new_unary(tls, ND_NEG, cast1(tls, rest, (*Token)(unsafe.Pointer(tok)).next), tok)
	}

	if equal(tls, tok, ts+9488) != 0 {
		var lhs uintptr = cast1(tls, rest, (*Token)(unsafe.Pointer(tok)).next)
		add_type(tls, lhs)
		if (*Node)(unsafe.Pointer(lhs)).kind == ND_MEMBER && (*Member)(unsafe.Pointer((*Node)(unsafe.Pointer(lhs)).member)).is_bitfield != 0 {
			error_tok(tls, tok, ts+9533, 0)
		}
		return new_unary(tls, ND_ADDR, lhs, tok)
	}

	if equal(tls, tok, ts+8833) != 0 {
		// [https://www.sigbus.info/n1570#6.5.3.2p4] This is an oddity
		// in the C spec, but dereferencing a function shouldn't do
		// anything. If foo is a function, `*foo`, `**foo` or `*****foo`
		// are all equivalent to just `foo`.
		var node uintptr = cast1(tls, rest, (*Token)(unsafe.Pointer(tok)).next)
		add_type(tls, node)
		if (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).ty)).kind == TY_FUNC {
			return node
		}
		return new_unary(tls, ND_DEREF, node, tok)
	}

	if equal(tls, tok, ts+9565) != 0 {
		return new_unary(tls, ND_NOT, cast1(tls, rest, (*Token)(unsafe.Pointer(tok)).next), tok)
	}

	if equal(tls, tok, ts+9567) != 0 {
		return new_unary(tls, ND_BITNOT, cast1(tls, rest, (*Token)(unsafe.Pointer(tok)).next), tok)
	}

	// Read ++i as i+=1
	if equal(tls, tok, ts+9569) != 0 {
		return to_assign(tls, new_add(tls, unary(tls, rest, (*Token)(unsafe.Pointer(tok)).next), new_num(tls, int64(1), tok), tok))
	}

	// Read --i as i-=1
	if equal(tls, tok, ts+9572) != 0 {
		return to_assign(tls, new_sub(tls, unary(tls, rest, (*Token)(unsafe.Pointer(tok)).next), new_num(tls, int64(1), tok), tok))
	}

	// [GNU] labels-as-values
	if equal(tls, tok, ts+9481) != 0 {
		var node uintptr = new_node(tls, ND_LABEL_VAL, tok)
		(*Node)(unsafe.Pointer(node)).label = get_ident(tls, (*Token)(unsafe.Pointer(tok)).next)
		(*Node)(unsafe.Pointer(node)).goto_next = gotos
		gotos = node
		*(*uintptr)(unsafe.Pointer(rest)) = (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(tok)).next)).next
		return node
	}

	return postfix(tls, rest, tok)
}

// struct-members = (declspec declarator (","  declarator)* ";")*
func struct_members(tls *libc.TLS, rest uintptr, tok uintptr, ty uintptr) { /* parse.c:2545:13: */
	bp := tls.Alloc(76)
	defer tls.Free(76)
	*(*uintptr)(unsafe.Pointer(bp + 56)) = tok

	*(*Member)(unsafe.Pointer(bp /* head */)) = Member{}
	var cur uintptr = bp /* &head */
	var idx int32 = 0

	for !(equal(tls, *(*uintptr)(unsafe.Pointer(bp + 56)), ts+8835) != 0) {
		*(*VarAttr)(unsafe.Pointer(bp + 64 /* attr */)) = VarAttr{}
		var basety uintptr = declspec(tls, bp+56, *(*uintptr)(unsafe.Pointer(bp + 56 /* tok */)), bp+64)
		var first uint8 = uint8(1)

		// Anonymous struct member
		if ((*Type)(unsafe.Pointer(basety)).kind == TY_STRUCT || (*Type)(unsafe.Pointer(basety)).kind == TY_UNION) && consume(tls, bp+56, *(*uintptr)(unsafe.Pointer(bp + 56)), ts+8876) != 0 {
			var mem uintptr = libc.Xcalloc(tls, uint64(1), uint64(unsafe.Sizeof(Member{})))
			(*Member)(unsafe.Pointer(mem)).ty = basety
			(*Member)(unsafe.Pointer(mem)).idx = libc.PostIncInt32(&idx, 1)
			(*Member)(unsafe.Pointer(mem)).align = func() int32 {
				if (*VarAttr)(unsafe.Pointer(bp+64)).align != 0 {
					return (*VarAttr)(unsafe.Pointer(bp + 64 /* &attr */)).align
				}
				return (*Type)(unsafe.Pointer((*Member)(unsafe.Pointer(mem)).ty)).align
			}()
			cur = libc.AssignPtrUintptr(cur, mem)
			continue
		}

		// Regular struct members
		for !(consume(tls, bp+56, *(*uintptr)(unsafe.Pointer(bp + 56)), ts+8876) != 0) {
			if !(first != 0) {
				*(*uintptr)(unsafe.Pointer(bp + 56 /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp + 56 /* tok */)), ts+8331)
			}
			first = uint8(0)

			var mem uintptr = libc.Xcalloc(tls, uint64(1), uint64(unsafe.Sizeof(Member{})))
			(*Member)(unsafe.Pointer(mem)).ty = declarator(tls, bp+56, *(*uintptr)(unsafe.Pointer(bp + 56 /* tok */)), basety)
			(*Member)(unsafe.Pointer(mem)).name = (*Type)(unsafe.Pointer((*Member)(unsafe.Pointer(mem)).ty)).name
			(*Member)(unsafe.Pointer(mem)).idx = libc.PostIncInt32(&idx, 1)
			(*Member)(unsafe.Pointer(mem)).align = func() int32 {
				if (*VarAttr)(unsafe.Pointer(bp+64)).align != 0 {
					return (*VarAttr)(unsafe.Pointer(bp + 64 /* &attr */)).align
				}
				return (*Type)(unsafe.Pointer((*Member)(unsafe.Pointer(mem)).ty)).align
			}()

			if consume(tls, bp+56, *(*uintptr)(unsafe.Pointer(bp + 56)), ts+9308) != 0 {
				(*Member)(unsafe.Pointer(mem)).is_bitfield = uint8(1)
				(*Member)(unsafe.Pointer(mem)).bit_width = int32(const_expr(tls, bp+56, *(*uintptr)(unsafe.Pointer(bp + 56 /* tok */))))
			}

			cur = libc.AssignPtrUintptr(cur, mem)
		}
	}

	// If the last element is an array of incomplete type, it's
	// called a "flexible array member". It should behave as if
	// if were a zero-sized array.
	if cur != bp && (*Type)(unsafe.Pointer((*Member)(unsafe.Pointer(cur)).ty)).kind == TY_ARRAY && (*Type)(unsafe.Pointer((*Member)(unsafe.Pointer(cur)).ty)).array_len < 0 {
		(*Member)(unsafe.Pointer(cur)).ty = array_of(tls, (*Type)(unsafe.Pointer((*Member)(unsafe.Pointer(cur)).ty)).base, 0)
		(*Type)(unsafe.Pointer(ty)).is_flexible = uint8(1)
	}

	*(*uintptr)(unsafe.Pointer(rest)) = (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 56 /* tok */)))).next
	(*Type)(unsafe.Pointer(ty)).members = (*Member)(unsafe.Pointer(bp /* &head */)).next
}

// attribute = ("__attribute__" "(" "(" "packed" ")" ")")*
func attribute_list(tls *libc.TLS, tok uintptr, ty uintptr) uintptr { /* parse.c:2600:14: */
	bp := tls.Alloc(8)
	defer tls.Free(8)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	for consume(tls, bp, *(*uintptr)(unsafe.Pointer(bp)), ts+9575) != 0 {
		*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8666)
		*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8666)

		var first uint8 = uint8(1)

		for !(consume(tls, bp, *(*uintptr)(unsafe.Pointer(bp)), ts+8668) != 0) {
			if !(first != 0) {
				*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8331)
			}
			first = uint8(0)

			if consume(tls, bp, *(*uintptr)(unsafe.Pointer(bp)), ts+9589) != 0 {
				(*Type)(unsafe.Pointer(ty)).is_packed = uint8(1)
				continue
			}

			if consume(tls, bp, *(*uintptr)(unsafe.Pointer(bp)), ts+9596) != 0 {
				*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8666)
				(*Type)(unsafe.Pointer(ty)).align = int32(const_expr(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */))))
				*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8668)
				continue
			}

			error_tok(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+9604, 0)
		}

		*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8668)
	}

	return *(*uintptr)(unsafe.Pointer(bp /* tok */))
}

// struct-union-decl = attribute? ident? ("{" struct-members)?
func struct_union_decl(tls *libc.TLS, rest uintptr, tok uintptr) uintptr { /* parse.c:2634:13: */
	bp := tls.Alloc(8)
	defer tls.Free(8)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	var ty uintptr = struct_type(tls)
	*(*uintptr)(unsafe.Pointer(bp /* tok */)) = attribute_list(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ty)

	// Read a tag.
	var tag uintptr = uintptr(0)
	if (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp)))).kind == TK_IDENT {
		tag = *(*uintptr)(unsafe.Pointer(bp /* tok */))
		*(*uintptr)(unsafe.Pointer(bp /* tok */)) = (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next
	}

	if tag != 0 && !(equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8837) != 0) {
		*(*uintptr)(unsafe.Pointer(rest)) = *(*uintptr)(unsafe.Pointer(bp /* tok */))

		var ty2 uintptr = find_tag(tls, tag)
		if ty2 != 0 {
			return ty2
		}

		(*Type)(unsafe.Pointer(ty)).size = -1
		push_tag_scope(tls, tag, ty)
		return ty
	}

	*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8837)

	// Construct a struct object.
	struct_members(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ty)
	*(*uintptr)(unsafe.Pointer(rest)) = attribute_list(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ty)

	if tag != 0 {
		// If this is a redefinition, overwrite a previous type.
		// Otherwise, register the struct type.
		var ty2 uintptr = hashmap_get2(tls, scope+24, (*Token)(unsafe.Pointer(tag)).loc, (*Token)(unsafe.Pointer(tag)).len)
		if ty2 != 0 {
			*(*Type)(unsafe.Pointer(ty2)) = *(*Type)(unsafe.Pointer(ty))
			return ty2
		}

		push_tag_scope(tls, tag, ty)
	}

	return ty
}

// struct-decl = struct-union-decl
func struct_decl(tls *libc.TLS, rest uintptr, tok uintptr) uintptr { /* parse.c:2679:13: */
	var ty uintptr = struct_union_decl(tls, rest, tok)
	(*Type)(unsafe.Pointer(ty)).kind = TY_STRUCT

	if (*Type)(unsafe.Pointer(ty)).size < 0 {
		return ty
	}

	// Assign offsets within the struct to members.
	var bits int32 = 0

	{
		var mem uintptr = (*Type)(unsafe.Pointer(ty)).members
		for ; mem != 0; mem = (*Member)(unsafe.Pointer(mem)).next {
			if (*Member)(unsafe.Pointer(mem)).is_bitfield != 0 && (*Member)(unsafe.Pointer(mem)).bit_width == 0 {
				// Zero-width anonymous bitfield has a special meaning.
				// It affects only alignment.
				bits = align_to(tls, bits, (*Type)(unsafe.Pointer((*Member)(unsafe.Pointer(mem)).ty)).size*8)
			} else if (*Member)(unsafe.Pointer(mem)).is_bitfield != 0 {
				var sz int32 = (*Type)(unsafe.Pointer((*Member)(unsafe.Pointer(mem)).ty)).size
				if bits/(sz*8) != (bits+(*Member)(unsafe.Pointer(mem)).bit_width-1)/(sz*8) {
					bits = align_to(tls, bits, sz*8)
				}

				(*Member)(unsafe.Pointer(mem)).offset = align_down(tls, bits/8, sz)
				(*Member)(unsafe.Pointer(mem)).bit_offset = bits % (sz * 8)
				bits = bits + (*Member)(unsafe.Pointer(mem)).bit_width
			} else {
				if !(int32((*Type)(unsafe.Pointer(ty)).is_packed) != 0) {
					bits = align_to(tls, bits, (*Member)(unsafe.Pointer(mem)).align*8)
				}
				(*Member)(unsafe.Pointer(mem)).offset = bits / 8
				bits = bits + (*Type)(unsafe.Pointer((*Member)(unsafe.Pointer(mem)).ty)).size*8
			}

			if !(int32((*Type)(unsafe.Pointer(ty)).is_packed) != 0) && (*Type)(unsafe.Pointer(ty)).align < (*Member)(unsafe.Pointer(mem)).align {
				(*Type)(unsafe.Pointer(ty)).align = (*Member)(unsafe.Pointer(mem)).align
			}
		}
	}

	(*Type)(unsafe.Pointer(ty)).size = align_to(tls, bits, (*Type)(unsafe.Pointer(ty)).align*8) / 8
	return ty
}

// union-decl = struct-union-decl
func union_decl(tls *libc.TLS, rest uintptr, tok uintptr) uintptr { /* parse.c:2718:13: */
	var ty uintptr = struct_union_decl(tls, rest, tok)
	(*Type)(unsafe.Pointer(ty)).kind = TY_UNION

	if (*Type)(unsafe.Pointer(ty)).size < 0 {
		return ty
	}

	// If union, we don't have to assign offsets because they
	// are already initialized to zero. We need to compute the
	// alignment and the size though.
	{
		var mem uintptr = (*Type)(unsafe.Pointer(ty)).members
		for ; mem != 0; mem = (*Member)(unsafe.Pointer(mem)).next {
			if (*Type)(unsafe.Pointer(ty)).align < (*Member)(unsafe.Pointer(mem)).align {
				(*Type)(unsafe.Pointer(ty)).align = (*Member)(unsafe.Pointer(mem)).align
			}
			if (*Type)(unsafe.Pointer(ty)).size < (*Type)(unsafe.Pointer((*Member)(unsafe.Pointer(mem)).ty)).size {
				(*Type)(unsafe.Pointer(ty)).size = (*Type)(unsafe.Pointer((*Member)(unsafe.Pointer(mem)).ty)).size
			}
		}
	}
	(*Type)(unsafe.Pointer(ty)).size = align_to(tls, (*Type)(unsafe.Pointer(ty)).size, (*Type)(unsafe.Pointer(ty)).align)
	return ty
}

// Find a struct member by name.
func get_struct_member(tls *libc.TLS, ty uintptr, tok uintptr) uintptr { /* parse.c:2739:15: */
	{
		var mem uintptr = (*Type)(unsafe.Pointer(ty)).members
		for ; mem != 0; mem = (*Member)(unsafe.Pointer(mem)).next {
			// Anonymous struct member
			if ((*Type)(unsafe.Pointer((*Member)(unsafe.Pointer(mem)).ty)).kind == TY_STRUCT || (*Type)(unsafe.Pointer((*Member)(unsafe.Pointer(mem)).ty)).kind == TY_UNION) && !(int32((*Member)(unsafe.Pointer(mem)).name) != 0) {
				if get_struct_member(tls, (*Member)(unsafe.Pointer(mem)).ty, tok) != 0 {
					return mem
				}
				continue
			}

			// Regular struct member
			if (*Token)(unsafe.Pointer((*Member)(unsafe.Pointer(mem)).name)).len == (*Token)(unsafe.Pointer(tok)).len && !(libc.Xstrncmp(tls, (*Token)(unsafe.Pointer((*Member)(unsafe.Pointer(mem)).name)).loc, (*Token)(unsafe.Pointer(tok)).loc, uint64((*Token)(unsafe.Pointer(tok)).len)) != 0) {
				return mem
			}
		}
	}
	return uintptr(0)
}

// Create a node representing a struct member access, such as foo.bar
// where foo is a struct and bar is a member name.
//
// C has a feature called "anonymous struct" which allows a struct to
// have another unnamed struct as a member like this:
//
//	struct { struct { int a; }; int b; } x;
//
// The members of an anonymous struct belong to the outer struct's
// member namespace. Therefore, in the above example, you can access
// member "a" of the anonymous struct as "x.a".
//
// This function takes care of anonymous structs.
func struct_ref(tls *libc.TLS, node uintptr, tok uintptr) uintptr { /* parse.c:2770:13: */
	add_type(tls, node)
	if (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).ty)).kind != TY_STRUCT && (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).ty)).kind != TY_UNION {
		error_tok(tls, (*Node)(unsafe.Pointer(node)).tok, ts+9622, 0)
	}

	var ty uintptr = (*Node)(unsafe.Pointer(node)).ty

	for {
		var mem uintptr = get_struct_member(tls, ty, tok)
		if !(mem != 0) {
			error_tok(tls, tok, ts+9647, 0)
		}
		node = new_unary(tls, ND_MEMBER, node, tok)
		(*Node)(unsafe.Pointer(node)).member = mem
		if (*Member)(unsafe.Pointer(mem)).name != 0 {
			break
		}
		ty = (*Member)(unsafe.Pointer(mem)).ty
	}
	return node
}

// Convert A++ to `(typeof A)((A += 1) - 1)`
func new_inc_dec(tls *libc.TLS, node uintptr, tok uintptr, addend int32) uintptr { /* parse.c:2791:13: */
	add_type(tls, node)
	return new_cast(tls, new_add(tls, to_assign(tls, new_add(tls, node, new_num(tls, int64(addend), tok), tok)),
		new_num(tls, int64(-addend), tok), tok),
		(*Node)(unsafe.Pointer(node)).ty)
}

// postfix = "(" type-name ")" "{" initializer-list "}"
//
//	= ident "(" func-args ")" postfix-tail*
//	| primary postfix-tail*
//
// postfix-tail = "[" expr "]"
//
//	| "(" func-args ")"
//	| "." ident
//	| "->" ident
//	| "++"
//	| "--"
func postfix(tls *libc.TLS, rest uintptr, tok uintptr) uintptr { /* parse.c:2808:13: */
	bp := tls.Alloc(8)
	defer tls.Free(8)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8666) != 0 && is_typename(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp)))).next) != 0 {
		// Compound literal
		var start uintptr = *(*uintptr)(unsafe.Pointer(bp /* tok */))
		var ty uintptr = typename(tls, bp, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next)
		*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8668)

		if (*Scope)(unsafe.Pointer(scope)).next == uintptr(0) {
			var var1 uintptr = new_anon_gvar(tls, ty)
			gvar_initializer(tls, rest, *(*uintptr)(unsafe.Pointer(bp /* tok */)), var1)
			return new_var_node(tls, var1, start)
		}

		var var1 uintptr = new_lvar(tls, ts+8875, ty)
		var lhs uintptr = lvar_initializer(tls, rest, *(*uintptr)(unsafe.Pointer(bp /* tok */)), var1)
		var rhs uintptr = new_var_node(tls, var1, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		return new_binary(tls, ND_COMMA, lhs, rhs, start)
	}

	var node uintptr = primary(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)))

	for {
		if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8666) != 0 {
			node = funcall(tls, bp, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next, node)
			continue
		}

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8831) != 0 {
			// x[y] is short for *(x+y)
			var start uintptr = *(*uintptr)(unsafe.Pointer(bp /* tok */))
			var idx uintptr = expr(tls, bp, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next)
			*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8829)
			node = new_unary(tls, ND_DEREF, new_add(tls, node, idx, start), start)
			continue
		}

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+6708) != 0 {
			node = struct_ref(tls, node, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next)
			*(*uintptr)(unsafe.Pointer(bp /* tok */)) = (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next)).next
			continue
		}

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9662) != 0 {
			// x->y is short for (*x).y
			node = new_unary(tls, ND_DEREF, node, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
			node = struct_ref(tls, node, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next)
			*(*uintptr)(unsafe.Pointer(bp /* tok */)) = (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next)).next
			continue
		}

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9569) != 0 {
			node = new_inc_dec(tls, node, *(*uintptr)(unsafe.Pointer(bp /* tok */)), 1)
			*(*uintptr)(unsafe.Pointer(bp /* tok */)) = (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next
			continue
		}

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9572) != 0 {
			node = new_inc_dec(tls, node, *(*uintptr)(unsafe.Pointer(bp /* tok */)), -1)
			*(*uintptr)(unsafe.Pointer(bp /* tok */)) = (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next
			continue
		}

		*(*uintptr)(unsafe.Pointer(rest)) = *(*uintptr)(unsafe.Pointer(bp /* tok */))
		return node
	}
	return uintptr(0)
}

// funcall = (assign ("," assign)*)? ")"
func funcall(tls *libc.TLS, rest uintptr, tok uintptr, fn uintptr) uintptr { /* parse.c:2876:13: */
	bp := tls.Alloc(288)
	defer tls.Free(288)
	*(*uintptr)(unsafe.Pointer(bp + 280)) = tok

	add_type(tls, fn)

	if (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(fn)).ty)).kind != TY_FUNC && ((*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(fn)).ty)).kind != TY_PTR || (*Type)(unsafe.Pointer((*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(fn)).ty)).base)).kind != TY_FUNC) {
		error_tok(tls, (*Node)(unsafe.Pointer(fn)).tok, ts+9665, 0)
	}

	var ty uintptr
	if (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(fn)).ty)).kind == TY_FUNC {
		ty = (*Node)(unsafe.Pointer(fn)).ty
	} else {
		ty = (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(fn)).ty)).base
	}
	var param_ty uintptr = (*Type)(unsafe.Pointer(ty)).params

	*(*Node)(unsafe.Pointer(bp /* head */)) = Node{}
	var cur uintptr = bp /* &head */

	for !(equal(tls, *(*uintptr)(unsafe.Pointer(bp + 280)), ts+8668) != 0) {
		if cur != bp {
			*(*uintptr)(unsafe.Pointer(bp + 280 /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp + 280 /* tok */)), ts+8331)
		}

		var arg uintptr = assign(tls, bp+280, *(*uintptr)(unsafe.Pointer(bp + 280 /* tok */)))
		add_type(tls, arg)

		if !(param_ty != 0) && !(int32((*Type)(unsafe.Pointer(ty)).is_variadic) != 0) {
			error_tok(tls, *(*uintptr)(unsafe.Pointer(bp + 280 /* tok */)), ts+9680, 0)
		}

		if param_ty != 0 {
			if (*Type)(unsafe.Pointer(param_ty)).kind != TY_STRUCT && (*Type)(unsafe.Pointer(param_ty)).kind != TY_UNION {
				arg = new_cast(tls, arg, param_ty)
			}
			param_ty = (*Type)(unsafe.Pointer(param_ty)).next
		} else if (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(arg)).ty)).kind == TY_FLOAT {
			// If parameter type is omitted (e.g. in "..."), float
			// arguments are promoted to double.
			arg = new_cast(tls, arg, ty_double)
		}

		cur = libc.AssignPtrUintptr(cur+8, arg)
	}

	if param_ty != 0 {
		error_tok(tls, *(*uintptr)(unsafe.Pointer(bp + 280 /* tok */)), ts+9699, 0)
	}

	*(*uintptr)(unsafe.Pointer(rest)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp + 280 /* tok */)), ts+8668)

	var node uintptr = new_unary(tls, ND_FUNCALL, fn, *(*uintptr)(unsafe.Pointer(bp + 280 /* tok */)))
	(*Node)(unsafe.Pointer(node)).func_ty = ty
	(*Node)(unsafe.Pointer(node)).ty = (*Type)(unsafe.Pointer(ty)).return_ty
	(*Node)(unsafe.Pointer(node)).args = (*Node)(unsafe.Pointer(bp /* &head */)).next

	// If a function returns a struct, it is caller's responsibility
	// to allocate a space for the return value.
	if (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).ty)).kind == TY_STRUCT || (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).ty)).kind == TY_UNION {
		(*Node)(unsafe.Pointer(node)).ret_buffer = new_lvar(tls, ts+8875, (*Node)(unsafe.Pointer(node)).ty)
	}
	return node
}

// generic-selection = "(" assign "," generic-assoc ("," generic-assoc)* ")"
//
// generic-assoc = type-name ":" assign
//
//	| "default" ":" assign
func generic_selection(tls *libc.TLS, rest uintptr, tok uintptr) uintptr { /* parse.c:2933:13: */
	bp := tls.Alloc(8)
	defer tls.Free(8)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	var start uintptr = *(*uintptr)(unsafe.Pointer(bp /* tok */))
	*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8666)

	var ctrl uintptr = assign(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
	add_type(tls, ctrl)

	var t1 uintptr = (*Node)(unsafe.Pointer(ctrl)).ty
	if (*Type)(unsafe.Pointer(t1)).kind == TY_FUNC {
		t1 = pointer_to(tls, t1)
	} else if (*Type)(unsafe.Pointer(t1)).kind == TY_ARRAY {
		t1 = pointer_to(tls, (*Type)(unsafe.Pointer(t1)).base)
	}

	var ret uintptr = uintptr(0)

	for !(consume(tls, rest, *(*uintptr)(unsafe.Pointer(bp)), ts+8668) != 0) {
		*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8331)

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9310) != 0 {
			*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next, ts+9308)
			var node uintptr = assign(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
			if !(ret != 0) {
				ret = node
			}
			continue
		}

		var t2 uintptr = typename(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+9308)
		var node uintptr = assign(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		if is_compatible(tls, t1, t2) != 0 {
			ret = node
		}
	}

	if !(ret != 0) {
		error_tok(tls, start,
			ts+9717, 0)
	}
	return ret
}

// primary = "(" "{" stmt+ "}" ")"
//
//	| "(" expr ")"
//	| "sizeof" "(" type-name ")"
//	| "sizeof" unary
//	| "_Alignof" "(" type-name ")"
//	| "_Alignof" unary
//	| "_Generic" generic-selection
//	| "__builtin_types_compatible_p" "(" type-name, type-name, ")"
//	| "__builtin_reg_class" "(" type-name ")"
//	| ident
//	| str
//	| num
func primary(tls *libc.TLS, rest uintptr, tok uintptr) uintptr { /* parse.c:2984:13: */
	bp := tls.Alloc(8)
	defer tls.Free(8)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	var start uintptr = *(*uintptr)(unsafe.Pointer(bp /* tok */))

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8666) != 0 && equal(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp)))).next, ts+8837) != 0 {
		// This is a GNU statement expresssion.
		var node uintptr = new_node(tls, ND_STMT_EXPR, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		(*Node)(unsafe.Pointer(node)).body = (*Node)(unsafe.Pointer(compound_stmt(tls, bp, (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next)).next))).body
		*(*uintptr)(unsafe.Pointer(rest)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8668)
		return node
	}

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8666) != 0 {
		var node uintptr = expr(tls, bp, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next)
		*(*uintptr)(unsafe.Pointer(rest)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8668)
		return node
	}

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9794) != 0 && equal(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp)))).next, ts+8666) != 0 && is_typename(tls, (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp)))).next)).next) != 0 {
		var ty uintptr = typename(tls, bp, (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next)).next)
		*(*uintptr)(unsafe.Pointer(rest)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8668)

		if (*Type)(unsafe.Pointer(ty)).kind == TY_VLA {
			if (*Type)(unsafe.Pointer(ty)).vla_size != 0 {
				return new_var_node(tls, (*Type)(unsafe.Pointer(ty)).vla_size, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
			}

			var lhs uintptr = compute_vla_size(tls, ty, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
			var rhs uintptr = new_var_node(tls, (*Type)(unsafe.Pointer(ty)).vla_size, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
			return new_binary(tls, ND_COMMA, lhs, rhs, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		}

		return new_ulong(tls, int64((*Type)(unsafe.Pointer(ty)).size), start)
	}

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9794) != 0 {
		var node uintptr = unary(tls, rest, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next)
		add_type(tls, node)
		if (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).ty)).kind == TY_VLA {
			return new_var_node(tls, (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).ty)).vla_size, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		}
		return new_ulong(tls, int64((*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).ty)).size), *(*uintptr)(unsafe.Pointer(bp /* tok */)))
	}

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9801) != 0 && equal(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp)))).next, ts+8666) != 0 && is_typename(tls, (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp)))).next)).next) != 0 {
		var ty uintptr = typename(tls, bp, (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next)).next)
		*(*uintptr)(unsafe.Pointer(rest)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8668)
		return new_ulong(tls, int64((*Type)(unsafe.Pointer(ty)).align), *(*uintptr)(unsafe.Pointer(bp /* tok */)))
	}

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9801) != 0 {
		var node uintptr = unary(tls, rest, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next)
		add_type(tls, node)
		return new_ulong(tls, int64((*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).ty)).align), *(*uintptr)(unsafe.Pointer(bp /* tok */)))
	}

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9810) != 0 {
		return generic_selection(tls, rest, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next)
	}

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9819) != 0 {
		*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next, ts+8666)
		var t1 uintptr = typename(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8331)
		var t2 uintptr = typename(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		*(*uintptr)(unsafe.Pointer(rest)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8668)
		return new_num(tls, int64(is_compatible(tls, t1, t2)), start)
	}

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9848) != 0 {
		*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next, ts+8666)
		var ty uintptr = typename(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		*(*uintptr)(unsafe.Pointer(rest)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8668)

		if is_integer(tls, ty) != 0 || (*Type)(unsafe.Pointer(ty)).kind == TY_PTR {
			return new_num(tls, int64(0), start)
		}
		if is_flonum(tls, ty) != 0 {
			return new_num(tls, int64(1), start)
		}
		return new_num(tls, int64(2), start)
	}

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9868) != 0 {
		var node uintptr = new_node(tls, ND_CAS, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next, ts+8666)
		(*Node)(unsafe.Pointer(node)).cas_addr = assign(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8331)
		(*Node)(unsafe.Pointer(node)).cas_old = assign(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8331)
		(*Node)(unsafe.Pointer(node)).cas_new = assign(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		*(*uintptr)(unsafe.Pointer(rest)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8668)
		return node
	}

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+9895) != 0 {
		var node uintptr = new_node(tls, ND_EXCH, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next, ts+8666)
		(*Node)(unsafe.Pointer(node)).lhs = assign(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8331)
		(*Node)(unsafe.Pointer(node)).rhs = assign(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		*(*uintptr)(unsafe.Pointer(rest)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8668)
		return node
	}

	if (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp)))).kind == TK_IDENT {
		// Variable or enum constant
		var sc uintptr = find_var(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		*(*uintptr)(unsafe.Pointer(rest)) = (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next

		// For "static inline" function
		if sc != 0 && (*VarScope)(unsafe.Pointer(sc)).__var != 0 && (*Obj)(unsafe.Pointer((*VarScope)(unsafe.Pointer(sc)).__var)).is_function != 0 {
			if current_fn1 != 0 {
				strarray_push(tls, current_fn1+128, (*Obj)(unsafe.Pointer((*VarScope)(unsafe.Pointer(sc)).__var)).name)
			} else {
				(*Obj)(unsafe.Pointer((*VarScope)(unsafe.Pointer(sc)).__var)).is_root = uint8(1)
			}
		}

		if sc != 0 {
			if (*VarScope)(unsafe.Pointer(sc)).__var != 0 {
				return new_var_node(tls, (*VarScope)(unsafe.Pointer(sc)).__var, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
			}
			if (*VarScope)(unsafe.Pointer(sc)).enum_ty != 0 {
				return new_num(tls, int64((*VarScope)(unsafe.Pointer(sc)).enum_val), *(*uintptr)(unsafe.Pointer(bp /* tok */)))
			}
		}

		if equal(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp)))).next, ts+8666) != 0 {
			error_tok(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+9921, 0)
		}
		error_tok(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+9956, 0)
	}

	if (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp)))).kind == TK_STR {
		var var1 uintptr = new_string_literal(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).str, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).ty)
		*(*uintptr)(unsafe.Pointer(rest)) = (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next
		return new_var_node(tls, var1, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
	}

	if (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp)))).kind == TK_NUM {
		var node uintptr
		if is_flonum(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp)))).ty) != 0 {
			node = new_node(tls, ND_NUM, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
			(*Node)(unsafe.Pointer(node)).fval = (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).fval
		} else {
			node = new_num(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).val, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		}

		(*Node)(unsafe.Pointer(node)).ty = (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).ty
		*(*uintptr)(unsafe.Pointer(rest)) = (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next
		return node
	}

	error_tok(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+9975, 0)
	return uintptr(0)
}

