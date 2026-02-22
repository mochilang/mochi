package main

import (
	"modernc.org/libc"
	"unsafe"
	"reflect"
)

func convert_pp_int(tls *libc.TLS, tok uintptr) uint8 { /* tokenize.c:342:13: */
	bp := tls.Alloc(8)
	defer tls.Free(8)

	*(*uintptr)(unsafe.Pointer(bp /* p */)) = (*Token)(unsafe.Pointer(tok)).loc

	// Read a binary, octal, decimal or hexadecimal number.
	var base int32 = 10
	if !(libc.Xstrncasecmp(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+11777, uint64(2)) != 0) && int32(*(*uint16)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(libc.X__ctype_b_loc(tls))) + uintptr(int32(*(*int8)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp)) + 2))))*2)))&int32(_ISxdigit) != 0 {
		*(*uintptr)(unsafe.Pointer(bp /* p */)) += uintptr(2)
		base = 16
	} else if !(libc.Xstrncasecmp(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+11780, uint64(2)) != 0) && (int32(*(*int8)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp)) + 2))) == '0' || int32(*(*int8)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp)) + 2))) == '1') {
		*(*uintptr)(unsafe.Pointer(bp /* p */)) += uintptr(2)
		base = 2
	} else if int32(*(*int8)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp))))) == '0' {
		base = 8
	}

	var val int64_t = int64_t(libc.Xstrtoul(tls, *(*uintptr)(unsafe.Pointer(bp /* p */)), bp, base))

	// Read U, L or LL suffixes.
	var l uint8 = uint8(0)
	var u uint8 = uint8(0)

	if startswith(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+11783) != 0 || startswith(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+11787) != 0 || startswith(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+11791) != 0 || startswith(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+11795) != 0 || startswith(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+11799) != 0 || startswith(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+11803) != 0 || startswith(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+11807) != 0 || startswith(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+11811) != 0 {
		*(*uintptr)(unsafe.Pointer(bp /* p */)) += uintptr(3)
		l = libc.AssignUint8(&u, uint8(1))
	} else if !(libc.Xstrncasecmp(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+11815, uint64(2)) != 0) || !(libc.Xstrncasecmp(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+11818, uint64(2)) != 0) {
		*(*uintptr)(unsafe.Pointer(bp /* p */)) += uintptr(2)
		l = libc.AssignUint8(&u, uint8(1))
	} else if startswith(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+11821) != 0 || startswith(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+11824) != 0 {
		*(*uintptr)(unsafe.Pointer(bp /* p */)) += uintptr(2)
		l = uint8(1)
	} else if int32(*(*int8)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp))))) == 'L' || int32(*(*int8)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp))))) == 'l' {
		*(*uintptr)(unsafe.Pointer(bp /* p */))++
		l = uint8(1)
	} else if int32(*(*int8)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp))))) == 'U' || int32(*(*int8)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp))))) == 'u' {
		*(*uintptr)(unsafe.Pointer(bp /* p */))++
		u = uint8(1)
	}

	if *(*uintptr)(unsafe.Pointer(bp)) != (*Token)(unsafe.Pointer(tok)).loc+uintptr((*Token)(unsafe.Pointer(tok)).len) {
		return uint8(0)
	}

	// Infer a type.
	var ty uintptr
	if base == 10 {
		if l != 0 && u != 0 {
			ty = ty_ulong
		} else if l != 0 {
			ty = ty_long
		} else if u != 0 {
			if val>>32 != 0 {
				ty = ty_ulong
			} else {
				ty = ty_uint
			}
		} else {
			if val>>31 != 0 {
				ty = ty_long
			} else {
				ty = ty_int
			}
		}
	} else {
		if l != 0 && u != 0 {
			ty = ty_ulong
		} else if l != 0 {
			if val>>63 != 0 {
				ty = ty_ulong
			} else {
				ty = ty_long
			}
		} else if u != 0 {
			if val>>32 != 0 {
				ty = ty_ulong
			} else {
				ty = ty_uint
			}
		} else if val>>63 != 0 {
			ty = ty_ulong
		} else if val>>32 != 0 {
			ty = ty_long
		} else if val>>31 != 0 {
			ty = ty_uint
		} else {
			ty = ty_int
		}
	}

	(*Token)(unsafe.Pointer(tok)).kind = TK_NUM
	(*Token)(unsafe.Pointer(tok)).val = val
	(*Token)(unsafe.Pointer(tok)).ty = ty
	return uint8(1)
}

// The definition of the numeric literal at the preprocessing stage
// is more relaxed than the definition of that at the later stages.
// In order to handle that, a numeric literal is tokenized as a
// "pp-number" token first and then converted to a regular number
// token after preprocessing.
//
// This function converts a pp-number token to a regular number token.
func convert_pp_number(tls *libc.TLS, tok uintptr) { /* tokenize.c:427:13: */
	bp := tls.Alloc(8)
	defer tls.Free(8)

	// Try to parse as an integer constant.
	if convert_pp_int(tls, tok) != 0 {
		return
	}

	// If it's not an integer, it must be a floating point constant.
	// var end uintptr at bp, 8

	var val float64 = libc.Xstrtold(tls, (*Token)(unsafe.Pointer(tok)).loc, bp)
	var ty uintptr
	if int32(*(*int8)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp))))) == 'f' || int32(*(*int8)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp))))) == 'F' {
		ty = ty_float
		*(*uintptr)(unsafe.Pointer(bp /* end */))++
	} else if int32(*(*int8)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp))))) == 'l' || int32(*(*int8)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp))))) == 'L' {
		ty = ty_ldouble
		*(*uintptr)(unsafe.Pointer(bp /* end */))++
	} else {
		ty = ty_double
	}

	if (*Token)(unsafe.Pointer(tok)).loc+uintptr((*Token)(unsafe.Pointer(tok)).len) != *(*uintptr)(unsafe.Pointer(bp)) {
		error_tok(tls, tok, ts+11827, 0)
	}

	(*Token)(unsafe.Pointer(tok)).kind = TK_NUM
	(*Token)(unsafe.Pointer(tok)).fval = val
	(*Token)(unsafe.Pointer(tok)).ty = ty
}

func convert_pp_tokens(tls *libc.TLS, tok uintptr) { /* tokenize.c:455:6: */
	{
		var t uintptr = tok
		for ; (*Token)(unsafe.Pointer(t)).kind != TK_EOF; t = (*Token)(unsafe.Pointer(t)).next {
			if is_keyword(tls, t) != 0 {
				(*Token)(unsafe.Pointer(t)).kind = TK_KEYWORD
			} else if (*Token)(unsafe.Pointer(t)).kind == TK_PP_NUM {
				convert_pp_number(tls, t)
			}
		}
	}
}

// Initialize line info for all tokens.
func add_line_numbers(tls *libc.TLS, tok uintptr) { /* tokenize.c:465:13: */
	var p uintptr = (*File)(unsafe.Pointer(current_file)).contents
	var n int32 = 1

	for __ccgo := true; __ccgo; __ccgo = *(*int8)(unsafe.Pointer(libc.PostIncUintptr(&p, 1))) != 0 {
		if p == (*Token)(unsafe.Pointer(tok)).loc {
			(*Token)(unsafe.Pointer(tok)).line_no = n
			tok = (*Token)(unsafe.Pointer(tok)).next
		}
		if int32(*(*int8)(unsafe.Pointer(p))) == '\n' {
			n++
		}
	}
}

func tokenize_string_literal(tls *libc.TLS, tok uintptr, basety uintptr) uintptr { /* tokenize.c:479:7: */
	var t uintptr
	if (*Type)(unsafe.Pointer(basety)).size == 2 {
		t = read_utf16_string_literal(tls, (*Token)(unsafe.Pointer(tok)).loc, (*Token)(unsafe.Pointer(tok)).loc)
	} else {
		t = read_utf32_string_literal(tls, (*Token)(unsafe.Pointer(tok)).loc, (*Token)(unsafe.Pointer(tok)).loc, basety)
	}
	(*Token)(unsafe.Pointer(t)).next = (*Token)(unsafe.Pointer(tok)).next
	return t
}

// Tokenize a given string and returns new tokens.
func tokenize(tls *libc.TLS, file uintptr) uintptr { /* tokenize.c:490:7: */
	bp := tls.Alloc(112)
	defer tls.Free(112)

	current_file = file

	var p uintptr = (*File)(unsafe.Pointer(file)).contents
	*(*Token)(unsafe.Pointer(bp /* head */)) = Token{}
	var cur uintptr = bp /* &head */

	at_bol = uint8(1)
	has_space = uint8(0)

	for *(*int8)(unsafe.Pointer(p)) != 0 {
		// Skip line comments.
		if startswith(tls, p, ts+11852) != 0 {
			p += uintptr(2)
			for int32(*(*int8)(unsafe.Pointer(p))) != '\n' {
				p++
			}
			has_space = uint8(1)
			continue
		}

		// Skip block comments.
		if startswith(tls, p, ts+11855) != 0 {
			var q uintptr = libc.Xstrstr(tls, p+uintptr(2), ts+11858)
			if !(q != 0) {
				error_at(tls, p, ts+11861, 0)
			}
			p = q + uintptr(2)
			has_space = uint8(1)
			continue
		}

		// Skip newline.
		if int32(*(*int8)(unsafe.Pointer(p))) == '\n' {
			p++
			at_bol = uint8(1)
			has_space = uint8(0)
			continue
		}

		// Skip whitespace characters.
		if int32(*(*uint16)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(libc.X__ctype_b_loc(tls))) + uintptr(int32(*(*int8)(unsafe.Pointer(p))))*2)))&int32(_ISspace) != 0 {
			p++
			has_space = uint8(1)
			continue
		}

		// Numeric literal
		if int32(*(*uint16)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(libc.X__ctype_b_loc(tls))) + uintptr(int32(*(*int8)(unsafe.Pointer(p))))*2)))&int32(_ISdigit) != 0 || int32(*(*int8)(unsafe.Pointer(p))) == '.' && int32(*(*uint16)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(libc.X__ctype_b_loc(tls))) + uintptr(int32(*(*int8)(unsafe.Pointer(p + 1))))*2)))&int32(_ISdigit) != 0 {
			var q uintptr = libc.PostIncUintptr(&p, 1)
			for {
				if *(*int8)(unsafe.Pointer(p)) != 0 && *(*int8)(unsafe.Pointer(p + 1)) != 0 && libc.Xstrchr(tls, ts+11884, int32(*(*int8)(unsafe.Pointer(p)))) != 0 && libc.Xstrchr(tls, ts+11889, int32(*(*int8)(unsafe.Pointer(p + 1)))) != 0 {
					p += uintptr(2)
				} else if int32(*(*uint16)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(libc.X__ctype_b_loc(tls))) + uintptr(int32(*(*int8)(unsafe.Pointer(p))))*2)))&int32(_ISalnum) != 0 || int32(*(*int8)(unsafe.Pointer(p))) == '.' {
					p++
				} else {
					break
				}
			}
			cur = libc.AssignPtrUintptr(cur+8, new_token(tls, TK_PP_NUM, q, p))
			continue
		}

		// String literal
		if int32(*(*int8)(unsafe.Pointer(p))) == '"' {
			cur = libc.AssignPtrUintptr(cur+8, read_string_literal(tls, p, p))
			p += uintptr((*Token)(unsafe.Pointer(cur)).len)
			continue
		}

		// UTF-8 string literal
		if startswith(tls, p, ts+11892) != 0 {
			cur = libc.AssignPtrUintptr(cur+8, read_string_literal(tls, p, p+uintptr(2)))
			p += uintptr((*Token)(unsafe.Pointer(cur)).len)
			continue
		}

		// UTF-16 string literal
		if startswith(tls, p, ts+11896) != 0 {
			cur = libc.AssignPtrUintptr(cur+8, read_utf16_string_literal(tls, p, p+uintptr(1)))
			p += uintptr((*Token)(unsafe.Pointer(cur)).len)
			continue
		}

		// Wide string literal
		if startswith(tls, p, ts+11899) != 0 {
			cur = libc.AssignPtrUintptr(cur+8, read_utf32_string_literal(tls, p, p+uintptr(1), ty_int))
			p += uintptr((*Token)(unsafe.Pointer(cur)).len)
			continue
		}

		// UTF-32 string literal
		if startswith(tls, p, ts+11902) != 0 {
			cur = libc.AssignPtrUintptr(cur+8, read_utf32_string_literal(tls, p, p+uintptr(1), ty_uint))
			p += uintptr((*Token)(unsafe.Pointer(cur)).len)
			continue
		}

		// Character literal
		if int32(*(*int8)(unsafe.Pointer(p))) == '\'' {
			cur = libc.AssignPtrUintptr(cur+8, read_char_literal(tls, p, p, ty_int))
			(*Token)(unsafe.Pointer(cur)).val = int64_t(int8((*Token)(unsafe.Pointer(cur)).val))
			p += uintptr((*Token)(unsafe.Pointer(cur)).len)
			continue
		}

		// UTF-16 character literal
		if startswith(tls, p, ts+11905) != 0 {
			cur = libc.AssignPtrUintptr(cur+8, read_char_literal(tls, p, p+uintptr(1), ty_ushort))
			*(*int64_t)(unsafe.Pointer(cur + 16)) &= int64(0xffff)
			p += uintptr((*Token)(unsafe.Pointer(cur)).len)
			continue
		}

		// Wide character literal
		if startswith(tls, p, ts+11908) != 0 {
			cur = libc.AssignPtrUintptr(cur+8, read_char_literal(tls, p, p+uintptr(1), ty_int))
			p += uintptr((*Token)(unsafe.Pointer(cur)).len)
			continue
		}

		// UTF-32 character literal
		if startswith(tls, p, ts+11911) != 0 {
			cur = libc.AssignPtrUintptr(cur+8, read_char_literal(tls, p, p+uintptr(1), ty_uint))
			p += uintptr((*Token)(unsafe.Pointer(cur)).len)
			continue
		}

		// Identifier or keyword
		var ident_len int32 = read_ident(tls, p)
		if ident_len != 0 {
			cur = libc.AssignPtrUintptr(cur+8, new_token(tls, TK_IDENT, p, p+uintptr(ident_len)))
			p += uintptr((*Token)(unsafe.Pointer(cur)).len)
			continue
		}

		// Punctuators
		var punct_len int32 = read_punct(tls, p)
		if punct_len != 0 {
			cur = libc.AssignPtrUintptr(cur+8, new_token(tls, TK_PUNCT, p, p+uintptr(punct_len)))
			p += uintptr((*Token)(unsafe.Pointer(cur)).len)
			continue
		}

		error_at(tls, p, ts+11914, 0)
	}

	cur = libc.AssignPtrUintptr(cur+8, new_token(tls, TK_EOF, p, p))
	add_line_numbers(tls, (*Token)(unsafe.Pointer(bp /* &head */)).next)
	return (*Token)(unsafe.Pointer(bp /* &head */)).next
}

// Returns the contents of a given file.
func read_file(tls *libc.TLS, path uintptr) uintptr { /* tokenize.c:640:13: */
	bp := tls.Alloc(4096)
	defer tls.Free(4096)

	var fp uintptr

	if libc.Xstrcmp(tls, path, ts+7420) == 0 {
		// By convention, read from stdin if a given filename is "-".
		fp = libc.Xstdin
	} else {
		fp = libc.Xfopen(tls, path, ts+11928)
		if !(fp != 0) {
			return uintptr(0)
		}
	}

	var buf uintptr = uintptr(0)
	var len size_t = uint64(0)
	var cap size_t = uint64(0)

	// Read the entire file.
	for {
		// var buf2 [4096]int8 at bp, 4096

		var n int32 = int32(libc.Xfread(tls, bp, uint64(1), uint64(unsafe.Sizeof([4096]int8{})), fp))
		if n == 0 {
			break
		}
		if len+size_t(n)+uint64(2) > cap {
			cap = func() uint64 {
				if cap*uint64(2) < len+size_t(n)+uint64(2) {
					return len + size_t(n) + uint64(2)
				}
				return cap * uint64(2)
			}()
			buf = libc.Xrealloc(tls, buf, cap)
		}
		libc.Xmemcpy(tls, buf+uintptr(len), bp, uint64(n))
		len = len + size_t(n)
	}

	if fp != libc.Xstdin {
		libc.Xfclose(tls, fp)
	}

	// Make sure that the last line is properly terminated with '\n'.
	if len == uint64(0) || int32(*(*int8)(unsafe.Pointer(buf + uintptr(len-uint64(1))))) != '\n' {
		if len+uint64(2) > cap {
			cap = func() uint64 {
				if cap*uint64(2) < len+uint64(2) {
					return len + uint64(2)
				}
				return cap * uint64(2)
			}()
			buf = libc.Xrealloc(tls, buf, cap)
		}
		*(*int8)(unsafe.Pointer(buf + uintptr(libc.PostIncUint64(&len, 1)))) = int8('\n')
	}
	*(*int8)(unsafe.Pointer(buf + uintptr(len))) = int8(0)
	return buf
}

func get_input_files(tls *libc.TLS) uintptr { /* tokenize.c:685:6: */
	return input_files
}

func new_file(tls *libc.TLS, name uintptr, file_no int32, contents uintptr) uintptr { /* tokenize.c:689:6: */
	var file uintptr = libc.Xcalloc(tls, uint64(1), uint64(unsafe.Sizeof(File{})))
	(*File)(unsafe.Pointer(file)).name = name
	(*File)(unsafe.Pointer(file)).display_name = name
	(*File)(unsafe.Pointer(file)).file_no = file_no
	(*File)(unsafe.Pointer(file)).contents = contents
	return file
}

// Replaces \r or \r\n with \n.
func canonicalize_newline(tls *libc.TLS, p uintptr) { /* tokenize.c:699:13: */
	var i int32 = 0
	var j int32 = 0

	for *(*int8)(unsafe.Pointer(p + uintptr(i))) != 0 {
		if int32(*(*int8)(unsafe.Pointer(p + uintptr(i)))) == '\r' && int32(*(*int8)(unsafe.Pointer(p + uintptr(i+1)))) == '\n' {
			i = i + 2
			*(*int8)(unsafe.Pointer(p + uintptr(libc.PostIncInt32(&j, 1)))) = int8('\n')
		} else if int32(*(*int8)(unsafe.Pointer(p + uintptr(i)))) == '\r' {
			i++
			*(*int8)(unsafe.Pointer(p + uintptr(libc.PostIncInt32(&j, 1)))) = int8('\n')
		} else {
			*(*int8)(unsafe.Pointer(p + uintptr(libc.PostIncInt32(&j, 1)))) = *(*int8)(unsafe.Pointer(p + uintptr(libc.PostIncInt32(&i, 1))))
		}
	}

	*(*int8)(unsafe.Pointer(p + uintptr(j))) = int8(0)
}

// Removes backslashes followed by a newline.
func remove_backslash_newline(tls *libc.TLS, p uintptr) { /* tokenize.c:718:13: */
	var i int32 = 0
	var j int32 = 0

	// We want to keep the number of newline characters so that
	// the logical line number matches the physical one.
	// This counter maintain the number of newlines we have removed.
	var n int32 = 0

	for *(*int8)(unsafe.Pointer(p + uintptr(i))) != 0 {
		if int32(*(*int8)(unsafe.Pointer(p + uintptr(i)))) == '\\' && int32(*(*int8)(unsafe.Pointer(p + uintptr(i+1)))) == '\n' {
			i = i + 2
			n++
		} else if int32(*(*int8)(unsafe.Pointer(p + uintptr(i)))) == '\n' {
			*(*int8)(unsafe.Pointer(p + uintptr(libc.PostIncInt32(&j, 1)))) = *(*int8)(unsafe.Pointer(p + uintptr(libc.PostIncInt32(&i, 1))))
			for ; n > 0; n-- {
				*(*int8)(unsafe.Pointer(p + uintptr(libc.PostIncInt32(&j, 1)))) = int8('\n')
			}
		} else {
			*(*int8)(unsafe.Pointer(p + uintptr(libc.PostIncInt32(&j, 1)))) = *(*int8)(unsafe.Pointer(p + uintptr(libc.PostIncInt32(&i, 1))))
		}
	}

	for ; n > 0; n-- {
		*(*int8)(unsafe.Pointer(p + uintptr(libc.PostIncInt32(&j, 1)))) = int8('\n')
	}
	*(*int8)(unsafe.Pointer(p + uintptr(j))) = int8(0)
}

func read_universal_char(tls *libc.TLS, p uintptr, len int32) uint32_t { /* tokenize.c:744:17: */
	var c uint32_t = uint32_t(0)
	{
		var i int32 = 0
		for ; i < len; i++ {
			if !(int32(*(*uint16)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(libc.X__ctype_b_loc(tls))) + uintptr(int32(*(*int8)(unsafe.Pointer(p + uintptr(i)))))*2)))&int32(_ISxdigit) != 0) {
				return uint32_t(0)
			}
			c = c<<4 | uint32_t(from_hex(tls, *(*int8)(unsafe.Pointer(p + uintptr(i)))))
		}
	}
	return c
}

// Replace \u or \U escape sequences with corresponding UTF-8 bytes.
func convert_universal_chars(tls *libc.TLS, p uintptr) { /* tokenize.c:755:13: */
	var q uintptr = p

	for *(*int8)(unsafe.Pointer(p)) != 0 {
		if startswith(tls, p, ts+11930) != 0 {
			var c uint32_t = read_universal_char(tls, p+uintptr(2), 4)
			if c != 0 {
				p += uintptr(6)
				q += uintptr(encode_utf8(tls, q, c))
			} else {
				*(*int8)(unsafe.Pointer(libc.PostIncUintptr(&q, 1))) = *(*int8)(unsafe.Pointer(libc.PostIncUintptr(&p, 1)))
			}
		} else if startswith(tls, p, ts+11933) != 0 {
			var c uint32_t = read_universal_char(tls, p+uintptr(2), 8)
			if c != 0 {
				p += uintptr(10)
				q += uintptr(encode_utf8(tls, q, c))
			} else {
				*(*int8)(unsafe.Pointer(libc.PostIncUintptr(&q, 1))) = *(*int8)(unsafe.Pointer(libc.PostIncUintptr(&p, 1)))
			}
		} else if int32(*(*int8)(unsafe.Pointer(p))) == '\\' {
			*(*int8)(unsafe.Pointer(libc.PostIncUintptr(&q, 1))) = *(*int8)(unsafe.Pointer(libc.PostIncUintptr(&p, 1)))
			*(*int8)(unsafe.Pointer(libc.PostIncUintptr(&q, 1))) = *(*int8)(unsafe.Pointer(libc.PostIncUintptr(&p, 1)))
		} else {
			*(*int8)(unsafe.Pointer(libc.PostIncUintptr(&q, 1))) = *(*int8)(unsafe.Pointer(libc.PostIncUintptr(&p, 1)))
		}
	}

	*(*int8)(unsafe.Pointer(q)) = int8(0)
}

func tokenize_file(tls *libc.TLS, path uintptr) uintptr { /* tokenize.c:786:7: */
	var p uintptr = read_file(tls, path)
	if !(p != 0) {
		return uintptr(0)
	}

	// UTF-8 texts may start with a 3-byte "BOM" marker sequence.
	// If exists, just skip them because they are useless bytes.
	// (It is actually not recommended to add BOM markers to UTF-8
	// texts, but it's not uncommon particularly on Windows.)
	if !(libc.Xmemcmp(tls, p, ts+11936, uint64(3)) != 0) {
		p += uintptr(3)
	}

	canonicalize_newline(tls, p)
	remove_backslash_newline(tls, p)
	convert_universal_chars(tls, p)
	var file uintptr = new_file(tls, path, file_no+1, p)

	// Save the filename for assembler .file directive.
	input_files = libc.Xrealloc(tls, input_files, uint64(unsafe.Sizeof(uintptr(0)))*uint64(file_no+2))
	*(*uintptr)(unsafe.Pointer(input_files + uintptr(file_no)*8)) = file
	*(*uintptr)(unsafe.Pointer(input_files + uintptr(file_no+1)*8)) = uintptr(0)
	file_no++

	return tokenize(tls, file)
}

var file_no int32 /* tokenize.c:803:14: */

var ty_void_storage = Type{size: 1, align: 1}                /* type.c:3:13 */
var ty_bool_storage = Type{kind: TY_BOOL, size: 1, align: 1} /* type.c:4:13 */

var ty_char_storage = Type{kind: TY_CHAR, size: 1, align: 1}   /* type.c:6:13 */
var ty_short_storage = Type{kind: TY_SHORT, size: 2, align: 2} /* type.c:7:13 */
var ty_int_storage = Type{kind: TY_INT, size: 4, align: 4}     /* type.c:8:13 */
var ty_long_storage = Type{kind: TY_LONG, size: 8, align: 8}   /* type.c:9:13 */

var ty_uchar_storage = Type{kind: TY_CHAR, size: 1, align: 1, is_unsigned: uint8(1)}   /* type.c:11:13 */
var ty_ushort_storage = Type{kind: TY_SHORT, size: 2, align: 2, is_unsigned: uint8(1)} /* type.c:12:13 */
var ty_uint_storage = Type{kind: TY_INT, size: 4, align: 4, is_unsigned: uint8(1)}     /* type.c:13:13 */
var ty_ulong_storage = Type{kind: TY_LONG, size: 8, align: 8, is_unsigned: uint8(1)}   /* type.c:14:13 */

var ty_float_storage = Type{kind: TY_FLOAT, size: 4, align: 4}       /* type.c:16:13 */
var ty_double_storage = Type{kind: TY_DOUBLE, size: 8, align: 8}     /* type.c:17:13 */
var ty_ldouble_storage = Type{kind: TY_LDOUBLE, size: 16, align: 16} /* type.c:18:13 */

var ty_void uintptr = 0 /* type.c:20:6 */
var ty_bool uintptr = 0 /* type.c:21:6 */

var ty_char uintptr = 0  /* type.c:23:6 */
var ty_short uintptr = 0 /* type.c:24:6 */
var ty_int uintptr = 0   /* type.c:25:6 */
var ty_long uintptr = 0  /* type.c:26:6 */

var ty_uchar uintptr = 0  /* type.c:28:6 */
var ty_ushort uintptr = 0 /* type.c:29:6 */
var ty_uint uintptr = 0   /* type.c:30:6 */
var ty_ulong uintptr = 0  /* type.c:31:6 */

var ty_float uintptr = 0   /* type.c:33:6 */
var ty_double uintptr = 0  /* type.c:34:6 */
var ty_ldouble uintptr = 0 /* type.c:35:6 */

func new_type(tls *libc.TLS, kind TypeKind, size int32, align int32) uintptr { /* type.c:37:13: */
	var ty uintptr = libc.Xcalloc(tls, uint64(1), uint64(unsafe.Sizeof(Type{})))
	(*Type)(unsafe.Pointer(ty)).kind = kind
	(*Type)(unsafe.Pointer(ty)).size = size
	(*Type)(unsafe.Pointer(ty)).align = align
	return ty
}

func is_integer(tls *libc.TLS, ty uintptr) uint8 { /* type.c:45:6: */
	var k TypeKind = (*Type)(unsafe.Pointer(ty)).kind
	return uint8(libc.Bool32(k == TY_BOOL || k == TY_CHAR || k == TY_SHORT || k == TY_INT || k == TY_LONG || k == TY_ENUM))
}

func is_flonum(tls *libc.TLS, ty uintptr) uint8 { /* type.c:51:6: */
	return uint8(libc.Bool32((*Type)(unsafe.Pointer(ty)).kind == TY_FLOAT || (*Type)(unsafe.Pointer(ty)).kind == TY_DOUBLE || (*Type)(unsafe.Pointer(ty)).kind == TY_LDOUBLE))
}

func is_numeric(tls *libc.TLS, ty uintptr) uint8 { /* type.c:56:6: */
	return uint8(libc.Bool32(is_integer(tls, ty) != 0 || is_flonum(tls, ty) != 0))
}

func is_compatible(tls *libc.TLS, t1 uintptr, t2 uintptr) uint8 { /* type.c:60:6: */
	if t1 == t2 {
		return uint8(1)
	}

	if (*Type)(unsafe.Pointer(t1)).origin != 0 {
		return is_compatible(tls, (*Type)(unsafe.Pointer(t1)).origin, t2)
	}

	if (*Type)(unsafe.Pointer(t2)).origin != 0 {
		return is_compatible(tls, t1, (*Type)(unsafe.Pointer(t2)).origin)
	}

	if (*Type)(unsafe.Pointer(t1)).kind != (*Type)(unsafe.Pointer(t2)).kind {
		return uint8(0)
	}

	switch (*Type)(unsafe.Pointer(t1)).kind {
	case TY_CHAR:
		fallthrough
	case TY_SHORT:
		fallthrough
	case TY_INT:
		fallthrough
	case TY_LONG:
		return uint8(libc.Bool32((*Type)(unsafe.Pointer(t1)).is_unsigned == (*Type)(unsafe.Pointer(t2)).is_unsigned))
	case TY_FLOAT:
		fallthrough
	case TY_DOUBLE:
		fallthrough
	case TY_LDOUBLE:
		return uint8(1)
	case TY_PTR:
		return is_compatible(tls, (*Type)(unsafe.Pointer(t1)).base, (*Type)(unsafe.Pointer(t2)).base)
	case TY_FUNC:
		{
			if !(is_compatible(tls, (*Type)(unsafe.Pointer(t1)).return_ty, (*Type)(unsafe.Pointer(t2)).return_ty) != 0) {
				return uint8(0)
			}
			if (*Type)(unsafe.Pointer(t1)).is_variadic != (*Type)(unsafe.Pointer(t2)).is_variadic {
				return uint8(0)
			}

			var p1 uintptr = (*Type)(unsafe.Pointer(t1)).params
			var p2 uintptr = (*Type)(unsafe.Pointer(t2)).params
		__1:
			if !(p1 != 0 && p2 != 0) {
				goto __3
			}
			if !(is_compatible(tls, p1, p2) != 0) {
				return uint8(0)
			}
			goto __2
		__2:
			p1 = (*Type)(unsafe.Pointer(p1)).next
			p2 = (*Type)(unsafe.Pointer(p2)).next
			goto __1
			goto __3
		__3:
			;
			return uint8(libc.Bool32(p1 == uintptr(0) && p2 == uintptr(0)))

		}
	case TY_ARRAY:
		if !(is_compatible(tls, (*Type)(unsafe.Pointer(t1)).base, (*Type)(unsafe.Pointer(t2)).base) != 0) {
			return uint8(0)
		}
		return uint8(libc.Bool32((*Type)(unsafe.Pointer(t1)).array_len < 0 && (*Type)(unsafe.Pointer(t2)).array_len < 0 && (*Type)(unsafe.Pointer(t1)).array_len == (*Type)(unsafe.Pointer(t2)).array_len))
	}
	return uint8(0)
}

func copy_type(tls *libc.TLS, ty uintptr) uintptr { /* type.c:107:6: */
	var ret uintptr = libc.Xcalloc(tls, uint64(1), uint64(unsafe.Sizeof(Type{})))
	*(*Type)(unsafe.Pointer(ret)) = *(*Type)(unsafe.Pointer(ty))
	(*Type)(unsafe.Pointer(ret)).origin = ty
	return ret
}

func pointer_to(tls *libc.TLS, base uintptr) uintptr { /* type.c:114:6: */
	var ty uintptr = new_type(tls, TY_PTR, 8, 8)
	(*Type)(unsafe.Pointer(ty)).base = base
	(*Type)(unsafe.Pointer(ty)).is_unsigned = uint8(1)
	return ty
}

func func_type(tls *libc.TLS, return_ty uintptr) uintptr { /* type.c:121:6: */
	// The C spec disallows sizeof(<function type>), but
	// GCC allows that and the expression is evaluated to 1.
	var ty uintptr = new_type(tls, TY_FUNC, 1, 1)
	(*Type)(unsafe.Pointer(ty)).return_ty = return_ty
	return ty
}

func array_of(tls *libc.TLS, base uintptr, len int32) uintptr { /* type.c:129:6: */
	var ty uintptr = new_type(tls, TY_ARRAY, (*Type)(unsafe.Pointer(base)).size*len, (*Type)(unsafe.Pointer(base)).align)
	(*Type)(unsafe.Pointer(ty)).base = base
	(*Type)(unsafe.Pointer(ty)).array_len = len
	return ty
}

func vla_of(tls *libc.TLS, base uintptr, len uintptr) uintptr { /* type.c:136:6: */
	var ty uintptr = new_type(tls, TY_VLA, 8, 8)
	(*Type)(unsafe.Pointer(ty)).base = base
	(*Type)(unsafe.Pointer(ty)).vla_len = len
	return ty
}

func enum_type(tls *libc.TLS) uintptr { /* type.c:143:6: */
	return new_type(tls, TY_ENUM, 4, 4)
}

func struct_type(tls *libc.TLS) uintptr { /* type.c:147:6: */
	return new_type(tls, TY_STRUCT, 0, 1)
}

func get_common_type(tls *libc.TLS, ty1 uintptr, ty2 uintptr) uintptr { /* type.c:151:13: */
	if (*Type)(unsafe.Pointer(ty1)).base != 0 {
		return pointer_to(tls, (*Type)(unsafe.Pointer(ty1)).base)
	}

	if (*Type)(unsafe.Pointer(ty1)).kind == TY_FUNC {
		return pointer_to(tls, ty1)
	}
	if (*Type)(unsafe.Pointer(ty2)).kind == TY_FUNC {
		return pointer_to(tls, ty2)
	}

	if (*Type)(unsafe.Pointer(ty1)).kind == TY_LDOUBLE || (*Type)(unsafe.Pointer(ty2)).kind == TY_LDOUBLE {
		return ty_ldouble
	}
	if (*Type)(unsafe.Pointer(ty1)).kind == TY_DOUBLE || (*Type)(unsafe.Pointer(ty2)).kind == TY_DOUBLE {
		return ty_double
	}
	if (*Type)(unsafe.Pointer(ty1)).kind == TY_FLOAT || (*Type)(unsafe.Pointer(ty2)).kind == TY_FLOAT {
		return ty_float
	}

	if (*Type)(unsafe.Pointer(ty1)).size < 4 {
		ty1 = ty_int
	}
	if (*Type)(unsafe.Pointer(ty2)).size < 4 {
		ty2 = ty_int
	}

	if (*Type)(unsafe.Pointer(ty1)).size != (*Type)(unsafe.Pointer(ty2)).size {
		if (*Type)(unsafe.Pointer(ty1)).size < (*Type)(unsafe.Pointer(ty2)).size {
			return ty2
		}
		return ty1
	}

	if (*Type)(unsafe.Pointer(ty2)).is_unsigned != 0 {
		return ty2
	}
	return ty1
}

// For many binary operators, we implicitly promote operands so that
// both operands have the same type. Any integral type smaller than
// int is always promoted to int. If the type of one operand is larger
// than the other's (e.g. "long" vs. "int"), the smaller operand will
// be promoted to match with the other.
//
// This operation is called the "usual arithmetic conversion".
func usual_arith_conv(tls *libc.TLS, lhs uintptr, rhs uintptr) { /* type.c:187:13: */
	var ty uintptr = get_common_type(tls, (*Node)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(lhs)))).ty, (*Node)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(rhs)))).ty)
	*(*uintptr)(unsafe.Pointer(lhs)) = new_cast(tls, *(*uintptr)(unsafe.Pointer(lhs)), ty)
	*(*uintptr)(unsafe.Pointer(rhs)) = new_cast(tls, *(*uintptr)(unsafe.Pointer(rhs)), ty)
}

func add_type(tls *libc.TLS, node uintptr) { /* type.c:193:6: */
	if !(node != 0) || (*Node)(unsafe.Pointer(node)).ty != 0 {
		return
	}

	add_type(tls, (*Node)(unsafe.Pointer(node)).lhs)
	add_type(tls, (*Node)(unsafe.Pointer(node)).rhs)
	add_type(tls, (*Node)(unsafe.Pointer(node)).cond)
	add_type(tls, (*Node)(unsafe.Pointer(node)).then)
	add_type(tls, (*Node)(unsafe.Pointer(node)).els)
	add_type(tls, (*Node)(unsafe.Pointer(node)).init)
	add_type(tls, (*Node)(unsafe.Pointer(node)).inc)

	{
		var n uintptr = (*Node)(unsafe.Pointer(node)).body
		for ; n != 0; n = (*Node)(unsafe.Pointer(n)).next {
			add_type(tls, n)
		}
	}
	{
		var n1 uintptr = (*Node)(unsafe.Pointer(node)).args
		for ; n1 != 0; n1 = (*Node)(unsafe.Pointer(n1)).next {
			add_type(tls, n1)
		}
	}

	switch (*Node)(unsafe.Pointer(node)).kind {
	case ND_NUM:
		(*Node)(unsafe.Pointer(node)).ty = ty_int
		return
	case ND_ADD:
		fallthrough
	case ND_SUB:
		fallthrough
	case ND_MUL:
		fallthrough
	case ND_DIV:
		fallthrough
	case ND_MOD:
		fallthrough
	case ND_BITAND:
		fallthrough
	case ND_BITOR:
		fallthrough
	case ND_BITXOR:
		usual_arith_conv(tls, node+32, node+40)
		(*Node)(unsafe.Pointer(node)).ty = (*Node)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).lhs)).ty
		return
	case ND_NEG:
		{
			var ty uintptr = get_common_type(tls, ty_int, (*Node)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).lhs)).ty)
			(*Node)(unsafe.Pointer(node)).lhs = new_cast(tls, (*Node)(unsafe.Pointer(node)).lhs, ty)
			(*Node)(unsafe.Pointer(node)).ty = ty
			return

		}
	case ND_ASSIGN:
		if (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).lhs)).ty)).kind == TY_ARRAY {
			error_tok(tls, (*Node)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).lhs)).tok, ts+516, 0)
		}
		if (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).lhs)).ty)).kind != TY_STRUCT {
			(*Node)(unsafe.Pointer(node)).rhs = new_cast(tls, (*Node)(unsafe.Pointer(node)).rhs, (*Node)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).lhs)).ty)
		}
		(*Node)(unsafe.Pointer(node)).ty = (*Node)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).lhs)).ty
		return
	case ND_EQ:
		fallthrough
	case ND_NE:
		fallthrough
	case ND_LT:
		fallthrough
	case ND_LE:
		usual_arith_conv(tls, node+32, node+40)
		(*Node)(unsafe.Pointer(node)).ty = ty_int
		return
	case ND_FUNCALL:
		(*Node)(unsafe.Pointer(node)).ty = (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).func_ty)).return_ty
		return
	case ND_NOT:
		fallthrough
	case ND_LOGOR:
		fallthrough
	case ND_LOGAND:
		(*Node)(unsafe.Pointer(node)).ty = ty_int
		return
	case ND_BITNOT:
		fallthrough
	case ND_SHL:
		fallthrough
	case ND_SHR:
		(*Node)(unsafe.Pointer(node)).ty = (*Node)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).lhs)).ty
		return
	case ND_VAR:
		fallthrough
	case ND_VLA_PTR:
		(*Node)(unsafe.Pointer(node)).ty = (*Obj)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).__var)).ty
		return
	case ND_COND:
		if (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).then)).ty)).kind == TY_VOID || (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).els)).ty)).kind == TY_VOID {
			(*Node)(unsafe.Pointer(node)).ty = ty_void
		} else {
			usual_arith_conv(tls, node+56, node+64)
			(*Node)(unsafe.Pointer(node)).ty = (*Node)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).then)).ty
		}
		return
	case ND_COMMA:
		(*Node)(unsafe.Pointer(node)).ty = (*Node)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).rhs)).ty
		return
	case ND_MEMBER:
		(*Node)(unsafe.Pointer(node)).ty = (*Member)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).member)).ty
		return
	case ND_ADDR:
		{
			var ty uintptr = (*Node)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).lhs)).ty
			if (*Type)(unsafe.Pointer(ty)).kind == TY_ARRAY {
				(*Node)(unsafe.Pointer(node)).ty = pointer_to(tls, (*Type)(unsafe.Pointer(ty)).base)
			} else {
				(*Node)(unsafe.Pointer(node)).ty = pointer_to(tls, ty)
			}
			return

		}
	case ND_DEREF:
		if !(int32((*Type)(unsafe.Pointer((*Node)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).lhs)).ty)).base) != 0) {
			error_tok(tls, (*Node)(unsafe.Pointer(node)).tok, ts+11940, 0)
		}
		if (*Type)(unsafe.Pointer((*Type)(unsafe.Pointer((*Node)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).lhs)).ty)).base)).kind == TY_VOID {
			error_tok(tls, (*Node)(unsafe.Pointer(node)).tok, ts+11968, 0)
		}

		(*Node)(unsafe.Pointer(node)).ty = (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).lhs)).ty)).base
		return
	case ND_STMT_EXPR:
		if (*Node)(unsafe.Pointer(node)).body != 0 {
			var stmt uintptr = (*Node)(unsafe.Pointer(node)).body
			for (*Node)(unsafe.Pointer(stmt)).next != 0 {
				stmt = (*Node)(unsafe.Pointer(stmt)).next
			}
			if (*Node)(unsafe.Pointer(stmt)).kind == ND_EXPR_STMT {
				(*Node)(unsafe.Pointer(node)).ty = (*Node)(unsafe.Pointer((*Node)(unsafe.Pointer(stmt)).lhs)).ty
				return
			}
		}
		error_tok(tls, (*Node)(unsafe.Pointer(node)).tok, ts+11997, 0)
		return
	case ND_LABEL_VAL:
		(*Node)(unsafe.Pointer(node)).ty = pointer_to(tls, ty_void)
		return
	case ND_CAS:
		add_type(tls, (*Node)(unsafe.Pointer(node)).cas_addr)
		add_type(tls, (*Node)(unsafe.Pointer(node)).cas_old)
		add_type(tls, (*Node)(unsafe.Pointer(node)).cas_new)
		(*Node)(unsafe.Pointer(node)).ty = ty_bool

		if (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).cas_addr)).ty)).kind != TY_PTR {
			error_tok(tls, (*Node)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).cas_addr)).tok, ts+12050, 0)
		}
		if (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).cas_old)).ty)).kind != TY_PTR {
			error_tok(tls, (*Node)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).cas_old)).tok, ts+12050, 0)
		}
		return
	case ND_EXCH:
		if (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).lhs)).ty)).kind != TY_PTR {
			error_tok(tls, (*Node)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).cas_addr)).tok, ts+12050, 0)
		}
		(*Node)(unsafe.Pointer(node)).ty = (*Type)(unsafe.Pointer((*Node)(unsafe.Pointer((*Node)(unsafe.Pointer(node)).lhs)).ty)).base
		return
	}
}

// Encode a given character in UTF-8.
func encode_utf8(tls *libc.TLS, buf uintptr, c uint32_t) int32 { /* unicode.c:4:5: */
	if c <= uint32_t(0x7F) {
		*(*int8)(unsafe.Pointer(buf)) = int8(c)
		return 1
	}

	if c <= uint32_t(0x7FF) {
		*(*int8)(unsafe.Pointer(buf)) = int8(uint32_t(0b11000000) | c>>6)
		*(*int8)(unsafe.Pointer(buf + 1)) = int8(uint32_t(0b10000000) | c&uint32_t(0b00111111))
		return 2
	}

	if c <= uint32_t(0xFFFF) {
		*(*int8)(unsafe.Pointer(buf)) = int8(uint32_t(0b11100000) | c>>12)
		*(*int8)(unsafe.Pointer(buf + 1)) = int8(uint32_t(0b10000000) | c>>6&uint32_t(0b00111111))
		*(*int8)(unsafe.Pointer(buf + 2)) = int8(uint32_t(0b10000000) | c&uint32_t(0b00111111))
		return 3
	}

	*(*int8)(unsafe.Pointer(buf)) = int8(uint32_t(0b11110000) | c>>18)
	*(*int8)(unsafe.Pointer(buf + 1)) = int8(uint32_t(0b10000000) | c>>12&uint32_t(0b00111111))
	*(*int8)(unsafe.Pointer(buf + 2)) = int8(uint32_t(0b10000000) | c>>6&uint32_t(0b00111111))
	*(*int8)(unsafe.Pointer(buf + 3)) = int8(uint32_t(0b10000000) | c&uint32_t(0b00111111))
	return 4
}

// Read a UTF-8-encoded Unicode code point from a source file.
// We assume that source files are always in UTF-8.
//
// UTF-8 is a variable-width encoding in which one code point is
// encoded in one to four bytes. One byte UTF-8 code points are
// identical to ASCII. Non-ASCII characters are encoded using more
// than one byte.
func decode_utf8(tls *libc.TLS, new_pos uintptr, p uintptr) uint32_t { /* unicode.c:37:10: */
	if int32(uint8(*(*int8)(unsafe.Pointer(p)))) < 128 {
		*(*uintptr)(unsafe.Pointer(new_pos)) = p + uintptr(1)
		return uint32_t(*(*int8)(unsafe.Pointer(p)))
	}

	var start uintptr = p
	var len int32
	var c uint32_t

	if int32(uint8(*(*int8)(unsafe.Pointer(p)))) >= 0b11110000 {
		len = 4
		c = uint32_t(int32(*(*int8)(unsafe.Pointer(p))) & 0b111)
	} else if int32(uint8(*(*int8)(unsafe.Pointer(p)))) >= 0b11100000 {
		len = 3
		c = uint32_t(int32(*(*int8)(unsafe.Pointer(p))) & 0b1111)
	} else if int32(uint8(*(*int8)(unsafe.Pointer(p)))) >= 0b11000000 {
		len = 2
		c = uint32_t(int32(*(*int8)(unsafe.Pointer(p))) & 0b11111)
	} else {
		error_at(tls, start, ts+12067, 0)
	}

	{
		var i int32 = 1
		for ; i < len; i++ {
			if int32(uint8(*(*int8)(unsafe.Pointer(p + uintptr(i)))))>>6 != 0b10 {
				error_at(tls, start, ts+12067, 0)
			}
			c = c<<6 | uint32_t(int32(*(*int8)(unsafe.Pointer(p + uintptr(i))))&0b111111)
		}
	}

	*(*uintptr)(unsafe.Pointer(new_pos)) = p + uintptr(len)
	return c
}

func in_range(tls *libc.TLS, range1 uintptr, c uint32_t) uint8 { /* unicode.c:70:13: */
	{
		var i int32 = 0
		for ; *(*uint32_t)(unsafe.Pointer(range1 + uintptr(i)*4)) != libc.Uint32FromInt32(-1); i = i + 2 {
			if *(*uint32_t)(unsafe.Pointer(range1 + uintptr(i)*4)) <= c && c <= *(*uint32_t)(unsafe.Pointer(range1 + uintptr(i+1)*4)) {
				return uint8(1)
			}
		}
	}
	return uint8(0)
}

// [https://www.sigbus.info/n1570#D] C11 allows not only ASCII but
// some multibyte characters in certan Unicode ranges to be used in an
// identifier.
//
// This function returns true if a given character is acceptable as
// the first character of an identifier.
//
// For example, ¾ (U+00BE) is a valid identifier because characters in
// 0x00BE-0x00C0 are allowed, while neither ⟘ (U+27D8) nor '　'
// (U+3000, full-width space) are allowed because they are out of range.
func is_ident1(tls *libc.TLS, c uint32_t) uint8 { /* unicode.c:87:6: */

	return in_range(tls, uintptr(unsafe.Pointer(&range1)), c)
}

var range1 = [107]uint32_t{
	uint32_t('_'), uint32_t('_'), uint32_t('a'), uint32_t('z'), uint32_t('A'), uint32_t('Z'), uint32_t('$'), uint32_t('$'),
	uint32_t(0x00A8), uint32_t(0x00A8), uint32_t(0x00AA), uint32_t(0x00AA), uint32_t(0x00AD), uint32_t(0x00AD), uint32_t(0x00AF), uint32_t(0x00AF),
	uint32_t(0x00B2), uint32_t(0x00B5), uint32_t(0x00B7), uint32_t(0x00BA), uint32_t(0x00BC), uint32_t(0x00BE), uint32_t(0x00C0), uint32_t(0x00D6),
	uint32_t(0x00D8), uint32_t(0x00F6), uint32_t(0x00F8), uint32_t(0x00FF), uint32_t(0x0100), uint32_t(0x02FF), uint32_t(0x0370), uint32_t(0x167F),
	uint32_t(0x1681), uint32_t(0x180D), uint32_t(0x180F), uint32_t(0x1DBF), uint32_t(0x1E00), uint32_t(0x1FFF), uint32_t(0x200B), uint32_t(0x200D),
	uint32_t(0x202A), uint32_t(0x202E), uint32_t(0x203F), uint32_t(0x2040), uint32_t(0x2054), uint32_t(0x2054), uint32_t(0x2060), uint32_t(0x206F),
	uint32_t(0x2070), uint32_t(0x20CF), uint32_t(0x2100), uint32_t(0x218F), uint32_t(0x2460), uint32_t(0x24FF), uint32_t(0x2776), uint32_t(0x2793),
	uint32_t(0x2C00), uint32_t(0x2DFF), uint32_t(0x2E80), uint32_t(0x2FFF), uint32_t(0x3004), uint32_t(0x3007), uint32_t(0x3021), uint32_t(0x302F),
	uint32_t(0x3031), uint32_t(0x303F), uint32_t(0x3040), uint32_t(0xD7FF), uint32_t(0xF900), uint32_t(0xFD3D), uint32_t(0xFD40), uint32_t(0xFDCF),
	uint32_t(0xFDF0), uint32_t(0xFE1F), uint32_t(0xFE30), uint32_t(0xFE44), uint32_t(0xFE47), uint32_t(0xFFFD),
	uint32_t(0x10000), uint32_t(0x1FFFD), uint32_t(0x20000), uint32_t(0x2FFFD), uint32_t(0x30000), uint32_t(0x3FFFD), uint32_t(0x40000), uint32_t(0x4FFFD),
	uint32_t(0x50000), uint32_t(0x5FFFD), uint32_t(0x60000), uint32_t(0x6FFFD), uint32_t(0x70000), uint32_t(0x7FFFD), uint32_t(0x80000), uint32_t(0x8FFFD),
	uint32_t(0x90000), uint32_t(0x9FFFD), uint32_t(0xA0000), uint32_t(0xAFFFD), uint32_t(0xB0000), uint32_t(0xBFFFD), uint32_t(0xC0000), uint32_t(0xCFFFD),
	uint32_t(0xD0000), uint32_t(0xDFFFD), uint32_t(0xE0000), uint32_t(0xEFFFD), libc.Uint32FromInt32(-1),
} /* unicode.c:88:19 */

// Returns true if a given character is acceptable as a non-first
// character of an identifier.
func is_ident2(tls *libc.TLS, c uint32_t) uint8 { /* unicode.c:110:6: */

	return uint8(libc.Bool32(is_ident1(tls, c) != 0 || in_range(tls, uintptr(unsafe.Pointer(&range2)), c) != 0))
}

var range2 = [13]uint32_t{
	uint32_t('0'), uint32_t('9'), uint32_t('$'), uint32_t('$'), uint32_t(0x0300), uint32_t(0x036F), uint32_t(0x1DC0), uint32_t(0x1DFF), uint32_t(0x20D0), uint32_t(0x20FF),
	uint32_t(0xFE20), uint32_t(0xFE2F), libc.Uint32FromInt32(-1),
} /* unicode.c:111:19 */

// Returns the number of columns needed to display a given
// character in a fixed-width font.
//
// Based on https://www.cl.cam.ac.uk/~mgk25/ucs/wcwidth.c
func char_width(tls *libc.TLS, c uint32_t) int32 { /* unicode.c:123:12: */

	if in_range(tls, uintptr(unsafe.Pointer(&range11)), c) != 0 {
		return 0
	}

	if in_range(tls, uintptr(unsafe.Pointer(&range21)), c) != 0 {
		return 2
	}
	return 1
}

var range11 = [289]uint32_t{
	uint32_t(0x0000), uint32_t(0x001F), uint32_t(0x007f), uint32_t(0x00a0), uint32_t(0x0300), uint32_t(0x036F), uint32_t(0x0483), uint32_t(0x0486),
	uint32_t(0x0488), uint32_t(0x0489), uint32_t(0x0591), uint32_t(0x05BD), uint32_t(0x05BF), uint32_t(0x05BF), uint32_t(0x05C1), uint32_t(0x05C2),
	uint32_t(0x05C4), uint32_t(0x05C5), uint32_t(0x05C7), uint32_t(0x05C7), uint32_t(0x0600), uint32_t(0x0603), uint32_t(0x0610), uint32_t(0x0615),
	uint32_t(0x064B), uint32_t(0x065E), uint32_t(0x0670), uint32_t(0x0670), uint32_t(0x06D6), uint32_t(0x06E4), uint32_t(0x06E7), uint32_t(0x06E8),
	uint32_t(0x06EA), uint32_t(0x06ED), uint32_t(0x070F), uint32_t(0x070F), uint32_t(0x0711), uint32_t(0x0711), uint32_t(0x0730), uint32_t(0x074A),
	uint32_t(0x07A6), uint32_t(0x07B0), uint32_t(0x07EB), uint32_t(0x07F3), uint32_t(0x0901), uint32_t(0x0902), uint32_t(0x093C), uint32_t(0x093C),
	uint32_t(0x0941), uint32_t(0x0948), uint32_t(0x094D), uint32_t(0x094D), uint32_t(0x0951), uint32_t(0x0954), uint32_t(0x0962), uint32_t(0x0963),
	uint32_t(0x0981), uint32_t(0x0981), uint32_t(0x09BC), uint32_t(0x09BC), uint32_t(0x09C1), uint32_t(0x09C4), uint32_t(0x09CD), uint32_t(0x09CD),
	uint32_t(0x09E2), uint32_t(0x09E3), uint32_t(0x0A01), uint32_t(0x0A02), uint32_t(0x0A3C), uint32_t(0x0A3C), uint32_t(0x0A41), uint32_t(0x0A42),
	uint32_t(0x0A47), uint32_t(0x0A48), uint32_t(0x0A4B), uint32_t(0x0A4D), uint32_t(0x0A70), uint32_t(0x0A71), uint32_t(0x0A81), uint32_t(0x0A82),
	uint32_t(0x0ABC), uint32_t(0x0ABC), uint32_t(0x0AC1), uint32_t(0x0AC5), uint32_t(0x0AC7), uint32_t(0x0AC8), uint32_t(0x0ACD), uint32_t(0x0ACD),
	uint32_t(0x0AE2), uint32_t(0x0AE3), uint32_t(0x0B01), uint32_t(0x0B01), uint32_t(0x0B3C), uint32_t(0x0B3C), uint32_t(0x0B3F), uint32_t(0x0B3F),
	uint32_t(0x0B41), uint32_t(0x0B43), uint32_t(0x0B4D), uint32_t(0x0B4D), uint32_t(0x0B56), uint32_t(0x0B56), uint32_t(0x0B82), uint32_t(0x0B82),
	uint32_t(0x0BC0), uint32_t(0x0BC0), uint32_t(0x0BCD), uint32_t(0x0BCD), uint32_t(0x0C3E), uint32_t(0x0C40), uint32_t(0x0C46), uint32_t(0x0C48),
	uint32_t(0x0C4A), uint32_t(0x0C4D), uint32_t(0x0C55), uint32_t(0x0C56), uint32_t(0x0CBC), uint32_t(0x0CBC), uint32_t(0x0CBF), uint32_t(0x0CBF),
	uint32_t(0x0CC6), uint32_t(0x0CC6), uint32_t(0x0CCC), uint32_t(0x0CCD), uint32_t(0x0CE2), uint32_t(0x0CE3), uint32_t(0x0D41), uint32_t(0x0D43),
	uint32_t(0x0D4D), uint32_t(0x0D4D), uint32_t(0x0DCA), uint32_t(0x0DCA), uint32_t(0x0DD2), uint32_t(0x0DD4), uint32_t(0x0DD6), uint32_t(0x0DD6),
	uint32_t(0x0E31), uint32_t(0x0E31), uint32_t(0x0E34), uint32_t(0x0E3A), uint32_t(0x0E47), uint32_t(0x0E4E), uint32_t(0x0EB1), uint32_t(0x0EB1),
	uint32_t(0x0EB4), uint32_t(0x0EB9), uint32_t(0x0EBB), uint32_t(0x0EBC), uint32_t(0x0EC8), uint32_t(0x0ECD), uint32_t(0x0F18), uint32_t(0x0F19),
	uint32_t(0x0F35), uint32_t(0x0F35), uint32_t(0x0F37), uint32_t(0x0F37), uint32_t(0x0F39), uint32_t(0x0F39), uint32_t(0x0F71), uint32_t(0x0F7E),
	uint32_t(0x0F80), uint32_t(0x0F84), uint32_t(0x0F86), uint32_t(0x0F87), uint32_t(0x0F90), uint32_t(0x0F97), uint32_t(0x0F99), uint32_t(0x0FBC),
	uint32_t(0x0FC6), uint32_t(0x0FC6), uint32_t(0x102D), uint32_t(0x1030), uint32_t(0x1032), uint32_t(0x1032), uint32_t(0x1036), uint32_t(0x1037),
	uint32_t(0x1039), uint32_t(0x1039), uint32_t(0x1058), uint32_t(0x1059), uint32_t(0x1160), uint32_t(0x11FF), uint32_t(0x135F), uint32_t(0x135F),
	uint32_t(0x1712), uint32_t(0x1714), uint32_t(0x1732), uint32_t(0x1734), uint32_t(0x1752), uint32_t(0x1753), uint32_t(0x1772), uint32_t(0x1773),
	uint32_t(0x17B4), uint32_t(0x17B5), uint32_t(0x17B7), uint32_t(0x17BD), uint32_t(0x17C6), uint32_t(0x17C6), uint32_t(0x17C9), uint32_t(0x17D3),
	uint32_t(0x17DD), uint32_t(0x17DD), uint32_t(0x180B), uint32_t(0x180D), uint32_t(0x18A9), uint32_t(0x18A9), uint32_t(0x1920), uint32_t(0x1922),
	uint32_t(0x1927), uint32_t(0x1928), uint32_t(0x1932), uint32_t(0x1932), uint32_t(0x1939), uint32_t(0x193B), uint32_t(0x1A17), uint32_t(0x1A18),
	uint32_t(0x1B00), uint32_t(0x1B03), uint32_t(0x1B34), uint32_t(0x1B34), uint32_t(0x1B36), uint32_t(0x1B3A), uint32_t(0x1B3C), uint32_t(0x1B3C),
	uint32_t(0x1B42), uint32_t(0x1B42), uint32_t(0x1B6B), uint32_t(0x1B73), uint32_t(0x1DC0), uint32_t(0x1DCA), uint32_t(0x1DFE), uint32_t(0x1DFF),
	uint32_t(0x200B), uint32_t(0x200F), uint32_t(0x202A), uint32_t(0x202E), uint32_t(0x2060), uint32_t(0x2063), uint32_t(0x206A), uint32_t(0x206F),
	uint32_t(0x20D0), uint32_t(0x20EF), uint32_t(0x302A), uint32_t(0x302F), uint32_t(0x3099), uint32_t(0x309A), uint32_t(0xA806), uint32_t(0xA806),
	uint32_t(0xA80B), uint32_t(0xA80B), uint32_t(0xA825), uint32_t(0xA826), uint32_t(0xFB1E), uint32_t(0xFB1E), uint32_t(0xFE00), uint32_t(0xFE0F),
	uint32_t(0xFE20), uint32_t(0xFE23), uint32_t(0xFEFF), uint32_t(0xFEFF), uint32_t(0xFFF9), uint32_t(0xFFFB), uint32_t(0x10A01), uint32_t(0x10A03),
	uint32_t(0x10A05), uint32_t(0x10A06), uint32_t(0x10A0C), uint32_t(0x10A0F), uint32_t(0x10A38), uint32_t(0x10A3A), uint32_t(0x10A3F), uint32_t(0x10A3F),
	uint32_t(0x1D167), uint32_t(0x1D169), uint32_t(0x1D173), uint32_t(0x1D182), uint32_t(0x1D185), uint32_t(0x1D18B), uint32_t(0x1D1AA), uint32_t(0x1D1AD),
	uint32_t(0x1D242), uint32_t(0x1D244), uint32_t(0xE0001), uint32_t(0xE0001), uint32_t(0xE0020), uint32_t(0xE007F), uint32_t(0xE0100), uint32_t(0xE01EF),
	libc.Uint32FromInt32(-1),
} /* unicode.c:124:19 */
var range21 = [29]uint32_t{
	uint32_t(0x1100), uint32_t(0x115F), uint32_t(0x2329), uint32_t(0x2329), uint32_t(0x232A), uint32_t(0x232A), uint32_t(0x2E80), uint32_t(0x303E),
	uint32_t(0x3040), uint32_t(0xA4CF), uint32_t(0xAC00), uint32_t(0xD7A3), uint32_t(0xF900), uint32_t(0xFAFF), uint32_t(0xFE10), uint32_t(0xFE19),
	uint32_t(0xFE30), uint32_t(0xFE6F), uint32_t(0xFF00), uint32_t(0xFF60), uint32_t(0xFFE0), uint32_t(0xFFE6), uint32_t(0x1F000), uint32_t(0x1F644),
	uint32_t(0x20000), uint32_t(0x2FFFD), uint32_t(0x30000), uint32_t(0x3FFFD), libc.Uint32FromInt32(-1),
} /* unicode.c:167:19 */

// Returns the number of columns needed to display a given
// string in a fixed-width font.
func display_width(tls *libc.TLS, p uintptr, len int32) int32 { /* unicode.c:181:5: */
	bp := tls.Alloc(8)
	defer tls.Free(8)
	*(*uintptr)(unsafe.Pointer(bp)) = p

	var start uintptr = *(*uintptr)(unsafe.Pointer(bp /* p */))
	var w int32 = 0
	for (int64(*(*uintptr)(unsafe.Pointer(bp)))-int64(start))/1 < int64(len) {
		var c uint32_t = decode_utf8(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* p */)))
		w = w + char_width(tls, c)
	}
	return w
}

func init() {
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 24)) = uintptr(unsafe.Pointer(&i32i64))            // codegen.c:375:27:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 32)) = uintptr(unsafe.Pointer(&i32u8))             // codegen.c:375:35:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 40)) = uintptr(unsafe.Pointer(&i32u16))            // codegen.c:375:42:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 56)) = uintptr(unsafe.Pointer(&i32i64))            // codegen.c:375:58:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 64)) = uintptr(unsafe.Pointer(&i32f32))            // codegen.c:375:66:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 72)) = uintptr(unsafe.Pointer(&i32f64))            // codegen.c:375:74:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 80)) = uintptr(unsafe.Pointer(&i32f80))            // codegen.c:375:82:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 88)) = uintptr(unsafe.Pointer(&i32i8))             // codegen.c:376:4:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 112)) = uintptr(unsafe.Pointer(&i32i64))           // codegen.c:376:27:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 120)) = uintptr(unsafe.Pointer(&i32u8))            // codegen.c:376:35:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 128)) = uintptr(unsafe.Pointer(&i32u16))           // codegen.c:376:42:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 144)) = uintptr(unsafe.Pointer(&i32i64))           // codegen.c:376:58:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 152)) = uintptr(unsafe.Pointer(&i32f32))           // codegen.c:376:66:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 160)) = uintptr(unsafe.Pointer(&i32f64))           // codegen.c:376:74:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 168)) = uintptr(unsafe.Pointer(&i32f80))           // codegen.c:376:82:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 176)) = uintptr(unsafe.Pointer(&i32i8))            // codegen.c:377:4:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 184)) = uintptr(unsafe.Pointer(&i32i16))           // codegen.c:377:11:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 200)) = uintptr(unsafe.Pointer(&i32i64))           // codegen.c:377:27:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 208)) = uintptr(unsafe.Pointer(&i32u8))            // codegen.c:377:35:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 216)) = uintptr(unsafe.Pointer(&i32u16))           // codegen.c:377:42:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 232)) = uintptr(unsafe.Pointer(&i32i64))           // codegen.c:377:58:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 240)) = uintptr(unsafe.Pointer(&i32f32))           // codegen.c:377:66:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 248)) = uintptr(unsafe.Pointer(&i32f64))           // codegen.c:377:74:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 256)) = uintptr(unsafe.Pointer(&i32f80))           // codegen.c:377:82:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 264)) = uintptr(unsafe.Pointer(&i32i8))            // codegen.c:378:4:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 272)) = uintptr(unsafe.Pointer(&i32i16))           // codegen.c:378:11:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 296)) = uintptr(unsafe.Pointer(&i32u8))            // codegen.c:378:35:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 304)) = uintptr(unsafe.Pointer(&i32u16))           // codegen.c:378:42:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 328)) = uintptr(unsafe.Pointer(&i64f32))           // codegen.c:378:66:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 336)) = uintptr(unsafe.Pointer(&i64f64))           // codegen.c:378:74:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 344)) = uintptr(unsafe.Pointer(&i64f80))           // codegen.c:378:82:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 352)) = uintptr(unsafe.Pointer(&i32i8))            // codegen.c:380:4:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 376)) = uintptr(unsafe.Pointer(&i32i64))           // codegen.c:380:27:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 408)) = uintptr(unsafe.Pointer(&i32i64))           // codegen.c:380:58:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 416)) = uintptr(unsafe.Pointer(&i32f32))           // codegen.c:380:66:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 424)) = uintptr(unsafe.Pointer(&i32f64))           // codegen.c:380:74:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 432)) = uintptr(unsafe.Pointer(&i32f80))           // codegen.c:380:82:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 440)) = uintptr(unsafe.Pointer(&i32i8))            // codegen.c:381:4:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 448)) = uintptr(unsafe.Pointer(&i32i16))           // codegen.c:381:11:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 464)) = uintptr(unsafe.Pointer(&i32i64))           // codegen.c:381:27:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 472)) = uintptr(unsafe.Pointer(&i32u8))            // codegen.c:381:35:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 496)) = uintptr(unsafe.Pointer(&i32i64))           // codegen.c:381:58:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 504)) = uintptr(unsafe.Pointer(&i32f32))           // codegen.c:381:66:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 512)) = uintptr(unsafe.Pointer(&i32f64))           // codegen.c:381:74:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 520)) = uintptr(unsafe.Pointer(&i32f80))           // codegen.c:381:82:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 528)) = uintptr(unsafe.Pointer(&i32i8))            // codegen.c:382:4:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 536)) = uintptr(unsafe.Pointer(&i32i16))           // codegen.c:382:11:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 552)) = uintptr(unsafe.Pointer(&u32i64))           // codegen.c:382:27:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 560)) = uintptr(unsafe.Pointer(&i32u8))            // codegen.c:382:35:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 568)) = uintptr(unsafe.Pointer(&i32u16))           // codegen.c:382:42:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 584)) = uintptr(unsafe.Pointer(&u32i64))           // codegen.c:382:58:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 592)) = uintptr(unsafe.Pointer(&u32f32))           // codegen.c:382:66:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 600)) = uintptr(unsafe.Pointer(&u32f64))           // codegen.c:382:74:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 608)) = uintptr(unsafe.Pointer(&u32f80))           // codegen.c:382:82:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 616)) = uintptr(unsafe.Pointer(&i32i8))            // codegen.c:383:4:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 624)) = uintptr(unsafe.Pointer(&i32i16))           // codegen.c:383:11:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 648)) = uintptr(unsafe.Pointer(&i32u8))            // codegen.c:383:35:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 656)) = uintptr(unsafe.Pointer(&i32u16))           // codegen.c:383:42:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 680)) = uintptr(unsafe.Pointer(&u64f32))           // codegen.c:383:66:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 688)) = uintptr(unsafe.Pointer(&u64f64))           // codegen.c:383:74:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 696)) = uintptr(unsafe.Pointer(&u64f80))           // codegen.c:383:82:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 704)) = uintptr(unsafe.Pointer(&f32i8))            // codegen.c:385:4:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 712)) = uintptr(unsafe.Pointer(&f32i16))           // codegen.c:385:11:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 720)) = uintptr(unsafe.Pointer(&f32i32))           // codegen.c:385:19:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 728)) = uintptr(unsafe.Pointer(&f32i64))           // codegen.c:385:27:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 736)) = uintptr(unsafe.Pointer(&f32u8))            // codegen.c:385:35:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 744)) = uintptr(unsafe.Pointer(&f32u16))           // codegen.c:385:42:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 752)) = uintptr(unsafe.Pointer(&f32u32))           // codegen.c:385:50:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 760)) = uintptr(unsafe.Pointer(&f32u64))           // codegen.c:385:58:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 776)) = uintptr(unsafe.Pointer(&f32f64))           // codegen.c:385:74:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 784)) = uintptr(unsafe.Pointer(&f32f80))           // codegen.c:385:82:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 792)) = uintptr(unsafe.Pointer(&f64i8))            // codegen.c:386:4:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 800)) = uintptr(unsafe.Pointer(&f64i16))           // codegen.c:386:11:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 808)) = uintptr(unsafe.Pointer(&f64i32))           // codegen.c:386:19:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 816)) = uintptr(unsafe.Pointer(&f64i64))           // codegen.c:386:27:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 824)) = uintptr(unsafe.Pointer(&f64u8))            // codegen.c:386:35:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 832)) = uintptr(unsafe.Pointer(&f64u16))           // codegen.c:386:42:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 840)) = uintptr(unsafe.Pointer(&f64u32))           // codegen.c:386:50:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 848)) = uintptr(unsafe.Pointer(&f64u64))           // codegen.c:386:58:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 856)) = uintptr(unsafe.Pointer(&f64f32))           // codegen.c:386:66:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 872)) = uintptr(unsafe.Pointer(&f64f80))           // codegen.c:386:82:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 880)) = uintptr(unsafe.Pointer(&f80i8))            // codegen.c:387:4:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 888)) = uintptr(unsafe.Pointer(&f80i16))           // codegen.c:387:11:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 896)) = uintptr(unsafe.Pointer(&f80i32))           // codegen.c:387:19:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 904)) = uintptr(unsafe.Pointer(&f80i64))           // codegen.c:387:27:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 912)) = uintptr(unsafe.Pointer(&f80u8))            // codegen.c:387:35:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 920)) = uintptr(unsafe.Pointer(&f80u16))           // codegen.c:387:42:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 928)) = uintptr(unsafe.Pointer(&f80u32))           // codegen.c:387:50:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 936)) = uintptr(unsafe.Pointer(&f80u64))           // codegen.c:387:58:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 944)) = uintptr(unsafe.Pointer(&f80f32))           // codegen.c:387:66:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&cast_table)) + 952)) = uintptr(unsafe.Pointer(&f80f64))           // codegen.c:387:74:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&scope)) + 0)) = uintptr(unsafe.Pointer(&scope_storage))           // parse.c:91:23:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&ty_bool)) + 0)) = uintptr(unsafe.Pointer(&ty_bool_storage))       // type.c:21:17:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&ty_bool)) + 0)) = uintptr(unsafe.Pointer(&ty_bool_storage))       // type.c:21:17:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&ty_char)) + 0)) = uintptr(unsafe.Pointer(&ty_char_storage))       // type.c:23:17:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&ty_char)) + 0)) = uintptr(unsafe.Pointer(&ty_char_storage))       // type.c:23:17:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&ty_double)) + 0)) = uintptr(unsafe.Pointer(&ty_double_storage))   // type.c:34:19:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&ty_double)) + 0)) = uintptr(unsafe.Pointer(&ty_double_storage))   // type.c:34:19:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&ty_float)) + 0)) = uintptr(unsafe.Pointer(&ty_float_storage))     // type.c:33:18:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&ty_float)) + 0)) = uintptr(unsafe.Pointer(&ty_float_storage))     // type.c:33:18:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&ty_int)) + 0)) = uintptr(unsafe.Pointer(&ty_int_storage))         // type.c:25:16:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&ty_int)) + 0)) = uintptr(unsafe.Pointer(&ty_int_storage))         // type.c:25:16:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&ty_ldouble)) + 0)) = uintptr(unsafe.Pointer(&ty_ldouble_storage)) // type.c:35:20:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&ty_ldouble)) + 0)) = uintptr(unsafe.Pointer(&ty_ldouble_storage)) // type.c:35:20:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&ty_long)) + 0)) = uintptr(unsafe.Pointer(&ty_long_storage))       // type.c:26:17:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&ty_long)) + 0)) = uintptr(unsafe.Pointer(&ty_long_storage))       // type.c:26:17:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&ty_short)) + 0)) = uintptr(unsafe.Pointer(&ty_short_storage))     // type.c:24:18:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&ty_short)) + 0)) = uintptr(unsafe.Pointer(&ty_short_storage))     // type.c:24:18:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&ty_uchar)) + 0)) = uintptr(unsafe.Pointer(&ty_uchar_storage))     // type.c:28:18:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&ty_uchar)) + 0)) = uintptr(unsafe.Pointer(&ty_uchar_storage))     // type.c:28:18:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&ty_uint)) + 0)) = uintptr(unsafe.Pointer(&ty_uint_storage))       // type.c:30:17:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&ty_uint)) + 0)) = uintptr(unsafe.Pointer(&ty_uint_storage))       // type.c:30:17:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&ty_ulong)) + 0)) = uintptr(unsafe.Pointer(&ty_ulong_storage))     // type.c:31:18:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&ty_ulong)) + 0)) = uintptr(unsafe.Pointer(&ty_ulong_storage))     // type.c:31:18:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&ty_ushort)) + 0)) = uintptr(unsafe.Pointer(&ty_ushort_storage))   // type.c:29:19:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&ty_ushort)) + 0)) = uintptr(unsafe.Pointer(&ty_ushort_storage))   // type.c:29:19:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&ty_void)) + 0)) = uintptr(unsafe.Pointer(&ty_void_storage))       // type.c:20:17:
	*(*uintptr)(unsafe.Pointer(uintptr(unsafe.Pointer(&ty_void)) + 0)) = uintptr(unsafe.Pointer(&ty_void_storage))       // type.c:20:17:
}

var ts1 = "%dil\x00%sil\x00%dl\x00%cl\x00%r8b\x00%r9b\x00%di\x00%si\x00%dx\x00%cx\x00%r8w\x00%r9w\x00%edi\x00%esi\x00%edx\x00%ecx\x00%r8d\x00%r9d\x00%rdi\x00%rsi\x00%rdx\x00%rcx\x00%r8\x00%r9\x00\n\x00  push %%rax\x00  pop %s\x00  sub $8, %%rsp\x00  movsd %%xmm0, (%%rsp)\x00  movsd (%%rsp), %%xmm%d\x00  add $8, %%rsp\x00internal error at %s:%d\x00codegen.c\x00%al\x00%ax\x00%eax\x00%rax\x00  mov %d(%%rbp), %%rax\x00  lea %d(%%rbp), %%rax\x00  data16 lea %s@tlsgd(%%rip), %%rdi\x00  .value 0x6666\x00  rex64\x00  call __tls_get_addr@PLT\x00  mov %s@GOTPCREL(%%rip), %%rax\x00  mov %%fs:0, %%rax\x00  add $%s@tpoff, %%rax\x00  lea %s(%%rip), %%rax\x00  add $%d, %%rax\x00not an lvalue\x00  movss (%%rax), %%xmm0\x00  movsd (%%rax), %%xmm0\x00  fldt (%%rax)\x00movz\x00movs\x00  %sbl (%%rax), %%eax\x00  %swl (%%rax), %%eax\x00  movsxd (%%rax), %%rax\x00  mov (%%rax), %%rax\x00  mov %d(%%rax), %%r8b\x00  mov %%r8b, %d(%%rdi)\x00  movss %%xmm0, (%%rdi)\x00  movsd %%xmm0, (%%rdi)\x00  fstpt (%%rdi)\x00  mov %%al, (%%rdi)\x00  mov %%ax, (%%rdi)\x00  mov %%eax, (%%rdi)\x00  mov %%rax, (%%rdi)\x00  xorps %%xmm1, %%xmm1\x00  ucomiss %%xmm1, %%xmm0\x00  xorpd %%xmm1, %%xmm1\x00  ucomisd %%xmm1, %%xmm0\x00  fldz\x00  fucomip\x00  fstp %%st(0)\x00  cmp $0, %%eax\x00  cmp $0, %%rax\x00movsbl %al, %eax\x00movzbl %al, %eax\x00movswl %ax, %eax\x00movzwl %ax, %eax\x00cvtsi2ssl %eax, %xmm0\x00movsxd %eax, %rax\x00cvtsi2sdl %eax, %xmm0\x00mov %eax, -4(%rsp); fildl -4(%rsp)\x00mov %eax, %eax; cvtsi2ssq %rax, %xmm0\x00mov %eax, %eax\x00mov %eax, %eax; cvtsi2sdq %rax, %xmm0\x00mov %eax, %eax; mov %rax, -8(%rsp); fildll -8(%rsp)\x00cvtsi2ssq %rax, %xmm0\x00cvtsi2sdq %rax, %xmm0\x00movq %rax, -8(%rsp); fildll -8(%rsp)\x00test %rax,%rax; js 1f; pxor %xmm0,%xmm0; cvtsi2sd %rax,%xmm0; jmp 2f; 1: mov %rax,%rdi; and $1,%eax; pxor %xmm0,%xmm0; shr %rdi; or %rax,%rdi; cvtsi2sd %rdi,%xmm0; addsd %xmm0,%xmm0; 2:\x00mov %rax, -8(%rsp); fildq -8(%rsp); test %rax, %rax; jns 1f;mov $1602224128, %eax; mov %eax, -4(%rsp); fadds -4(%rsp); 1:\x00cvttss2sil %xmm0, %eax; movsbl %al, %eax\x00cvttss2sil %xmm0, %eax; movzbl %al, %eax\x00cvttss2sil %xmm0, %eax; movswl %ax, %eax\x00cvttss2sil %xmm0, %eax; movzwl %ax, %eax\x00cvttss2sil %xmm0, %eax\x00cvttss2siq %xmm0, %rax\x00cvtss2sd %xmm0, %xmm0\x00movss %xmm0, -4(%rsp); flds -4(%rsp)\x00cvttsd2sil %xmm0, %eax; movsbl %al, %eax\x00cvttsd2sil %xmm0, %eax; movzbl %al, %eax\x00cvttsd2sil %xmm0, %eax; movswl %ax, %eax\x00cvttsd2sil %xmm0, %eax; movzwl %ax, %eax\x00cvttsd2sil %xmm0, %eax\x00cvttsd2siq %xmm0, %rax\x00cvtsd2ss %xmm0, %xmm0\x00movsd %xmm0, -8(%rsp); fldl -8(%rsp)\x00fnstcw -10(%rsp); movzwl -10(%rsp), %eax; or $12, %ah; mov %ax, -12(%rsp); fldcw -12(%rsp); fistps -24(%rsp); fldcw -10(%rsp); movsbl -24(%rsp), %eax\x00fnstcw -10(%rsp); movzwl -10(%rsp), %eax; or $12, %ah; mov %ax, -12(%rsp); fldcw -12(%rsp); fistps -24(%rsp); fldcw -10(%rsp); movzbl -24(%rsp), %eax\x00fnstcw -10(%rsp); movzwl -10(%rsp), %eax; or $12, %ah; mov %ax, -12(%rsp); fldcw -12(%rsp); fistpl -24(%rsp); fldcw -10(%rsp); movswl -24(%rsp), %eax\x00fnstcw -10(%rsp); movzwl -10(%rsp), %eax; or $12, %ah; mov %ax, -12(%rsp); fldcw -12(%rsp); fistpl -24(%rsp); fldcw -10(%rsp); mov -24(%rsp), %eax\x00fnstcw -10(%rsp); movzwl -10(%rsp), %eax; or $12, %ah; mov %ax, -12(%rsp); fldcw -12(%rsp); fistpq -24(%rsp); fldcw -10(%rsp); mov -24(%rsp), %rax\x00fstps -8(%rsp); movss -8(%rsp), %xmm0\x00fstpl -8(%rsp); movsd -8(%rsp), %xmm0\x00  setne %%al\x00  movzx %%al, %%eax\x00  %s\x00  sub $%d, %%rsp\x00  mov %d(%%rax), %%r10b\x00  mov %%r10b, %d(%%rsp)\x00  sub $16, %%rsp\x00  fstpt (%%rsp)\x00ty->size == 4 || 8 <= ty->size\x00  movss %%xmm0, %d(%%rbp)\x00  movsd %%xmm0, %d(%%rbp)\x00  mov %%al, %d(%%rbp)\x00  shr $8, %%rax\x00ty->size == 12 || ty->size == 16\x00  movss %%xmm%d, %d(%%rbp)\x00  movsd %%xmm%d, %d(%%rbp)\x00  mov %s, %d(%%rbp)\x00  shr $8, %s\x00copy_ret_buffer\x00  mov %%rax, %%rdi\x00  movss (%%rdi), %%xmm0\x00  movsd (%%rdi), %%xmm0\x00  mov $0, %%rax\x00  shl $8, %%rax\x00  mov %d(%%rdi), %%al\x00  movss 8(%%rdi), %%xmm%d\x00  movsd 8(%%rdi), %%xmm%d\x00  mov $0, %s\x00  shl $8, %s\x00  mov %d(%%rdi), %s\x00copy_struct_reg\x00  mov %d(%%rbp), %%rdi\x00  mov %d(%%rax), %%dl\x00  mov %%dl, %d(%%rdi)\x00  add $15, %%rdi\x00  and $0xfffffff0, %%edi\x00  mov %d(%%rbp), %%rcx\x00  sub %%rsp, %%rcx\x00  mov %%rsp, %%rax\x00  sub %%rdi, %%rsp\x00  mov %%rsp, %%rdx\x001:\x00  cmp $0, %%rcx\x00  je 2f\x00  mov (%%rax), %%r8b\x00  mov %%r8b, (%%rdx)\x00  inc %%rdx\x00  inc %%rax\x00  dec %%rcx\x00  jmp 1b\x002:\x00  sub %%rdi, %%rax\x00  mov %%rax, %d(%%rbp)\x00  .loc %d %d\x00  mov $%u, %%eax  # float %Lf\x00  movq %%rax, %%xmm0\x00  mov $%lu, %%rax  # double %Lf\x00  mov $%lu, %%rax  # long double %Lf\x00  mov %%rax, -16(%%rsp)\x00  mov $%lu, %%rax\x00  mov %%rax, -8(%%rsp)\x00  fldt -16(%%rsp)\x00  mov $%ld, %%rax\x00  mov $1, %%rax\x00  shl $31, %%rax\x00  movq %%rax, %%xmm1\x00  xorps %%xmm1, %%xmm0\x00  shl $63, %%rax\x00  xorpd %%xmm1, %%xmm0\x00  fchs\x00  neg %%rax\x00  shl $%d, %%rax\x00  shr $%d, %%rax\x00  sar $%d, %%rax\x00  mov %%rax, %%r8\x00  and $%ld, %%rdi\x00  shl $%d, %%rdi\x00  mov (%%rsp), %%rax\x00  mov $%ld, %%r9\x00  and %%r9, %%rax\x00  or %%rdi, %%rax\x00  mov %%r8, %%rax\x00  mov $%d, %%rcx\x00  lea %d(%%rbp), %%rdi\x00  mov $0, %%al\x00  rep stosb\x00  je .L.else.%d\x00  jmp .L.end.%d\x00.L.else.%d:\x00.L.end.%d:\x00  sete %%al\x00  movzx %%al, %%rax\x00  not %%rax\x00  je .L.false.%d\x00.L.false.%d:\x00  jne .L.true.%d\x00.L.true.%d:\x00alloca\x00  mov %%rax, %%r10\x00  mov $%d, %%rax\x00  call *%%r10\x00  add $%d, %%rsp\x00  movzbl %%al, %%eax\x00  movsbl %%al, %%eax\x00  movzwl %%ax, %%eax\x00  movswl %%ax, %%eax\x00  lock cmpxchg %s, (%%rdi)\x00  sete %%cl\x00  je 1f\x00  mov %s, (%%r8)\x00  movzbl %%cl, %%eax\x00  xchg %s, (%%rdi)\x00ss\x00sd\x00  add%s %%xmm1, %%xmm0\x00  sub%s %%xmm1, %%xmm0\x00  mul%s %%xmm1, %%xmm0\x00  div%s %%xmm1, %%xmm0\x00  ucomi%s %%xmm0, %%xmm1\x00  setnp %%dl\x00  and %%dl, %%al\x00  setp %%dl\x00  or %%dl, %%al\x00  seta %%al\x00  setae %%al\x00  and $1, %%al\x00  movzb %%al, %%rax\x00invalid expression\x00  faddp\x00  fsubrp\x00  fmulp\x00  fdivrp\x00  fcomip\x00  add %s, %s\x00  sub %s, %s\x00  imul %s, %s\x00  div %s\x00  cqo\x00  cdq\x00  idiv %s\x00  mov %%rdx, %%rax\x00  and %s, %s\x00  or %s, %s\x00  xor %s, %s\x00  cmp %s, %s\x00  setb %%al\x00  setl %%al\x00  setbe %%al\x00  setle %%al\x00  mov %%rdi, %%rcx\x00  shl %%cl, %s\x00  shr %%cl, %s\x00  sar %%cl, %s\x00  je  .L.else.%d\x00.L.begin.%d:\x00  je %s\x00%s:\x00  jmp .L.begin.%d\x00  jne .L.begin.%d\x00  cmp $%ld, %s\x00  mov %s, %s\x00  sub $%ld, %s\x00  jbe %s\x00  jmp %s\x00  jmp *%%rax\x00  jmp .L.return.%s\x00invalid statement\x00  .local %s\x00  .globl %s\x00  .comm %s, %d, %d\x00  .section .tdata,\"awT\",@progbits\x00  .data\x00  .type %s, @object\x00  .size %s, %d\x00  .align %d\x00  .quad %s%+ld\x00  .byte %d\x00  .section .tbss,\"awT\",@nobits\x00  .bss\x00  .zero %d\x00  .text\x00  .type %s, @function\x00  push %%rbp\x00  mov %%rsp, %%rbp\x00  mov %%rsp, %d(%%rbp)\x00  movl $%d, %d(%%rbp)\x00  movq %%rbp, %d(%%rbp)\x00  addq $16, %d(%%rbp)\x00  addq $%d, %d(%%rbp)\x00  movq %%rdi, %d(%%rbp)\x00  movq %%rsi, %d(%%rbp)\x00  movq %%rdx, %d(%%rbp)\x00  movq %%rcx, %d(%%rbp)\x00  movq %%r8, %d(%%rbp)\x00  movq %%r9, %d(%%rbp)\x00  movsd %%xmm1, %d(%%rbp)\x00  movsd %%xmm2, %d(%%rbp)\x00  movsd %%xmm3, %d(%%rbp)\x00  movsd %%xmm4, %d(%%rbp)\x00  movsd %%xmm5, %d(%%rbp)\x00  movsd %%xmm6, %d(%%rbp)\x00  movsd %%xmm7, %d(%%rbp)\x00ty->size <= 16\x00depth == 0\x00main\x00.L.return.%s:\x00  mov %%rbp, %%rsp\x00  pop %%rbp\x00  ret\x00emit_text\x00  .file %d \"%s\"\x00.\x00/\x00cap > 0\x00hashmap.c\x00map2.used == nkeys\x00rehash\x00key %d\x00(size_t)hashmap_get(map, format(\"key %d\", i)) == i\x00no such key\x00hashmap_get(map, \"no such key\") == NULL\x00OK\n\x00hashmap_test\x00chibicc [ -o <path> ] <file>\n\x00-o\x00-I\x00-idirafter\x00-include\x00-x\x00-MF\x00-MT\x00-Xlinker\x00%s/include\x00/usr/local/include\x00/usr/include/x86_64-linux-gnu\x00/usr/include\x001\x00c\x00assembler\x00none\x00<command line>: unknown argument for -x: %s\x00-###\x00-cc1\x00--help\x00-S\x00-fcommon\x00-fno-common\x00-c\x00-E\x00-D\x00-U\x00-l\x00-Wl,\x00-s\x00-M\x00-MP\x00%s %s\x00-MD\x00-MQ\x00-MMD\x00-fpic\x00-fPIC\x00-cc1-input\x00-cc1-output\x00-static\x00-shared\x00-L\x00-hashmap-test\x00-O\x00-W\x00-g\x00-std=\x00-ffreestanding\x00-fno-builtin\x00-fno-omit-frame-pointer\x00-fno-stack-protector\x00-fno-strict-aliasing\x00-m64\x00-mno-red-zone\x00-w\x00unknown argument: %s\x00no input files\x00-\x00w\x00cannot open output file: %s: %s\x00%s%s\x00/tmp/chibicc-XXXXXX\x00mkstemp failed: %s\x00%s\x00 %s\x00 \x00%.*s\x00.d\x00.o\x00 \\\n  %s\x00\n\n\x00%s:\n\n\x00%s: %s\x00-include: %s: %s\x00as\x00%s/%s%s\x00/usr/lib/x86_64-linux-gnu/crti.o\x00/usr/lib/x86_64-linux-gnu\x00/usr/lib64/crti.o\x00/usr/lib64\x00library path is not found\x00/usr/lib/gcc/x86_64-linux-gnu/*/crtbegin.o\x00/usr/lib/gcc/x86_64-pc-linux-gnu/*/crtbegin.o\x00/usr/lib/gcc/x86_64-redhat-linux/*/crtbegin.o\x00gcc library path is not found\x00ld\x00-m\x00elf_x86_64\x00%s/crti.o\x00%s/crtbeginS.o\x00%s/crt1.o\x00%s/crtbegin.o\x00-L%s\x00-L/usr/lib/x86_64-linux-gnu\x00-L/usr/lib64\x00-L/lib64\x00-L/usr/lib/x86_64-pc-linux-gnu\x00-L/usr/lib/x86_64-redhat-linux\x00-L/usr/lib\x00-L/lib\x00-dynamic-linker\x00/lib64/ld-linux-x86-64.so.2\x00--start-group\x00-lgcc\x00-lgcc_eh\x00-lc\x00--end-group\x00--as-needed\x00-lgcc_s\x00--no-as-needed\x00%s/crtendS.o\x00%s/crtend.o\x00%s/crtn.o\x00.a\x00.so\x00.c\x00.s\x00<command line>: unknown file extension: %s\x00cannot specify '-o' with '-c,' '-S' or '-E' with multiple files\x00,\x00type == FILE_C\x00main.c\x00a.out\x00.L..%d\x00expected an identifier\x00typedef\x00static\x00extern\x00inline\x00_Thread_local\x00__thread\x00storage class specifier is not allowed in this context\x00typedef may not be used together with static, extern, inline, __thread or _Thread_local\x00const\x00volatile\x00auto\x00register\x00restrict\x00__restrict\x00__restrict__\x00_Noreturn\x00_Atomic\x00(\x00)\x00_Alignas\x00_Alignas is not allowed in this context\x00struct\x00union\x00enum\x00typeof\x00void\x00_Bool\x00char\x00short\x00int\x00long\x00float\x00double\x00signed\x00unsigned\x00parse.c\x00invalid type\x00...\x00]\x00[\x00*\x00}\x00{\x00unknown enum type\x00not an enum tag\x00=\x00\x00;\x00variable declared void\x00variable name omitted\x00variable-sized object may not be initialized\x00variable has incomplete type\x00array designator index exceeds array bounds\x00array designator range [%d, %d] is empty\x00expected a field designator\x00struct has no such member\x00array index in non-array initializer\x00field name not in struct or union initializer\x00expected string literal\x00return\x00if\x00else\x00switch\x00case\x00stray case\x00empty case range specified\x00:\x00default\x00stray default\x00for\x00while\x00do\x00asm\x00goto\x00break\x00stray break\x00continue\x00stray continue\x00not a compile-time constant\x00invalid initializer\x00+=\x00-=\x00*=\x00/=\x00%=\x00&=\x00|=\x00^=\x00<<=\x00>>=\x00?\x00||\x00&&\x00|\x00^\x00&\x00==\x00!=\x00<\x00<=\x00>\x00>=\x00<<\x00>>\x00invalid operands\x00+\x00%\x00cannot take address of bitfield\x00!\x00~\x00++\x00--\x00__attribute__\x00packed\x00aligned\x00unknown attribute\x00not a struct nor a union\x00no such member\x00->\x00not a function\x00too many arguments\x00too few arguments\x00controlling expression type not compatible with any generic association type\x00sizeof\x00_Alignof\x00_Generic\x00__builtin_types_compatible_p\x00__builtin_reg_class\x00__builtin_compare_and_swap\x00__builtin_atomic_exchange\x00implicit declaration of a function\x00undefined variable\x00expected an expression\x00typedef name omitted\x00parameter name omitted\x00use of undeclared label\x00function name omitted\x00redeclared as a different kind of symbol\x00redefinition of %s\x00static declaration follows a non-static declaration\x00__va_area__\x00__alloca_size__\x00__func__\x00__FUNCTION__\x00#\x00extra token\x00ifdef\x00ifndef\x00endif\x00elif\x00%d\n\x00defined\x00macro name must be an identifier\x00no expression\x00__VA_ARGS__\x00premature end of input\x00%.*s%.*s\x00pasting forms '%s', an invalid token\x00'#' is not followed by a macro parameter\x00##\x00'##' cannot appear at start of macro expansion\x00'##' cannot appear at end of macro expansion\x00__VA_OPT__\x00%s/%s\x00expected '>'\x00expected a filename\x00define\x00%s: cannot open file: %s\x00invalid line marker\x00filename expected\x00include\x00include_next\x00undef\x00stray #elif\x00stray #else\x00stray #endif\x00line\x00pragma\x00once\x00error\x00invalid preprocessor directive\x00<built-in>\x00??? ??? ?? ??:??:?? ????\x00\"%s %2d %d\"\x00Jan\x00Feb\x00Mar\x00Apr\x00May\x00Jun\x00Jul\x00Aug\x00Sep\x00Oct\x00Nov\x00Dec\x00\"%02d:%02d:%02d\"\x00_LP64\x00__C99_MACRO_WITH_VA_ARGS\x00__ELF__\x00__LP64__\x00__SIZEOF_DOUBLE__\x008\x00__SIZEOF_FLOAT__\x004\x00__SIZEOF_INT__\x00__SIZEOF_LONG_DOUBLE__\x00__SIZEOF_LONG_LONG__\x00__SIZEOF_LONG__\x00__SIZEOF_POINTER__\x00__SIZEOF_PTRDIFF_T__\x00__SIZEOF_SHORT__\x002\x00__SIZEOF_SIZE_T__\x00__SIZE_TYPE__\x00unsigned long\x00__STDC_HOSTED__\x00__STDC_NO_COMPLEX__\x00__STDC_UTF_16__\x00__STDC_UTF_32__\x00__STDC_VERSION__\x00201112L\x00__STDC__\x00__USER_LABEL_PREFIX__\x00__alignof__\x00__amd64\x00__amd64__\x00__chibicc__\x00__const__\x00__gnu_linux__\x00__inline__\x00__linux\x00__linux__\x00__signed__\x00__typeof__\x00__unix\x00__unix__\x00__volatile__\x00__x86_64\x00__x86_64__\x00linux\x00unix\x00__FILE__\x00__LINE__\x00__COUNTER__\x00__TIMESTAMP__\x00__BASE_FILE__\x00__DATE__\x00__TIME__\x00u8\x00preprocess.c\x00unsupported non-standard concatenation of string literals\x00unterminated conditional directive\x00%s:%d: \x00%.*s\n\x00%*s\x00^ \x00expected '%s'\x00invalid hex escape sequence\x00unclosed string literal\x00unclosed char literal\x000x\x000b\x00LLU\x00LLu\x00llU\x00llu\x00ULL\x00Ull\x00uLL\x00ull\x00lu\x00ul\x00LL\x00ll\x00invalid numeric constant\x00//\x00/*\x00*/\x00unclosed block comment\x00eEpP\x00+-\x00u8\"\x00u\"\x00L\"\x00U\"\x00u'\x00L'\x00U'\x00invalid token\x00r\x00\\u\x00\\U\x00\ufeff\x00invalid pointer dereference\x00dereferencing a void pointer\x00statement expression returning void is not supported\x00pointer expected\x00invalid UTF-8 sequence\x00"
var ts = (*reflect.StringHeader)(unsafe.Pointer(&ts1)).Data
