package main

import (
	"modernc.org/libc"
	"unsafe"
)

func usage(tls *libc.TLS, status int32) { /* main.c:37:13: */
	libc.Xfprintf(tls, libc.Xstderr, ts+6883, 0)
	libc.Xexit(tls, status)
}

func take_arg(tls *libc.TLS, arg uintptr) uint8 { /* main.c:42:13: */
	bp := tls.Alloc(64)
	defer tls.Free(64)

	*(*[8]uintptr)(unsafe.Pointer(bp /* x */)) = [8]uintptr{
		ts + 6913, ts + 6916, ts + 6919, ts + 6930, ts + 6939, ts + 6942, ts + 6946, ts + 6950,
	}

	{
		var i int32 = 0
		for ; uint64(i) < uint64(unsafe.Sizeof([8]uintptr{}))/uint64(unsafe.Sizeof(uintptr(0))); i++ {
			if !(libc.Xstrcmp(tls, arg, *(*uintptr)(unsafe.Pointer(bp + uintptr(i)*8))) != 0) {
				return uint8(1)
			}
		}
	}
	return uint8(0)
}

func add_default_include_paths(tls *libc.TLS, argv0 uintptr) { /* main.c:53:13: */
	bp := tls.Alloc(8)
	defer tls.Free(8)

	// We expect that chibicc-specific include files are installed
	// to ./include relative to argv[0].
	strarray_push(tls, uintptr(unsafe.Pointer(&include_paths)), format(tls, ts+6959, libc.VaList(bp, xdirname(tls, argv0))))

	// Add standard include paths.
	strarray_push(tls, uintptr(unsafe.Pointer(&include_paths)), ts+6970)
	strarray_push(tls, uintptr(unsafe.Pointer(&include_paths)), ts+6989)
	strarray_push(tls, uintptr(unsafe.Pointer(&include_paths)), ts+7019)

	// Keep a copy of the standard include paths for -MMD option.
	{
		var i int32 = 0
		for ; i < include_paths.len; i++ {
			strarray_push(tls, uintptr(unsafe.Pointer(&std_include_paths)), *(*uintptr)(unsafe.Pointer(include_paths.data + uintptr(i)*8)))
		}
	}
}

func define(tls *libc.TLS, str uintptr) { /* main.c:68:13: */
	var eq uintptr = libc.Xstrchr(tls, str, '=')
	if eq != 0 {
		define_macro(tls, xstrndup(tls, str, uint64((int64(eq)-int64(str))/1)), eq+uintptr(1))
	} else {
		define_macro(tls, str, ts+7032)
	}
}

func parse_opt_x(tls *libc.TLS, s uintptr) FileType { /* main.c:76:17: */
	bp := tls.Alloc(8)
	defer tls.Free(8)

	if !(libc.Xstrcmp(tls, s, ts+7034) != 0) {
		return FILE_C
	}
	if !(libc.Xstrcmp(tls, s, ts+7036) != 0) {
		return FILE_ASM
	}
	if !(libc.Xstrcmp(tls, s, ts+7046) != 0) {
		return FILE_NONE
	}
	error(tls, ts+7051, libc.VaList(bp, s))
	return FileType(0)
}

func quote_makefile(tls *libc.TLS, s uintptr) uintptr { /* main.c:86:13: */
	var buf uintptr = libc.Xcalloc(tls, uint64(1), libc.Xstrlen(tls, s)*uint64(2)+uint64(1))

	{
		var i int32 = 0
		var j int32 = 0
		for ; *(*int8)(unsafe.Pointer(s + uintptr(i))) != 0; i++ {
			{
				var k int32
				switch int32(*(*int8)(unsafe.Pointer(s + uintptr(i)))) {
				case '$':
					*(*int8)(unsafe.Pointer(buf + uintptr(libc.PostIncInt32(&j, 1)))) = int8('$')
					*(*int8)(unsafe.Pointer(buf + uintptr(libc.PostIncInt32(&j, 1)))) = int8('$')
					break
				case '#':
					*(*int8)(unsafe.Pointer(buf + uintptr(libc.PostIncInt32(&j, 1)))) = int8('\\')
					*(*int8)(unsafe.Pointer(buf + uintptr(libc.PostIncInt32(&j, 1)))) = int8('#')
					break
				case ' ':
					fallthrough
				case '\t':
					{
						k = i - 1
						for ; k >= 0 && int32(*(*int8)(unsafe.Pointer(s + uintptr(k)))) == '\\'; k-- {
							*(*int8)(unsafe.Pointer(buf + uintptr(libc.PostIncInt32(&j, 1)))) = int8('\\')
						}
					}
					*(*int8)(unsafe.Pointer(buf + uintptr(libc.PostIncInt32(&j, 1)))) = int8('\\')
					*(*int8)(unsafe.Pointer(buf + uintptr(libc.PostIncInt32(&j, 1)))) = *(*int8)(unsafe.Pointer(s + uintptr(i)))
					break
				default:
					*(*int8)(unsafe.Pointer(buf + uintptr(libc.PostIncInt32(&j, 1)))) = *(*int8)(unsafe.Pointer(s + uintptr(i)))
					break
				}
			}
		}
	}
	return buf
}

func parse_args(tls *libc.TLS, argc int32, argv uintptr) { /* main.c:114:13: */
	bp := tls.Alloc(56)
	defer tls.Free(56)

	// Make sure that all command line options that take an argument
	// have an argument.
	{
		var i int32 = 1
		for ; i < argc; i++ {
			if take_arg(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i)*8))) != 0 {
				if !(int32(*(*uintptr)(unsafe.Pointer(argv + uintptr(libc.PreIncInt32(&i, 1))*8))) != 0) {
					usage(tls, 1)
				}
			}
		}
	}

	*(*StringArray)(unsafe.Pointer(bp + 40 /* idirafter */)) = StringArray{}

	{
		var i1 int32 = 1
		for ; i1 < argc; i1++ {
			if !(libc.Xstrcmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7095) != 0) {
				opt_hash_hash_hash = uint8(1)
				continue
			}

			if !(libc.Xstrcmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7100) != 0) {
				opt_cc1 = uint8(1)
				continue
			}

			if !(libc.Xstrcmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7105) != 0) {
				usage(tls, 0)
			}

			if !(libc.Xstrcmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+6913) != 0) {
				opt_o = *(*uintptr)(unsafe.Pointer(argv + uintptr(libc.PreIncInt32(&i1, 1))*8))
				continue
			}

			if !(libc.Xstrncmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+6913, uint64(2)) != 0) {
				opt_o = *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)) + uintptr(2)
				continue
			}

			if !(libc.Xstrcmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7112) != 0) {
				opt_S = uint8(1)
				continue
			}

			if !(libc.Xstrcmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7115) != 0) {
				opt_fcommon = uint8(1)
				continue
			}

			if !(libc.Xstrcmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7124) != 0) {
				opt_fcommon = uint8(0)
				continue
			}

			if !(libc.Xstrcmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7136) != 0) {
				opt_c = uint8(1)
				continue
			}

			if !(libc.Xstrcmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7139) != 0) {
				opt_E = uint8(1)
				continue
			}

			if !(libc.Xstrncmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+6916, uint64(2)) != 0) {
				strarray_push(tls, uintptr(unsafe.Pointer(&include_paths)), *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8))+uintptr(2))
				continue
			}

			if !(libc.Xstrcmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7142) != 0) {
				define(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(libc.PreIncInt32(&i1, 1))*8)))
				continue
			}

			if !(libc.Xstrncmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7142, uint64(2)) != 0) {
				define(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8))+uintptr(2))
				continue
			}

			if !(libc.Xstrcmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7145) != 0) {
				undef_macro(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(libc.PreIncInt32(&i1, 1))*8)))
				continue
			}

			if !(libc.Xstrncmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7145, uint64(2)) != 0) {
				undef_macro(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8))+uintptr(2))
				continue
			}

			if !(libc.Xstrcmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+6930) != 0) {
				strarray_push(tls, uintptr(unsafe.Pointer(&opt_include)), *(*uintptr)(unsafe.Pointer(argv + uintptr(libc.PreIncInt32(&i1, 1))*8)))
				continue
			}

			if !(libc.Xstrcmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+6939) != 0) {
				opt_x = parse_opt_x(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(libc.PreIncInt32(&i1, 1))*8)))
				continue
			}

			if !(libc.Xstrncmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+6939, uint64(2)) != 0) {
				opt_x = parse_opt_x(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8))+uintptr(2))
				continue
			}

			if !(libc.Xstrncmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7148, uint64(2)) != 0) || !(libc.Xstrncmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7151, uint64(4)) != 0) {
				strarray_push(tls, uintptr(unsafe.Pointer(&input_paths)), *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)))
				continue
			}

			if !(libc.Xstrcmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+6950) != 0) {
				strarray_push(tls, uintptr(unsafe.Pointer(&ld_extra_args)), *(*uintptr)(unsafe.Pointer(argv + uintptr(libc.PreIncInt32(&i1, 1))*8)))
				continue
			}

			if !(libc.Xstrcmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7156) != 0) {
				strarray_push(tls, uintptr(unsafe.Pointer(&ld_extra_args)), ts+7156)
				continue
			}

			if !(libc.Xstrcmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7159) != 0) {
				opt_M = uint8(1)
				continue
			}

			if !(libc.Xstrcmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+6942) != 0) {
				opt_MF = *(*uintptr)(unsafe.Pointer(argv + uintptr(libc.PreIncInt32(&i1, 1))*8))
				continue
			}

			if !(libc.Xstrcmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7162) != 0) {
				opt_MP = uint8(1)
				continue
			}

			if !(libc.Xstrcmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+6946) != 0) {
				if opt_MT == uintptr(0) {
					opt_MT = *(*uintptr)(unsafe.Pointer(argv + uintptr(libc.PreIncInt32(&i1, 1))*8))
				} else {
					opt_MT = format(tls, ts+7166, libc.VaList(bp, opt_MT, *(*uintptr)(unsafe.Pointer(argv + uintptr(libc.PreIncInt32(&i1, 1))*8))))
				}
				continue
			}

			if !(libc.Xstrcmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7172) != 0) {
				opt_MD = uint8(1)
				continue
			}

			if !(libc.Xstrcmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7176) != 0) {
				if opt_MT == uintptr(0) {
					opt_MT = quote_makefile(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(libc.PreIncInt32(&i1, 1))*8)))
				} else {
					opt_MT = format(tls, ts+7166, libc.VaList(bp+16, opt_MT, quote_makefile(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(libc.PreIncInt32(&i1, 1))*8)))))
				}
				continue
			}

			if !(libc.Xstrcmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7180) != 0) {
				opt_MD = libc.AssignPtrUint8(uintptr(unsafe.Pointer(&opt_MMD)), uint8(1))
				continue
			}

			if !(libc.Xstrcmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7185) != 0) || !(libc.Xstrcmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7191) != 0) {
				opt_fpic = uint8(1)
				continue
			}

			if !(libc.Xstrcmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7197) != 0) {
				base_file = *(*uintptr)(unsafe.Pointer(argv + uintptr(libc.PreIncInt32(&i1, 1))*8))
				continue
			}

			if !(libc.Xstrcmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7208) != 0) {
				output_file1 = *(*uintptr)(unsafe.Pointer(argv + uintptr(libc.PreIncInt32(&i1, 1))*8))
				continue
			}

			if !(libc.Xstrcmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+6919) != 0) {
				strarray_push(tls, bp+40, *(*uintptr)(unsafe.Pointer(argv + uintptr(libc.PostIncInt32(&i1, 1))*8)))
				continue
			}

			if !(libc.Xstrcmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7220) != 0) {
				opt_static = uint8(1)
				strarray_push(tls, uintptr(unsafe.Pointer(&ld_extra_args)), ts+7220)
				continue
			}

			if !(libc.Xstrcmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7228) != 0) {
				opt_shared = uint8(1)
				strarray_push(tls, uintptr(unsafe.Pointer(&ld_extra_args)), ts+7228)
				continue
			}

			if !(libc.Xstrcmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7236) != 0) {
				strarray_push(tls, uintptr(unsafe.Pointer(&ld_extra_args)), ts+7236)
				strarray_push(tls, uintptr(unsafe.Pointer(&ld_extra_args)), *(*uintptr)(unsafe.Pointer(argv + uintptr(libc.PreIncInt32(&i1, 1))*8)))
				continue
			}

			if !(libc.Xstrncmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7236, uint64(2)) != 0) {
				strarray_push(tls, uintptr(unsafe.Pointer(&ld_extra_args)), ts+7236)
				strarray_push(tls, uintptr(unsafe.Pointer(&ld_extra_args)), *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8))+uintptr(2))
				continue
			}

			if !(libc.Xstrcmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7239) != 0) {
				hashmap_test(tls)
				libc.Xexit(tls, 0)
			}

			// These options are ignored for now.
			if !(libc.Xstrncmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7253, uint64(2)) != 0) || !(libc.Xstrncmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7256, uint64(2)) != 0) || !(libc.Xstrncmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7259, uint64(2)) != 0) || !(libc.Xstrncmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7262, uint64(5)) != 0) || !(libc.Xstrcmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7268) != 0) || !(libc.Xstrcmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7283) != 0) || !(libc.Xstrcmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7296) != 0) || !(libc.Xstrcmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7320) != 0) || !(libc.Xstrcmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7341) != 0) || !(libc.Xstrcmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7362) != 0) || !(libc.Xstrcmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7367) != 0) || !(libc.Xstrcmp(tls, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)), ts+7381) != 0) {
				continue
			}

			if int32(*(*int8)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8))))) == '-' && int32(*(*int8)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)) + 1))) != 0 {
				error(tls, ts+7384, libc.VaList(bp+32, *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8))))
			}

			strarray_push(tls, uintptr(unsafe.Pointer(&input_paths)), *(*uintptr)(unsafe.Pointer(argv + uintptr(i1)*8)))
		}
	}

	{
		var i2 int32 = 0
		for ; i2 < (*StringArray)(unsafe.Pointer(bp+40)).len; i2++ {
			strarray_push(tls, uintptr(unsafe.Pointer(&include_paths)), *(*uintptr)(unsafe.Pointer((*StringArray)(unsafe.Pointer(bp+40 /* &idirafter */)).data + uintptr(i2)*8)))
		}
	}

	if input_paths.len == 0 {
		error(tls, ts+7405, 0)
	}

	// -E implies that the input is the C macro language.
	if opt_E != 0 {
		opt_x = FILE_C
	}
}

func open_file(tls *libc.TLS, path uintptr) uintptr { /* main.c:350:13: */
	bp := tls.Alloc(16)
	defer tls.Free(16)

	if !(path != 0) || libc.Xstrcmp(tls, path, ts+7420) == 0 {
		return libc.Xstdout
	}

	var out uintptr = libc.Xfopen(tls, path, ts+7422)
	if !(out != 0) {
		error(tls, ts+7424, libc.VaList(bp, path, libc.Xstrerror(tls, *(*int32)(unsafe.Pointer(libc.X__errno_location(tls))))))
	}
	return out
}

func endswith(tls *libc.TLS, p uintptr, q uintptr) uint8 { /* main.c:360:13: */
	var len1 int32 = int32(libc.Xstrlen(tls, p))
	var len2 int32 = int32(libc.Xstrlen(tls, q))
	return uint8(libc.Bool32(len1 >= len2 && !(libc.Xstrcmp(tls, p+uintptr(len1)-uintptr(len2), q) != 0)))
}

// Replace file extension
func replace_extn(tls *libc.TLS, tmpl uintptr, extn uintptr) uintptr { /* main.c:367:13: */
	bp := tls.Alloc(16)
	defer tls.Free(16)

	var filename uintptr = xbasename(tls, tmpl)
	var dot uintptr = libc.Xstrrchr(tls, filename, '.')
	if dot != 0 {
		*(*int8)(unsafe.Pointer(dot)) = int8(0)
	}
	return format(tls, ts+7456, libc.VaList(bp, filename, extn))
}

func cleanup(tls *libc.TLS) { /* main.c:375:13: */
	{
		var i int32 = 0
		for ; i < tmpfiles.len; i++ {
			libc.Xunlink(tls, *(*uintptr)(unsafe.Pointer(tmpfiles.data + uintptr(i)*8)))
		}
	}
}

func create_tmpfile(tls *libc.TLS) uintptr { /* main.c:380:13: */
	bp := tls.Alloc(8)
	defer tls.Free(8)

	var path uintptr = libc.Xstrdup(tls, ts+7461)
	var fd int32 = libc.Xmkstemp(tls, path)
	if fd == -1 {
		error(tls, ts+7481, libc.VaList(bp, libc.Xstrerror(tls, *(*int32)(unsafe.Pointer(libc.X__errno_location(tls))))))
	}
	libc.Xclose(tls, fd)

	strarray_push(tls, uintptr(unsafe.Pointer(&tmpfiles)), path)
	return path
}

func run_subprocess(tls *libc.TLS, argv uintptr) { /* main.c:393:13: */
	bp := tls.Alloc(16)
	defer tls.Free(16)

	// If -### is given, dump the subprocess's command line.
	if opt_hash_hash_hash != 0 {
		libc.Xfprintf(tls, libc.Xstderr, ts+7500, libc.VaList(bp, *(*uintptr)(unsafe.Pointer(argv))))
		{
			var i int32 = 1
			for ; *(*uintptr)(unsafe.Pointer(argv + uintptr(i)*8)) != 0; i++ {
				libc.Xfprintf(tls, libc.Xstderr, ts+7503, libc.VaList(bp+8, *(*uintptr)(unsafe.Pointer(argv + uintptr(i)*8))))
			}
		}
		libc.Xfprintf(tls, libc.Xstderr, ts+112, 0)
	}

	run_subprocess_go(tls, argv)
}

func run_cc1(tls *libc.TLS, argc int32, argv uintptr, input uintptr, output uintptr) { /* main.c:405:13: */
	var args uintptr = libc.Xcalloc(tls, uint64(argc+10), uint64(unsafe.Sizeof(uintptr(0))))
	libc.Xmemcpy(tls, args, argv, uint64(argc)*uint64(unsafe.Sizeof(uintptr(0))))
	*(*uintptr)(unsafe.Pointer(args + uintptr(libc.PostIncInt32(&argc, 1))*8)) = ts + 7100 /* "-cc1" */

	if input != 0 {
		*(*uintptr)(unsafe.Pointer(args + uintptr(libc.PostIncInt32(&argc, 1))*8)) = ts + 7197 /* "-cc1-input" */
		*(*uintptr)(unsafe.Pointer(args + uintptr(libc.PostIncInt32(&argc, 1))*8)) = input
	}

	if output != 0 {
		*(*uintptr)(unsafe.Pointer(args + uintptr(libc.PostIncInt32(&argc, 1))*8)) = ts + 7208 /* "-cc1-output" */
		*(*uintptr)(unsafe.Pointer(args + uintptr(libc.PostIncInt32(&argc, 1))*8)) = output
	}

	run_subprocess(tls, args)
}

// Print tokens to stdout. Used for -E.
func print_tokens(tls *libc.TLS, tok uintptr) { /* main.c:424:13: */
	bp := tls.Alloc(16)
	defer tls.Free(16)

	var out uintptr = open_file(tls, func() uintptr {
		if opt_o != 0 {
			return opt_o
		}
		return ts + 7420 /* "-" */
	}())

	var line int32 = 1
	for ; (*Token)(unsafe.Pointer(tok)).kind != TK_EOF; tok = (*Token)(unsafe.Pointer(tok)).next {
		if line > 1 && (*Token)(unsafe.Pointer(tok)).at_bol != 0 {
			libc.Xfprintf(tls, out, ts+112, 0)
		}
		if (*Token)(unsafe.Pointer(tok)).has_space != 0 && !(int32((*Token)(unsafe.Pointer(tok)).at_bol) != 0) {
			libc.Xfprintf(tls, out, ts+7507, 0)
		}
		libc.Xfprintf(tls, out, ts+7509, libc.VaList(bp, (*Token)(unsafe.Pointer(tok)).len, (*Token)(unsafe.Pointer(tok)).loc))
		line++
	}
	libc.Xfprintf(tls, out, ts+112, 0)
}

func in_std_include_path(tls *libc.TLS, path uintptr) uint8 { /* main.c:439:13: */
	{
		var i int32 = 0
		for ; i < std_include_paths.len; i++ {
			var dir uintptr = *(*uintptr)(unsafe.Pointer(std_include_paths.data + uintptr(i)*8))
			var len int32 = int32(libc.Xstrlen(tls, dir))
			if libc.Xstrncmp(tls, dir, path, uint64(len)) == 0 && int32(*(*int8)(unsafe.Pointer(path + uintptr(len)))) == '/' {
				return uint8(1)
			}
		}
	}
	return uint8(0)
}

// If -M options is given, the compiler write a list of input files to
// stdout in a format that "make" command can read. This feature is
// used to automate file dependency management.
func print_dependencies(tls *libc.TLS) { /* main.c:452:13: */
	bp := tls.Alloc(32)
	defer tls.Free(32)

	var path uintptr
	if opt_MF != 0 {
		path = opt_MF
	} else if opt_MD != 0 {
		path = replace_extn(tls, func() uintptr {
			if opt_o != 0 {
				return opt_o
			}
			return base_file
		}(), ts+7514 /* ".d" */)
	} else if opt_o != 0 {
		path = opt_o
	} else {
		path = ts + 7420 /* "-" */
	}

	var out uintptr = open_file(tls, path)
	if opt_MT != 0 {
		libc.Xfprintf(tls, out, ts+5743, libc.VaList(bp, opt_MT))
	} else {
		libc.Xfprintf(tls, out, ts+5743, libc.VaList(bp+8, quote_makefile(tls, replace_extn(tls, base_file, ts+7517))))
	}

	var files uintptr = get_input_files(tls)

	{
		var i int32 = 0
		for ; *(*uintptr)(unsafe.Pointer(files + uintptr(i)*8)) != 0; i++ {
			if opt_MMD != 0 && in_std_include_path(tls, (*File)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(files + uintptr(i)*8)))).name) != 0 {
				continue
			}
			libc.Xfprintf(tls, out, ts+7520, libc.VaList(bp+16, (*File)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(files + uintptr(i)*8)))).name))
		}
	}

	libc.Xfprintf(tls, out, ts+7528, 0)

	if opt_MP != 0 {
		{
			var i int32 = 1
			for ; *(*uintptr)(unsafe.Pointer(files + uintptr(i)*8)) != 0; i++ {
				if opt_MMD != 0 && in_std_include_path(tls, (*File)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(files + uintptr(i)*8)))).name) != 0 {
					continue
				}
				libc.Xfprintf(tls, out, ts+7531, libc.VaList(bp+24, quote_makefile(tls, (*File)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(files + uintptr(i)*8)))).name)))
			}
		}
	}
}

func must_tokenize_file(tls *libc.TLS, path uintptr) uintptr { /* main.c:488:14: */
	bp := tls.Alloc(16)
	defer tls.Free(16)

	var tok uintptr = tokenize_file(tls, path)
	if !(tok != 0) {
		error(tls, ts+7537, libc.VaList(bp, path, libc.Xstrerror(tls, *(*int32)(unsafe.Pointer(libc.X__errno_location(tls))))))
	}
	return tok
}

func append_tokens(tls *libc.TLS, tok1 uintptr, tok2 uintptr) uintptr { /* main.c:495:14: */
	if !(tok1 != 0) || (*Token)(unsafe.Pointer(tok1)).kind == TK_EOF {
		return tok2
	}

	var t uintptr = tok1
	for (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(t)).next)).kind != TK_EOF {
		t = (*Token)(unsafe.Pointer(t)).next
	}
	(*Token)(unsafe.Pointer(t)).next = tok2
	return tok1
}

func cc1(tls *libc.TLS) { /* main.c:506:13: */
	bp := tls.Alloc(16)
	defer tls.Free(16)

	var tok uintptr = uintptr(0)

	// Process -include option
	{
		var i int32 = 0
		for ; i < opt_include.len; i++ {
			var incl uintptr = *(*uintptr)(unsafe.Pointer(opt_include.data + uintptr(i)*8))
			var path uintptr
			if file_exists(tls, incl) != 0 {
				path = incl
			} else {
				path = search_include_paths(tls, incl)
				if !(path != 0) {
					error(tls, ts+7544, libc.VaList(bp, incl, libc.Xstrerror(tls, *(*int32)(unsafe.Pointer(libc.X__errno_location(tls))))))
				}
			}

			var tok2 uintptr = must_tokenize_file(tls, path)
			tok = append_tokens(tls, tok, tok2)
		}
	}

	// Tokenize and parse.
	var tok2 uintptr = must_tokenize_file(tls, base_file)
	tok = append_tokens(tls, tok, tok2)
	tok = preprocess(tls, tok)

	// If -M or -MD are given, print file dependencies.
	if opt_M != 0 || opt_MD != 0 {
		print_dependencies(tls)
		if opt_M != 0 {
			return
		}
	}

	// If -E is given, print out preprocessed C code as a result.
	if opt_E != 0 {
		print_tokens(tls, tok)
		return
	}

	var prog uintptr = parse(tls, tok)

	// Write the assembly output directly to the target file.
	var out uintptr = open_file(tls, output_file1)
	codegen(tls, prog, out)
	libc.Xfclose(tls, out)
}

func assemble(tls *libc.TLS, input uintptr, output uintptr) { /* main.c:552:13: */
	bp := tls.Alloc(48)
	defer tls.Free(48)

	*(*[6]uintptr)(unsafe.Pointer(bp /* cmd */)) = [6]uintptr{ts + 7561, ts + 7136, input, ts + 6913, output, uintptr(0)}
	run_subprocess(tls, bp)
}

func find_file(tls *libc.TLS, pattern uintptr) uintptr { /* main.c:557:13: */
	bp := tls.Alloc(24)
	defer tls.Free(24)

	var star uintptr = libc.Xstrchr(tls, pattern, '*')
	if !(star != 0) {
		if file_exists(tls, pattern) != 0 {
			return libc.Xstrdup(tls, pattern)
		}
		return uintptr(0)
	}

	var dir_len size_t = size_t((int64(star) - int64(pattern)) / 1)
	if dir_len == uint64(0) || int32(*(*int8)(unsafe.Pointer(pattern + uintptr(dir_len-uint64(1))))) != '/' {
		return uintptr(0)
	}
	var dir uintptr = xstrndup(tls, pattern, dir_len-uint64(1))
	var suffix uintptr = star + uintptr(1)

	var dp uintptr = libc.Xopendir(tls, dir)
	if !(dp != 0) {
		libc.Xfree(tls, dir)
		return uintptr(0)
	}
	var ent uintptr
	for libc.AssignUintptr(&ent, libc.Xreaddir(tls, dp)) != 0 {
		if int32(*(*int8)(unsafe.Pointer(ent + 19))) == '.' {
			continue
		}
		var path uintptr = format(tls, ts+7564, libc.VaList(bp, dir, ent+19, suffix))
		if file_exists(tls, path) != 0 {
			libc.Xclosedir(tls, dp)
			libc.Xfree(tls, dir)
			return path
		}
		libc.Xfree(tls, path)
	}

	libc.Xclosedir(tls, dp)
	libc.Xfree(tls, dir)
	return uintptr(0)
}

// Returns true if a given file exists.
func file_exists(tls *libc.TLS, path uintptr) uint8 { /* main.c:596:6: */
	bp := tls.Alloc(144)
	defer tls.Free(144)

	// var st stat at bp, 144

	return libc.BoolUint8(!(libc.Xstat(tls, path, bp) != 0))
}

func find_libpath(tls *libc.TLS) uintptr { /* main.c:601:13: */
	if file_exists(tls, ts+7572) != 0 {
		return ts + 7605 /* "/usr/lib/x86_64-..." */
	}
	if file_exists(tls, ts+7631) != 0 {
		return ts + 7649 /* "/usr/lib64" */
	}
	error(tls, ts+7660, 0)
	return uintptr(0)
}

func find_gcc_libpath(tls *libc.TLS) uintptr { /* main.c:609:13: */
	bp := tls.Alloc(24)
	defer tls.Free(24)

	*(*[3]uintptr)(unsafe.Pointer(bp /* paths */)) = [3]uintptr{
		ts + 7686,
		ts + 7729,
		ts + 7775,
	}

	{
		var i int32 = 0
		for ; uint64(i) < uint64(unsafe.Sizeof([3]uintptr{}))/uint64(unsafe.Sizeof(uintptr(0))); i++ {
			var path uintptr = find_file(tls, *(*uintptr)(unsafe.Pointer(bp + uintptr(i)*8)))
			if path != 0 {
				return xdirname(tls, path)
			}
		}
	}

	error(tls, ts+7821, 0)
	return uintptr(0)
}

func run_linker(tls *libc.TLS, inputs uintptr, output uintptr) { /* main.c:625:13: */
	bp := tls.Alloc(88)
	defer tls.Free(88)

	*(*StringArray)(unsafe.Pointer(bp + 72 /* arr */)) = StringArray{}

	strarray_push(tls, bp+72, ts+7851)
	strarray_push(tls, bp+72, ts+6913)
	strarray_push(tls, bp+72, output)
	strarray_push(tls, bp+72, ts+7854)
	strarray_push(tls, bp+72, ts+7857)

	var libpath uintptr = find_libpath(tls)
	var gcc_libpath uintptr = find_gcc_libpath(tls)

	if opt_shared != 0 {
		strarray_push(tls, bp+72, format(tls, ts+7868, libc.VaList(bp, libpath)))
		strarray_push(tls, bp+72, format(tls, ts+7878, libc.VaList(bp+8, gcc_libpath)))
	} else {
		strarray_push(tls, bp+72, format(tls, ts+7893, libc.VaList(bp+16, libpath)))
		strarray_push(tls, bp+72, format(tls, ts+7868, libc.VaList(bp+24, libpath)))
		strarray_push(tls, bp+72, format(tls, ts+7903, libc.VaList(bp+32, gcc_libpath)))
	}

	strarray_push(tls, bp+72, format(tls, ts+7917, libc.VaList(bp+40, gcc_libpath)))
	strarray_push(tls, bp+72, ts+7922)
	strarray_push(tls, bp+72, ts+7950)
	strarray_push(tls, bp+72, ts+7963)
	strarray_push(tls, bp+72, ts+7922)
	strarray_push(tls, bp+72, ts+7972)
	strarray_push(tls, bp+72, ts+8003)
	strarray_push(tls, bp+72, ts+8034)
	strarray_push(tls, bp+72, ts+8045)

	if !(opt_static != 0) {
		strarray_push(tls, bp+72, ts+8052)
		strarray_push(tls, bp+72, ts+8068)
	}

	{
		var i int32 = 0
		for ; i < ld_extra_args.len; i++ {
			strarray_push(tls, bp+72, *(*uintptr)(unsafe.Pointer(ld_extra_args.data + uintptr(i)*8)))
		}
	}

	{
		var i1 int32 = 0
		for ; i1 < (*StringArray)(unsafe.Pointer(inputs)).len; i1++ {
			strarray_push(tls, bp+72, *(*uintptr)(unsafe.Pointer((*StringArray)(unsafe.Pointer(inputs)).data + uintptr(i1)*8)))
		}
	}

	if opt_static != 0 {
		strarray_push(tls, bp+72, ts+8096)
		strarray_push(tls, bp+72, ts+8110)
		strarray_push(tls, bp+72, ts+8116)
		strarray_push(tls, bp+72, ts+8125)
		strarray_push(tls, bp+72, ts+8129)
	} else {
		strarray_push(tls, bp+72, ts+8125)
		strarray_push(tls, bp+72, ts+8110)
		strarray_push(tls, bp+72, ts+8141)
		strarray_push(tls, bp+72, ts+8153)
		strarray_push(tls, bp+72, ts+8161)
	}

	if opt_shared != 0 {
		strarray_push(tls, bp+72, format(tls, ts+8176, libc.VaList(bp+48, gcc_libpath)))
	} else {
		strarray_push(tls, bp+72, format(tls, ts+8189, libc.VaList(bp+56, gcc_libpath)))
	}

	strarray_push(tls, bp+72, format(tls, ts+8201, libc.VaList(bp+64, libpath)))
	strarray_push(tls, bp+72, uintptr(0))

	run_subprocess(tls, (*StringArray)(unsafe.Pointer(bp+72 /* &arr */)).data)
}

func get_file_type(tls *libc.TLS, filename uintptr) FileType { /* main.c:692:17: */
	bp := tls.Alloc(8)
	defer tls.Free(8)

	if opt_x != FILE_NONE {
		return opt_x
	}

	if endswith(tls, filename, ts+8211) != 0 {
		return FILE_AR
	}
	if endswith(tls, filename, ts+8214) != 0 {
		return FILE_DSO
	}
	if endswith(tls, filename, ts+7517) != 0 {
		return FILE_OBJ
	}
	if endswith(tls, filename, ts+8218) != 0 {
		return FILE_C
	}
	if endswith(tls, filename, ts+8221) != 0 {
		return FILE_ASM
	}

	error(tls, ts+8224, libc.VaList(bp, filename))
	return FileType(0)
}

func main1(tls *libc.TLS, argc int32, argv uintptr) int32 { /* main.c:710:5: */
	bp := tls.Alloc(16)
	defer tls.Free(16)

	libc.Xatexit(tls, *(*uintptr)(unsafe.Pointer(&struct{ f func(*libc.TLS) }{cleanup})))
	init_macros(tls)
	parse_args(tls, argc, argv)

	if opt_cc1 != 0 {
		add_default_include_paths(tls, *(*uintptr)(unsafe.Pointer(argv)))
		cc1(tls)
		return 0
	}

	if input_paths.len > 1 && opt_o != 0 && (opt_c != 0 || opt_S|opt_E != 0) {
		error(tls, ts+8267, 0)
	}

	*(*StringArray)(unsafe.Pointer(bp /* ld_args */)) = StringArray{}

	{
		var i int32 = 0
		for ; i < input_paths.len; i++ {
			var input uintptr = *(*uintptr)(unsafe.Pointer(input_paths.data + uintptr(i)*8))

			if !(libc.Xstrncmp(tls, input, ts+7148, uint64(2)) != 0) {
				strarray_push(tls, bp, input)
				continue
			}

			if !(libc.Xstrncmp(tls, input, ts+7151, uint64(4)) != 0) {
				var s uintptr = libc.Xstrdup(tls, input+uintptr(4))
				var arg uintptr = libc.Xstrtok(tls, s, ts+8331)
				for arg != 0 {
					strarray_push(tls, bp, arg)
					arg = libc.Xstrtok(tls, uintptr(0), ts+8331)
				}
				continue
			}
			var output uintptr
			if opt_o != 0 {
				output = opt_o
			} else if opt_S != 0 {
				output = replace_extn(tls, input, ts+8221)
			} else {
				output = replace_extn(tls, input, ts+7517)
			}

			var type1 FileType = get_file_type(tls, input)

			// Handle .o or .a
			if type1 == FILE_OBJ || type1 == FILE_AR || type1 == FILE_DSO {
				strarray_push(tls, bp, input)
				continue
			}

			// Handle .s
			if type1 == FILE_ASM {
				if !(opt_S != 0) {
					assemble(tls, input, output)
				}
				continue
			}

			if type1 == FILE_C {
			} else {
				libc.X__assert_fail(tls, ts+8333, ts+8348, uint32(767), uintptr(unsafe.Pointer(&__func__5)))
			}

			// Just preprocess
			if opt_E != 0 || opt_M != 0 {
				run_cc1(tls, argc, argv, input, uintptr(0))
				continue
			}

			// Compile
			if opt_S != 0 {
				run_cc1(tls, argc, argv, input, output)
				continue
			}

			// Compile and assemble
			if opt_c != 0 {
				var tmp uintptr = create_tmpfile(tls)
				run_cc1(tls, argc, argv, input, tmp)
				assemble(tls, tmp, output)
				continue
			}

			// Compile, assemble and link
			var tmp1 uintptr = create_tmpfile(tls)
			var tmp2 uintptr = create_tmpfile(tls)
			run_cc1(tls, argc, argv, input, tmp1)
			assemble(tls, tmp1, tmp2)
			strarray_push(tls, bp, tmp2)
			continue
		}
	}

	if (*StringArray)(unsafe.Pointer(bp)).len > 0 {
		run_linker(tls, bp, func() uintptr {
			if opt_o != 0 {
				return opt_o
			}
			return ts + 8355 /* "a.out" */
		}())
	}
	return 0
}

var __func__5 = *(*[5]int8)(unsafe.Pointer(ts + 6626)) /* main.c:710:33 */

// Scope for local variables, global variables, typedefs
// or enum constants
type VarScope = struct {
	__var    uintptr
	type_def uintptr
	enum_ty  uintptr
	enum_val int32
	_        [4]byte
} /* parse.c:28:3 */

// Represents a block scope.
type Scope1 = struct {
	next uintptr
	vars HashMap
	tags HashMap
} /* parse.c:31:9 */

// Represents a block scope.
type Scope = Scope1 /* parse.c:31:22 */

// Variable attributes such as typedef or extern.
type VarAttr = struct {
	is_typedef uint8
	is_static  uint8
	is_extern  uint8
	is_inline  uint8
	is_tls     uint8
	_          [3]byte
	align      int32
} /* parse.c:49:3 */

// This struct represents a variable initializer. Since initializers
// can be nested (e.g. `int x[2][2] = {{1, 2}, {3, 4}}`), this struct
// is a tree data structure.
type Initializer1 = struct {
	next        uintptr
	ty          uintptr
	tok         uintptr
	is_flexible uint8
	_           [7]byte
	expr        uintptr
	children    uintptr
	mem         uintptr
} /* parse.c:54:9 */

// This struct represents a variable initializer. Since initializers
// can be nested (e.g. `int x[2][2] = {{1, 2}, {3, 4}}`), this struct
// is a tree data structure.
type Initializer = Initializer1 /* parse.c:54:28 */

// For local variable initializer.
type InitDesg1 = struct {
	next   uintptr
	idx    int32
	_      [4]byte
	member uintptr
	__var  uintptr
} /* parse.c:75:9 */

// For local variable initializer.
type InitDesg = InitDesg1 /* parse.c:75:25 */

// All local variable instances created during parsing are
// accumulated to this list.
var locals uintptr /* parse.c:85:12: */

// Likewise, global variables are accumulated to this list.
var globals uintptr /* parse.c:88:12: */

var scope_storage Scope /* parse.c:90:14: */
var scope uintptr = 0   /* parse.c:91:14 */

// Points to the function object the parser is currently parsing.
var current_fn1 uintptr /* parse.c:94:12: */

// Lists of all goto statements and labels in the curent function.
var gotos uintptr  /* parse.c:97:13: */
var labels uintptr /* parse.c:98:13: */

// Current "goto" and "continue" jump targets.
var brk_label uintptr  /* parse.c:101:13: */
var cont_label uintptr /* parse.c:102:13: */

// Points to a node representing a switch if we are parsing
// a switch statement. Otherwise, NULL.
var current_switch uintptr /* parse.c:106:13: */

var builtin_alloca1 uintptr /* parse.c:108:12: */

func align_down(tls *libc.TLS, n int32, align int32) int32 { /* parse.c:160:12: */
	return align_to(tls, n-align+1, align)
}

func enter_scope(tls *libc.TLS) { /* parse.c:164:13: */
	var sc uintptr = libc.Xcalloc(tls, uint64(1), uint64(unsafe.Sizeof(Scope{})))
	(*Scope)(unsafe.Pointer(sc)).next = scope
	scope = sc
}

func leave_scope(tls *libc.TLS) { /* parse.c:170:13: */
	scope = (*Scope)(unsafe.Pointer(scope)).next
}

// Find a variable by name.
func find_var(tls *libc.TLS, tok uintptr) uintptr { /* parse.c:175:17: */
	{
		var sc uintptr = scope
		for ; sc != 0; sc = (*Scope)(unsafe.Pointer(sc)).next {
			var sc2 uintptr = hashmap_get2(tls, sc+8, (*Token)(unsafe.Pointer(tok)).loc, (*Token)(unsafe.Pointer(tok)).len)
			if sc2 != 0 {
				return sc2
			}
		}
	}
	return uintptr(0)
}

func find_tag(tls *libc.TLS, tok uintptr) uintptr { /* parse.c:184:13: */
	{
		var sc uintptr = scope
		for ; sc != 0; sc = (*Scope)(unsafe.Pointer(sc)).next {
			var ty uintptr = hashmap_get2(tls, sc+24, (*Token)(unsafe.Pointer(tok)).loc, (*Token)(unsafe.Pointer(tok)).len)
			if ty != 0 {
				return ty
			}
		}
	}
	return uintptr(0)
}

func new_node(tls *libc.TLS, kind NodeKind, tok uintptr) uintptr { /* parse.c:193:13: */
	var node uintptr = libc.Xcalloc(tls, uint64(1), uint64(unsafe.Sizeof(Node{})))
	(*Node)(unsafe.Pointer(node)).kind = kind
	(*Node)(unsafe.Pointer(node)).tok = tok
	return node
}

func new_binary(tls *libc.TLS, kind NodeKind, lhs uintptr, rhs uintptr, tok uintptr) uintptr { /* parse.c:200:13: */
	var node uintptr = new_node(tls, kind, tok)
	(*Node)(unsafe.Pointer(node)).lhs = lhs
	(*Node)(unsafe.Pointer(node)).rhs = rhs
	return node
}

func new_unary(tls *libc.TLS, kind NodeKind, expr uintptr, tok uintptr) uintptr { /* parse.c:207:13: */
	var node uintptr = new_node(tls, kind, tok)
	(*Node)(unsafe.Pointer(node)).lhs = expr
	return node
}

func new_num(tls *libc.TLS, val int64_t, tok uintptr) uintptr { /* parse.c:213:13: */
	var node uintptr = new_node(tls, ND_NUM, tok)
	(*Node)(unsafe.Pointer(node)).val = val
	return node
}

func new_long(tls *libc.TLS, val int64_t, tok uintptr) uintptr { /* parse.c:219:13: */
	var node uintptr = new_node(tls, ND_NUM, tok)
	(*Node)(unsafe.Pointer(node)).val = val
	(*Node)(unsafe.Pointer(node)).ty = ty_long
	return node
}

func new_ulong(tls *libc.TLS, val int64, tok uintptr) uintptr { /* parse.c:226:13: */
	var node uintptr = new_node(tls, ND_NUM, tok)
	(*Node)(unsafe.Pointer(node)).val = val
	(*Node)(unsafe.Pointer(node)).ty = ty_ulong
	return node
}

func new_var_node(tls *libc.TLS, var1 uintptr, tok uintptr) uintptr { /* parse.c:233:13: */
	var node uintptr = new_node(tls, ND_VAR, tok)
	(*Node)(unsafe.Pointer(node)).__var = var1
	return node
}

func new_vla_ptr(tls *libc.TLS, var1 uintptr, tok uintptr) uintptr { /* parse.c:239:13: */
	var node uintptr = new_node(tls, ND_VLA_PTR, tok)
	(*Node)(unsafe.Pointer(node)).__var = var1
	return node
}

func new_cast(tls *libc.TLS, expr uintptr, ty uintptr) uintptr { /* parse.c:245:6: */
	add_type(tls, expr)

	var node uintptr = libc.Xcalloc(tls, uint64(1), uint64(unsafe.Sizeof(Node{})))
	(*Node)(unsafe.Pointer(node)).kind = ND_CAST
	(*Node)(unsafe.Pointer(node)).tok = (*Node)(unsafe.Pointer(expr)).tok
	(*Node)(unsafe.Pointer(node)).lhs = expr
	(*Node)(unsafe.Pointer(node)).ty = copy_type(tls, ty)
	return node
}

func push_scope(tls *libc.TLS, name uintptr) uintptr { /* parse.c:256:17: */
	var sc uintptr = libc.Xcalloc(tls, uint64(1), uint64(unsafe.Sizeof(VarScope{})))
	hashmap_put(tls, scope+8, name, sc)
	return sc
}

func new_initializer(tls *libc.TLS, ty uintptr, is_flexible uint8) uintptr { /* parse.c:262:20: */
	var init1 uintptr = libc.Xcalloc(tls, uint64(1), uint64(unsafe.Sizeof(Initializer{})))
	(*Initializer)(unsafe.Pointer(init1)).ty = ty

	if (*Type)(unsafe.Pointer(ty)).kind == TY_ARRAY {
		if is_flexible != 0 && (*Type)(unsafe.Pointer(ty)).size < 0 {
			(*Initializer)(unsafe.Pointer(init1)).is_flexible = uint8(1)
			return init1
		}

		(*Initializer)(unsafe.Pointer(init1)).children = libc.Xcalloc(tls, uint64((*Type)(unsafe.Pointer(ty)).array_len), uint64(unsafe.Sizeof(uintptr(0))))
		{
			var i int32 = 0
			for ; i < (*Type)(unsafe.Pointer(ty)).array_len; i++ {
				*(*uintptr)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).children + uintptr(i)*8)) = new_initializer(tls, (*Type)(unsafe.Pointer(ty)).base, uint8(0))
			}
		}
		return init1
	}

	if (*Type)(unsafe.Pointer(ty)).kind == TY_STRUCT || (*Type)(unsafe.Pointer(ty)).kind == TY_UNION {
		// Count the number of struct members.
		var len int32 = 0
		{
			var mem uintptr = (*Type)(unsafe.Pointer(ty)).members
			for ; mem != 0; mem = (*Member)(unsafe.Pointer(mem)).next {
				len++
			}
		}

		(*Initializer)(unsafe.Pointer(init1)).children = libc.Xcalloc(tls, uint64(len), uint64(unsafe.Sizeof(uintptr(0))))

		{
			var mem1 uintptr = (*Type)(unsafe.Pointer(ty)).members
			for ; mem1 != 0; mem1 = (*Member)(unsafe.Pointer(mem1)).next {
				if is_flexible != 0 && (*Type)(unsafe.Pointer(ty)).is_flexible != 0 && !(int32((*Member)(unsafe.Pointer(mem1)).next) != 0) {
					var child uintptr = libc.Xcalloc(tls, uint64(1), uint64(unsafe.Sizeof(Initializer{})))
					(*Initializer)(unsafe.Pointer(child)).ty = (*Member)(unsafe.Pointer(mem1)).ty
					(*Initializer)(unsafe.Pointer(child)).is_flexible = uint8(1)
					*(*uintptr)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).children + uintptr((*Member)(unsafe.Pointer(mem1)).idx)*8)) = child
				} else {
					*(*uintptr)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).children + uintptr((*Member)(unsafe.Pointer(mem1)).idx)*8)) = new_initializer(tls, (*Member)(unsafe.Pointer(mem1)).ty, uint8(0))
				}
			}
		}
		return init1
	}

	return init1
}

func new_var(tls *libc.TLS, name uintptr, ty uintptr) uintptr { /* parse.c:302:12: */
	var var1 uintptr = libc.Xcalloc(tls, uint64(1), uint64(unsafe.Sizeof(Obj{})))
	(*Obj)(unsafe.Pointer(var1)).name = name
	(*Obj)(unsafe.Pointer(var1)).ty = ty
	(*Obj)(unsafe.Pointer(var1)).align = (*Type)(unsafe.Pointer(ty)).align
	(*VarScope)(unsafe.Pointer(push_scope(tls, name))).__var = var1
	return var1
}

func new_lvar(tls *libc.TLS, name uintptr, ty uintptr) uintptr { /* parse.c:311:12: */
	var var1 uintptr = new_var(tls, name, ty)
	(*Obj)(unsafe.Pointer(var1)).is_local = uint8(1)
	(*Obj)(unsafe.Pointer(var1)).next = locals
	locals = var1
	return var1
}

func new_gvar(tls *libc.TLS, name uintptr, ty uintptr) uintptr { /* parse.c:319:12: */
	var var1 uintptr = new_var(tls, name, ty)
	(*Obj)(unsafe.Pointer(var1)).next = globals
	(*Obj)(unsafe.Pointer(var1)).is_static = uint8(1)
	(*Obj)(unsafe.Pointer(var1)).is_definition = uint8(1)
	globals = var1
	return var1
}

func new_unique_name(tls *libc.TLS) uintptr { /* parse.c:328:13: */
	bp := tls.Alloc(8)
	defer tls.Free(8)

	return format(tls, ts+8361, libc.VaList(bp, libc.PostIncInt32(&id, 1)))
}

var id int32 = 0 /* parse.c:329:14 */

func new_anon_gvar(tls *libc.TLS, ty uintptr) uintptr { /* parse.c:333:12: */
	return new_gvar(tls, new_unique_name(tls), ty)
}

func new_string_literal(tls *libc.TLS, p uintptr, ty uintptr) uintptr { /* parse.c:337:12: */
	var var1 uintptr = new_anon_gvar(tls, ty)
	(*Obj)(unsafe.Pointer(var1)).init_data = p
	return var1
}

func get_ident(tls *libc.TLS, tok uintptr) uintptr { /* parse.c:343:13: */
	if (*Token)(unsafe.Pointer(tok)).kind != TK_IDENT {
		error_tok(tls, tok, ts+8368, 0)
	}
	return xstrndup(tls, (*Token)(unsafe.Pointer(tok)).loc, uint64((*Token)(unsafe.Pointer(tok)).len))
}

func find_typedef(tls *libc.TLS, tok uintptr) uintptr { /* parse.c:349:13: */
	if (*Token)(unsafe.Pointer(tok)).kind == TK_IDENT {
		var sc uintptr = find_var(tls, tok)
		if sc != 0 {
			return (*VarScope)(unsafe.Pointer(sc)).type_def
		}
	}
	return uintptr(0)
}

func push_tag_scope(tls *libc.TLS, tok uintptr, ty uintptr) { /* parse.c:358:13: */
	hashmap_put2(tls, scope+24, (*Token)(unsafe.Pointer(tok)).loc, (*Token)(unsafe.Pointer(tok)).len, ty)
}

// declspec = ("void" | "_Bool" | "char" | "short" | "int" | "long"
//
//	| "typedef" | "static" | "extern" | "inline"
//	| "_Thread_local" | "__thread"
//	| "signed" | "unsigned"
//	| struct-decl | union-decl | typedef-name
//	| enum-specifier | typeof-specifier
//	| "const" | "volatile" | "auto" | "register" | "restrict"
//	| "__restrict" | "__restrict__" | "_Noreturn")+
//
// The order of typenames in a type-specifier doesn't matter. For
// example, `int long static` means the same as `static long int`.
// That can also be written as `static long` because you can omit
// `int` if `long` or `short` are specified. However, something like
// `char int` is not a valid type specifier. We have to accept only a
// limited combinations of the typenames.
//
// In this function, we count the number of occurrences of each typename
// while keeping the "current" type object that the typenames up
// until that point represent. When we reach a non-typename token,
// we returns the current type object.
func declspec(tls *libc.TLS, rest uintptr, tok uintptr, attr uintptr) uintptr { /* parse.c:382:13: */
	bp := tls.Alloc(24)
	defer tls.Free(24)
	*(*uintptr)(unsafe.Pointer(bp + 16)) = tok

	var ty uintptr = ty_int
	var counter int32 = 0
	var is_atomic uint8 = uint8(0)

	for is_typename(tls, *(*uintptr)(unsafe.Pointer(bp + 16))) != 0 {
		// Handle storage class specifiers.
		if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 16)), ts+8391) != 0 || equal(tls, *(*uintptr)(unsafe.Pointer(bp + 16)), ts+8399) != 0 || equal(tls, *(*uintptr)(unsafe.Pointer(bp + 16)), ts+8406) != 0 || equal(tls, *(*uintptr)(unsafe.Pointer(bp + 16)), ts+8413) != 0 || equal(tls, *(*uintptr)(unsafe.Pointer(bp + 16)), ts+8420) != 0 || equal(tls, *(*uintptr)(unsafe.Pointer(bp + 16)), ts+8434) != 0 {
			if !(attr != 0) {
				error_tok(tls, *(*uintptr)(unsafe.Pointer(bp + 16 /* tok */)), ts+8443, 0)
			}

			if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 16)), ts+8391) != 0 {
				(*VarAttr)(unsafe.Pointer(attr)).is_typedef = uint8(1)
			} else if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 16)), ts+8399) != 0 {
				(*VarAttr)(unsafe.Pointer(attr)).is_static = uint8(1)
			} else if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 16)), ts+8406) != 0 {
				(*VarAttr)(unsafe.Pointer(attr)).is_extern = uint8(1)
			} else if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 16)), ts+8413) != 0 {
				(*VarAttr)(unsafe.Pointer(attr)).is_inline = uint8(1)
			} else {
				(*VarAttr)(unsafe.Pointer(attr)).is_tls = uint8(1)
			}

			if (*VarAttr)(unsafe.Pointer(attr)).is_typedef != 0 && int32((*VarAttr)(unsafe.Pointer(attr)).is_static+(*VarAttr)(unsafe.Pointer(attr)).is_extern+(*VarAttr)(unsafe.Pointer(attr)).is_inline+(*VarAttr)(unsafe.Pointer(attr)).is_tls) > 1 {
				error_tok(tls, *(*uintptr)(unsafe.Pointer(bp + 16 /* tok */)),
					ts+8498, 0)
			}
			*(*uintptr)(unsafe.Pointer(bp + 16 /* tok */)) = (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 16 /* tok */)))).next
			continue
		}

		// These keywords are recognized but ignored.
		if consume(tls, bp+16, *(*uintptr)(unsafe.Pointer(bp + 16)), ts+8586) != 0 || consume(tls, bp+16, *(*uintptr)(unsafe.Pointer(bp + 16)), ts+8592) != 0 || consume(tls, bp+16, *(*uintptr)(unsafe.Pointer(bp + 16)), ts+8601) != 0 || consume(tls, bp+16, *(*uintptr)(unsafe.Pointer(bp + 16)), ts+8606) != 0 || consume(tls, bp+16, *(*uintptr)(unsafe.Pointer(bp + 16)), ts+8615) != 0 || consume(tls, bp+16, *(*uintptr)(unsafe.Pointer(bp + 16)), ts+8624) != 0 || consume(tls, bp+16, *(*uintptr)(unsafe.Pointer(bp + 16)), ts+8635) != 0 || consume(tls, bp+16, *(*uintptr)(unsafe.Pointer(bp + 16)), ts+8648) != 0 {
			continue
		}

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 16)), ts+8658) != 0 {
			*(*uintptr)(unsafe.Pointer(bp + 16 /* tok */)) = (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 16 /* tok */)))).next
			if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 16)), ts+8666) != 0 {
				ty = typename(tls, bp+16, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 16 /* tok */)))).next)
				*(*uintptr)(unsafe.Pointer(bp + 16 /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp + 16 /* tok */)), ts+8668)
			}
			is_atomic = uint8(1)
			continue
		}

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 16)), ts+8670) != 0 {
			if !(attr != 0) {
				error_tok(tls, *(*uintptr)(unsafe.Pointer(bp + 16 /* tok */)), ts+8679, 0)
			}
			*(*uintptr)(unsafe.Pointer(bp + 16 /* tok */)) = skip(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 16 /* tok */)))).next, ts+8666)

			if is_typename(tls, *(*uintptr)(unsafe.Pointer(bp + 16))) != 0 {
				(*VarAttr)(unsafe.Pointer(attr)).align = (*Type)(unsafe.Pointer(typename(tls, bp+16, *(*uintptr)(unsafe.Pointer(bp + 16 /* tok */))))).align
			} else {
				(*VarAttr)(unsafe.Pointer(attr)).align = int32(const_expr(tls, bp+16, *(*uintptr)(unsafe.Pointer(bp + 16 /* tok */))))
			}
			*(*uintptr)(unsafe.Pointer(bp + 16 /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp + 16 /* tok */)), ts+8668)
			continue
		}

		// Handle user-defined types.
		var ty2 uintptr = find_typedef(tls, *(*uintptr)(unsafe.Pointer(bp + 16 /* tok */)))
		if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 16)), ts+8719) != 0 || equal(tls, *(*uintptr)(unsafe.Pointer(bp + 16)), ts+8726) != 0 || equal(tls, *(*uintptr)(unsafe.Pointer(bp + 16)), ts+8732) != 0 || equal(tls, *(*uintptr)(unsafe.Pointer(bp + 16)), ts+8737) != 0 || ty2 != 0 {
			if counter != 0 {
				break
			}

			if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 16)), ts+8719) != 0 {
				ty = struct_decl(tls, bp+16, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 16 /* tok */)))).next)
			} else if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 16)), ts+8726) != 0 {
				ty = union_decl(tls, bp+16, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 16 /* tok */)))).next)
			} else if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 16)), ts+8732) != 0 {
				ty = enum_specifier(tls, bp+16, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 16 /* tok */)))).next)
			} else if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 16)), ts+8737) != 0 {
				ty = typeof_specifier(tls, bp+16, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 16 /* tok */)))).next)
			} else {
				ty = ty2
				*(*uintptr)(unsafe.Pointer(bp + 16 /* tok */)) = (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 16 /* tok */)))).next
			}

			counter = counter + 65536
			continue
		}

		// Handle built-in types.
		if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 16)), ts+8744) != 0 {
			counter = counter + 1
		} else if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 16)), ts+8749) != 0 {
			counter = counter + 4
		} else if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 16)), ts+8755) != 0 {
			counter = counter + 16
		} else if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 16)), ts+8760) != 0 {
			counter = counter + 64
		} else if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 16)), ts+8766) != 0 {
			counter = counter + 256
		} else if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 16)), ts+8770) != 0 {
			counter = counter + 1024
		} else if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 16)), ts+8775) != 0 {
			counter = counter + 4096
		} else if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 16)), ts+8781) != 0 {
			counter = counter + 16384
		} else if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 16)), ts+8788) != 0 {
			counter = counter | 131072
		} else if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 16)), ts+8795) != 0 {
			counter = counter | 262144
		} else {
			error(tls, ts+217, libc.VaList(bp, ts+8804, 507))
		}

		switch counter {
		case 1 /* VOID */ :
			ty = ty_void
			break
		case 4 /* BOOL */ :
			ty = ty_bool
			break
		case 16 /* CHAR */ :
			fallthrough
		case 131072 + 16:
			ty = ty_char
			break
		case 262144 + 16:
			ty = ty_uchar
			break
		case 64 /* SHORT */ :
			fallthrough
		case 64 + 256:
			fallthrough
		case 131072 + 64:
			fallthrough
		case 131072 + 64 + 256:
			ty = ty_short
			break
		case 262144 + 64:
			fallthrough
		case 262144 + 64 + 256:
			ty = ty_ushort
			break
		case 256 /* INT */ :
			fallthrough
		case 131072 /* SIGNED */ :
			fallthrough
		case 131072 + 256:
			ty = ty_int
			break
		case 262144 /* UNSIGNED */ :
			fallthrough
		case 262144 + 256:
			ty = ty_uint
			break
		case 1024 /* LONG */ :
			fallthrough
		case 1024 + 256:
			fallthrough
		case 1024 + 1024:
			fallthrough
		case 1024 + 1024 + 256:
			fallthrough
		case 131072 + 1024:
			fallthrough
		case 131072 + 1024 + 256:
			fallthrough
		case 131072 + 1024 + 1024:
			fallthrough
		case 131072 + 1024 + 1024 + 256:
			ty = ty_long
			break
		case 262144 + 1024:
			fallthrough
		case 262144 + 1024 + 256:
			fallthrough
		case 262144 + 1024 + 1024:
			fallthrough
		case 262144 + 1024 + 1024 + 256:
			ty = ty_ulong
			break
		case 4096 /* FLOAT */ :
			ty = ty_float
			break
		case 16384 /* DOUBLE */ :
			ty = ty_double
			break
		case 1024 + 16384:
			ty = ty_ldouble
			break
		default:
			error_tok(tls, *(*uintptr)(unsafe.Pointer(bp + 16 /* tok */)), ts+8812, 0)
		}

		*(*uintptr)(unsafe.Pointer(bp + 16 /* tok */)) = (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 16 /* tok */)))).next
	}

	if is_atomic != 0 {
		ty = copy_type(tls, ty)
		(*Type)(unsafe.Pointer(ty)).is_atomic = uint8(1)
	}

	*(*uintptr)(unsafe.Pointer(rest)) = *(*uintptr)(unsafe.Pointer(bp + 16 /* tok */))
	return ty
}

// func-params = ("void" | param ("," param)* ("," "...")?)? ")"
// param       = declspec declarator
func func_params(tls *libc.TLS, rest uintptr, tok uintptr, ty uintptr) uintptr { /* parse.c:585:13: */
	bp := tls.Alloc(128)
	defer tls.Free(128)
	*(*uintptr)(unsafe.Pointer(bp + 120)) = tok

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 120)), ts+8744) != 0 && equal(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 120)))).next, ts+8668) != 0 {
		*(*uintptr)(unsafe.Pointer(rest)) = (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 120 /* tok */)))).next)).next
		return func_type(tls, ty)
	}

	*(*Type)(unsafe.Pointer(bp /* head */)) = Type{}
	var cur uintptr = bp /* &head */
	var is_variadic uint8 = uint8(0)

	for !(equal(tls, *(*uintptr)(unsafe.Pointer(bp + 120)), ts+8668) != 0) {
		if cur != bp {
			*(*uintptr)(unsafe.Pointer(bp + 120 /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp + 120 /* tok */)), ts+8331)
		}

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 120)), ts+8825) != 0 {
			is_variadic = uint8(1)
			*(*uintptr)(unsafe.Pointer(bp + 120 /* tok */)) = (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 120 /* tok */)))).next
			skip(tls, *(*uintptr)(unsafe.Pointer(bp + 120 /* tok */)), ts+8668)
			break
		}

		var ty2 uintptr = declspec(tls, bp+120, *(*uintptr)(unsafe.Pointer(bp + 120 /* tok */)), uintptr(0))
		ty2 = declarator(tls, bp+120, *(*uintptr)(unsafe.Pointer(bp + 120 /* tok */)), ty2)

		var name uintptr = (*Type)(unsafe.Pointer(ty2)).name

		if (*Type)(unsafe.Pointer(ty2)).kind == TY_ARRAY {
			// "array of T" is converted to "pointer to T" only in the parameter
			// context. For example, *argv[] is converted to **argv by this.
			ty2 = pointer_to(tls, (*Type)(unsafe.Pointer(ty2)).base)
			(*Type)(unsafe.Pointer(ty2)).name = name
		} else if (*Type)(unsafe.Pointer(ty2)).kind == TY_FUNC {
			// Likewise, a function is converted to a pointer to a function
			// only in the parameter context.
			ty2 = pointer_to(tls, ty2)
			(*Type)(unsafe.Pointer(ty2)).name = name
		}

		cur = libc.AssignPtrUintptr(cur+112, copy_type(tls, ty2))
	}

	if cur == bp {
		is_variadic = uint8(1)
	}

	ty = func_type(tls, ty)
	(*Type)(unsafe.Pointer(ty)).params = (*Type)(unsafe.Pointer(bp /* &head */)).next
	(*Type)(unsafe.Pointer(ty)).is_variadic = is_variadic
	*(*uintptr)(unsafe.Pointer(rest)) = (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 120 /* tok */)))).next
	return ty
}

// array-dimensions = ("static" | "restrict")* const-expr? "]" type-suffix
func array_dimensions(tls *libc.TLS, rest uintptr, tok uintptr, ty uintptr) uintptr { /* parse.c:637:13: */
	bp := tls.Alloc(8)
	defer tls.Free(8)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	for equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8399) != 0 || equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8615) != 0 {
		*(*uintptr)(unsafe.Pointer(bp /* tok */)) = (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next
	}

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8829) != 0 {
		ty = type_suffix(tls, rest, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next, ty)
		return array_of(tls, ty, -1)
	}

	var expr uintptr = conditional(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
	*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8829)
	ty = type_suffix(tls, rest, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ty)

	if (*Type)(unsafe.Pointer(ty)).kind == TY_VLA || !(is_const_expr(tls, expr) != 0) {
		return vla_of(tls, ty, expr)
	}
	return array_of(tls, ty, int32(eval(tls, expr)))
}

// type-suffix = "(" func-params
//
//	| "[" array-dimensions
//	| ε
func type_suffix(tls *libc.TLS, rest uintptr, tok uintptr, ty uintptr) uintptr { /* parse.c:658:13: */
	if equal(tls, tok, ts+8666) != 0 {
		return func_params(tls, rest, (*Token)(unsafe.Pointer(tok)).next, ty)
	}

	if equal(tls, tok, ts+8831) != 0 {
		return array_dimensions(tls, rest, (*Token)(unsafe.Pointer(tok)).next, ty)
	}

	*(*uintptr)(unsafe.Pointer(rest)) = tok
	return ty
}

// pointers = ("*" ("const" | "volatile" | "restrict")*)*
func pointers(tls *libc.TLS, rest uintptr, tok uintptr, ty uintptr) uintptr { /* parse.c:670:13: */
	bp := tls.Alloc(8)
	defer tls.Free(8)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	for consume(tls, bp, *(*uintptr)(unsafe.Pointer(bp)), ts+8833) != 0 {
		ty = pointer_to(tls, ty)
		for equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8586) != 0 || equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8592) != 0 || equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8615) != 0 || equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8624) != 0 || equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8635) != 0 {
			*(*uintptr)(unsafe.Pointer(bp /* tok */)) = (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next
		}
	}
	*(*uintptr)(unsafe.Pointer(rest)) = *(*uintptr)(unsafe.Pointer(bp /* tok */))
	return ty
}

// declarator = pointers ("(" ident ")" | "(" declarator ")" | ident) type-suffix
func declarator(tls *libc.TLS, rest uintptr, tok uintptr, ty uintptr) uintptr { /* parse.c:682:13: */
	bp := tls.Alloc(128)
	defer tls.Free(128)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	ty = pointers(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ty)

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8666) != 0 {
		var start uintptr = *(*uintptr)(unsafe.Pointer(bp /* tok */))
		*(*Type)(unsafe.Pointer(bp + 8 /* dummy */)) = Type{}
		declarator(tls, bp, (*Token)(unsafe.Pointer(start)).next, bp+8)
		*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8668)
		ty = type_suffix(tls, rest, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ty)
		return declarator(tls, bp, (*Token)(unsafe.Pointer(start)).next, ty)
	}

	var name uintptr = uintptr(0)
	var name_pos uintptr = *(*uintptr)(unsafe.Pointer(bp /* tok */))

	if (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp)))).kind == TK_IDENT {
		name = *(*uintptr)(unsafe.Pointer(bp /* tok */))
		*(*uintptr)(unsafe.Pointer(bp /* tok */)) = (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next
	}

	ty = type_suffix(tls, rest, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ty)
	(*Type)(unsafe.Pointer(ty)).name = name
	(*Type)(unsafe.Pointer(ty)).name_pos = name_pos
	return ty
}

// abstract-declarator = pointers ("(" abstract-declarator ")")? type-suffix
func abstract_declarator(tls *libc.TLS, rest uintptr, tok uintptr, ty uintptr) uintptr { /* parse.c:709:13: */
	bp := tls.Alloc(128)
	defer tls.Free(128)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	ty = pointers(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ty)

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8666) != 0 {
		var start uintptr = *(*uintptr)(unsafe.Pointer(bp /* tok */))
		*(*Type)(unsafe.Pointer(bp + 8 /* dummy */)) = Type{}
		abstract_declarator(tls, bp, (*Token)(unsafe.Pointer(start)).next, bp+8)
		*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8668)
		ty = type_suffix(tls, rest, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ty)
		return abstract_declarator(tls, bp, (*Token)(unsafe.Pointer(start)).next, ty)
	}

	return type_suffix(tls, rest, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ty)
}

// type-name = declspec abstract-declarator
func typename(tls *libc.TLS, rest uintptr, tok uintptr) uintptr { /* parse.c:725:13: */
	bp := tls.Alloc(8)
	defer tls.Free(8)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	var ty uintptr = declspec(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)), uintptr(0))
	return abstract_declarator(tls, rest, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ty)
}

func is_end(tls *libc.TLS, tok uintptr) uint8 { /* parse.c:730:13: */
	return uint8(libc.Bool32(equal(tls, tok, ts+8835) != 0 || equal(tls, tok, ts+8331) != 0 && equal(tls, (*Token)(unsafe.Pointer(tok)).next, ts+8835) != 0))
}

func consume_end(tls *libc.TLS, rest uintptr, tok uintptr) uint8 { /* parse.c:734:13: */
	if equal(tls, tok, ts+8835) != 0 {
		*(*uintptr)(unsafe.Pointer(rest)) = (*Token)(unsafe.Pointer(tok)).next
		return uint8(1)
	}

	if equal(tls, tok, ts+8331) != 0 && equal(tls, (*Token)(unsafe.Pointer(tok)).next, ts+8835) != 0 {
		*(*uintptr)(unsafe.Pointer(rest)) = (*Token)(unsafe.Pointer((*Token)(unsafe.Pointer(tok)).next)).next
		return uint8(1)
	}

	return uint8(0)
}

// enum-specifier = ident? "{" enum-list? "}"
//
//	| ident ("{" enum-list? "}")?
//
// enum-list      = ident ("=" num)? ("," ident ("=" num)?)* ","?
func enum_specifier(tls *libc.TLS, rest uintptr, tok uintptr) uintptr { /* parse.c:752:13: */
	bp := tls.Alloc(8)
	defer tls.Free(8)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	var ty uintptr = enum_type(tls)

	// Read a struct tag.
	var tag uintptr = uintptr(0)
	if (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp)))).kind == TK_IDENT {
		tag = *(*uintptr)(unsafe.Pointer(bp /* tok */))
		*(*uintptr)(unsafe.Pointer(bp /* tok */)) = (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next
	}

	if tag != 0 && !(equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8837) != 0) {
		var ty uintptr = find_tag(tls, tag)
		if !(ty != 0) {
			error_tok(tls, tag, ts+8839, 0)
		}
		if (*Type)(unsafe.Pointer(ty)).kind != TY_ENUM {
			error_tok(tls, tag, ts+8857, 0)
		}
		*(*uintptr)(unsafe.Pointer(rest)) = *(*uintptr)(unsafe.Pointer(bp /* tok */))
		return ty
	}

	*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8837)

	// Read an enum-list.
	var i int32 = 0
	var val int32 = 0
	for !(consume_end(tls, rest, *(*uintptr)(unsafe.Pointer(bp))) != 0) {
		if libc.PostIncInt32(&i, 1) > 0 {
			*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8331)
		}

		var name uintptr = get_ident(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		*(*uintptr)(unsafe.Pointer(bp /* tok */)) = (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8873) != 0 {
			val = int32(const_expr(tls, bp, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next))
		}

		var sc uintptr = push_scope(tls, name)
		(*VarScope)(unsafe.Pointer(sc)).enum_ty = ty
		(*VarScope)(unsafe.Pointer(sc)).enum_val = libc.PostIncInt32(&val, 1)
	}

	if tag != 0 {
		push_tag_scope(tls, tag, ty)
	}
	return ty
}

// typeof-specifier = "(" (expr | typename) ")"
func typeof_specifier(tls *libc.TLS, rest uintptr, tok uintptr) uintptr { /* parse.c:798:13: */
	bp := tls.Alloc(8)
	defer tls.Free(8)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8666)
	var ty uintptr
	if is_typename(tls, *(*uintptr)(unsafe.Pointer(bp))) != 0 {
		ty = typename(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
	} else {
		var node uintptr = expr(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
		add_type(tls, node)
		ty = (*Node)(unsafe.Pointer(node)).ty
	}
	*(*uintptr)(unsafe.Pointer(rest)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8668)
	return ty
}

// Generate code for computing a VLA size.
func compute_vla_size(tls *libc.TLS, ty uintptr, tok uintptr) uintptr { /* parse.c:814:13: */
	var node uintptr = new_node(tls, ND_NULL_EXPR, tok)
	if (*Type)(unsafe.Pointer(ty)).base != 0 {
		node = new_binary(tls, ND_COMMA, node, compute_vla_size(tls, (*Type)(unsafe.Pointer(ty)).base, tok), tok)
	}

	if (*Type)(unsafe.Pointer(ty)).kind != TY_VLA {
		return node
	}
	var base_sz uintptr
	if (*Type)(unsafe.Pointer((*Type)(unsafe.Pointer(ty)).base)).kind == TY_VLA {
		base_sz = new_var_node(tls, (*Type)(unsafe.Pointer((*Type)(unsafe.Pointer(ty)).base)).vla_size, tok)
	} else {
		base_sz = new_num(tls, int64((*Type)(unsafe.Pointer((*Type)(unsafe.Pointer(ty)).base)).size), tok)
	}

	(*Type)(unsafe.Pointer(ty)).vla_size = new_lvar(tls, ts+8875, ty_ulong)
	var expr uintptr = new_binary(tls, ND_ASSIGN, new_var_node(tls, (*Type)(unsafe.Pointer(ty)).vla_size, tok),
		new_binary(tls, ND_MUL, (*Type)(unsafe.Pointer(ty)).vla_len, base_sz, tok),
		tok)
	return new_binary(tls, ND_COMMA, node, expr, tok)
}

func new_alloca(tls *libc.TLS, sz uintptr) uintptr { /* parse.c:835:13: */
	var node uintptr = new_unary(tls, ND_FUNCALL, new_var_node(tls, builtin_alloca1, (*Node)(unsafe.Pointer(sz)).tok), (*Node)(unsafe.Pointer(sz)).tok)
	(*Node)(unsafe.Pointer(node)).func_ty = (*Obj)(unsafe.Pointer(builtin_alloca1)).ty
	(*Node)(unsafe.Pointer(node)).ty = (*Type)(unsafe.Pointer((*Obj)(unsafe.Pointer(builtin_alloca1)).ty)).return_ty
	(*Node)(unsafe.Pointer(node)).args = sz
	add_type(tls, sz)
	return node
}

// declaration = declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
func declaration(tls *libc.TLS, rest uintptr, tok uintptr, basety uintptr, attr uintptr) uintptr { /* parse.c:845:13: */
	bp := tls.Alloc(288)
	defer tls.Free(288)
	*(*uintptr)(unsafe.Pointer(bp + 280)) = tok

	*(*Node)(unsafe.Pointer(bp /* head */)) = Node{}
	var cur uintptr = bp /* &head */
	var i int32 = 0

	for !(equal(tls, *(*uintptr)(unsafe.Pointer(bp + 280)), ts+8876) != 0) {
		if libc.PostIncInt32(&i, 1) > 0 {
			*(*uintptr)(unsafe.Pointer(bp + 280 /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp + 280 /* tok */)), ts+8331)
		}

		var ty uintptr = declarator(tls, bp+280, *(*uintptr)(unsafe.Pointer(bp + 280 /* tok */)), basety)
		if (*Type)(unsafe.Pointer(ty)).kind == TY_VOID {
			error_tok(tls, *(*uintptr)(unsafe.Pointer(bp + 280 /* tok */)), ts+8878, 0)
		}
		if !(int32((*Type)(unsafe.Pointer(ty)).name) != 0) {
			error_tok(tls, (*Type)(unsafe.Pointer(ty)).name_pos, ts+8901, 0)
		}

		if attr != 0 && (*VarAttr)(unsafe.Pointer(attr)).is_static != 0 {
			// static local variable
			var var1 uintptr = new_anon_gvar(tls, ty)
			(*VarScope)(unsafe.Pointer(push_scope(tls, get_ident(tls, (*Type)(unsafe.Pointer(ty)).name)))).__var = var1
			if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 280)), ts+8873) != 0 {
				gvar_initializer(tls, bp+280, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 280 /* tok */)))).next, var1)
			}
			continue
		}

		// Generate code for computing a VLA size. We need to do this
		// even if ty is not VLA because ty may be a pointer to VLA
		// (e.g. int (*foo)[n][m] where n and m are variables.)
		cur = libc.AssignPtrUintptr(cur+8, new_unary(tls, ND_EXPR_STMT, compute_vla_size(tls, ty, *(*uintptr)(unsafe.Pointer(bp + 280 /* tok */))), *(*uintptr)(unsafe.Pointer(bp + 280 /* tok */))))

		if (*Type)(unsafe.Pointer(ty)).kind == TY_VLA {
			if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 280)), ts+8873) != 0 {
				error_tok(tls, *(*uintptr)(unsafe.Pointer(bp + 280 /* tok */)), ts+8923, 0)
			}

			// Variable length arrays (VLAs) are translated to alloca() calls.
			// For example, `int x[n+2]` is translated to `tmp = n + 2,
			// x = alloca(tmp)`.
			var var1 uintptr = new_lvar(tls, get_ident(tls, (*Type)(unsafe.Pointer(ty)).name), ty)
			var tok uintptr = (*Type)(unsafe.Pointer(ty)).name
			var expr uintptr = new_binary(tls, ND_ASSIGN, new_vla_ptr(tls, var1, tok),
				new_alloca(tls, new_var_node(tls, (*Type)(unsafe.Pointer(ty)).vla_size, tok)),
				tok)

			cur = libc.AssignPtrUintptr(cur+8, new_unary(tls, ND_EXPR_STMT, expr, tok))
			continue
		}

		var var1 uintptr = new_lvar(tls, get_ident(tls, (*Type)(unsafe.Pointer(ty)).name), ty)
		if attr != 0 && (*VarAttr)(unsafe.Pointer(attr)).align != 0 {
			(*Obj)(unsafe.Pointer(var1)).align = (*VarAttr)(unsafe.Pointer(attr)).align
		}

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 280)), ts+8873) != 0 {
			var expr uintptr = lvar_initializer(tls, bp+280, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 280 /* tok */)))).next, var1)
			cur = libc.AssignPtrUintptr(cur+8, new_unary(tls, ND_EXPR_STMT, expr, *(*uintptr)(unsafe.Pointer(bp + 280 /* tok */))))
		}

		if (*Type)(unsafe.Pointer((*Obj)(unsafe.Pointer(var1)).ty)).size < 0 {
			error_tok(tls, (*Type)(unsafe.Pointer(ty)).name, ts+8968, 0)
		}
		if (*Type)(unsafe.Pointer((*Obj)(unsafe.Pointer(var1)).ty)).kind == TY_VOID {
			error_tok(tls, (*Type)(unsafe.Pointer(ty)).name, ts+8878, 0)
		}
	}

	var node uintptr = new_node(tls, ND_BLOCK, *(*uintptr)(unsafe.Pointer(bp + 280 /* tok */)))
	(*Node)(unsafe.Pointer(node)).body = (*Node)(unsafe.Pointer(bp /* &head */)).next
	*(*uintptr)(unsafe.Pointer(rest)) = (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 280 /* tok */)))).next
	return node
}

func skip_excess_element(tls *libc.TLS, tok uintptr) uintptr { /* parse.c:912:14: */
	bp := tls.Alloc(8)
	defer tls.Free(8)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8837) != 0 {
		*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip_excess_element(tls, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next)
		return skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8835)
	}

	assign(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
	return *(*uintptr)(unsafe.Pointer(bp /* tok */))
}

// string-initializer = string-literal
func string_initializer(tls *libc.TLS, rest uintptr, tok uintptr, init1 uintptr) { /* parse.c:923:13: */
	bp := tls.Alloc(16)
	defer tls.Free(16)

	if (*Initializer)(unsafe.Pointer(init1)).is_flexible != 0 {
		*(*Initializer)(unsafe.Pointer(init1)) = *(*Initializer)(unsafe.Pointer(new_initializer(tls, array_of(tls, (*Type)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).ty)).base, (*Type)(unsafe.Pointer((*Token)(unsafe.Pointer(tok)).ty)).array_len), uint8(0))))
	}

	var len int32 = func() int32 {
		if (*Type)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).ty)).array_len < (*Type)(unsafe.Pointer((*Token)(unsafe.Pointer(tok)).ty)).array_len {
			return (*Type)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).ty)).array_len
		}
		return (*Type)(unsafe.Pointer((*Token)(unsafe.Pointer(tok)).ty)).array_len
	}()

	switch (*Type)(unsafe.Pointer((*Type)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).ty)).base)).size {
	case 1:
		{
			var str uintptr = (*Token)(unsafe.Pointer(tok)).str
			{
				var i int32 = 0
				for ; i < len; i++ {
					(*Initializer)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).children + uintptr(i)*8)))).expr = new_num(tls, int64(*(*int8)(unsafe.Pointer(str + uintptr(i)))), tok)
				}
			}
			break

		}
	case 2:
		{
			var str uintptr = (*Token)(unsafe.Pointer(tok)).str
			{
				var i int32 = 0
				for ; i < len; i++ {
					(*Initializer)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).children + uintptr(i)*8)))).expr = new_num(tls, int64(*(*uint16_t)(unsafe.Pointer(str + uintptr(i)*2))), tok)
				}
			}
			break

		}
	case 4:
		{
			var str uintptr = (*Token)(unsafe.Pointer(tok)).str
			{
				var i int32 = 0
				for ; i < len; i++ {
					(*Initializer)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).children + uintptr(i)*8)))).expr = new_num(tls, int64(*(*uint32_t)(unsafe.Pointer(str + uintptr(i)*4))), tok)
				}
			}
			break

		}
	default:
		error(tls, ts+217, libc.VaList(bp, ts+8804, 949))
	}

	*(*uintptr)(unsafe.Pointer(rest)) = (*Token)(unsafe.Pointer(tok)).next
}

// array-designator = "[" const-expr "]"
//
// C99 added the designated initializer to the language, which allows
// programmers to move the "cursor" of an initializer to any element.
// The syntax looks like this:
//
//	int x[10] = { 1, 2, [5]=3, 4, 5, 6, 7 };
//
// `[5]` moves the cursor to the 5th element, so the 5th element of x
// is set to 3. Initialization then continues forward in order, so
// 6th, 7th, 8th and 9th elements are initialized with 4, 5, 6 and 7,
// respectively. Unspecified elements (in this case, 3rd and 4th
// elements) are initialized with zero.
//
// Nesting is allowed, so the following initializer is valid:
//
//	int x[5][10] = { [5][8]=1, 2, 3 };
//
// It sets x[5][8], x[5][9] and x[6][0] to 1, 2 and 3, respectively.
//
// Use `.fieldname` to move the cursor for a struct initializer. E.g.
//
//	struct { int a, b, c; } x = { .c=5 };
//
// The above initializer sets x.c to 5.
func array_designator(tls *libc.TLS, rest uintptr, tok uintptr, ty uintptr, begin uintptr, end uintptr) { /* parse.c:980:13: */
	bp := tls.Alloc(24)
	defer tls.Free(24)
	*(*uintptr)(unsafe.Pointer(bp + 16)) = tok

	*(*int32)(unsafe.Pointer(begin)) = int32(const_expr(tls, bp+16, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 16 /* tok */)))).next))
	if *(*int32)(unsafe.Pointer(begin)) >= (*Type)(unsafe.Pointer(ty)).array_len {
		error_tok(tls, *(*uintptr)(unsafe.Pointer(bp + 16 /* tok */)), ts+8997, 0)
	}

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp + 16)), ts+8825) != 0 {
		*(*int32)(unsafe.Pointer(end)) = int32(const_expr(tls, bp+16, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp + 16 /* tok */)))).next))
		if *(*int32)(unsafe.Pointer(end)) >= (*Type)(unsafe.Pointer(ty)).array_len {
			error_tok(tls, *(*uintptr)(unsafe.Pointer(bp + 16 /* tok */)), ts+8997, 0)
		}
		if *(*int32)(unsafe.Pointer(end)) < *(*int32)(unsafe.Pointer(begin)) {
			error_tok(tls, *(*uintptr)(unsafe.Pointer(bp + 16 /* tok */)), ts+9041, libc.VaList(bp, *(*int32)(unsafe.Pointer(begin)), *(*int32)(unsafe.Pointer(end))))
		}
	} else {
		*(*int32)(unsafe.Pointer(end)) = *(*int32)(unsafe.Pointer(begin))
	}

	*(*uintptr)(unsafe.Pointer(rest)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp + 16 /* tok */)), ts+8829)
}

// struct-designator = "." ident
func struct_designator(tls *libc.TLS, rest uintptr, tok uintptr, ty uintptr) uintptr { /* parse.c:999:15: */
	var start uintptr = tok
	tok = skip(tls, tok, ts+6708)
	if (*Token)(unsafe.Pointer(tok)).kind != TK_IDENT {
		error_tok(tls, tok, ts+9082, 0)
	}

	{
		var mem uintptr = (*Type)(unsafe.Pointer(ty)).members
		for ; mem != 0; mem = (*Member)(unsafe.Pointer(mem)).next {
			// Anonymous struct member
			if (*Type)(unsafe.Pointer((*Member)(unsafe.Pointer(mem)).ty)).kind == TY_STRUCT && !(int32((*Member)(unsafe.Pointer(mem)).name) != 0) {
				if get_struct_member(tls, (*Member)(unsafe.Pointer(mem)).ty, tok) != 0 {
					*(*uintptr)(unsafe.Pointer(rest)) = start
					return mem
				}
				continue
			}

			// Regular struct member
			if (*Token)(unsafe.Pointer((*Member)(unsafe.Pointer(mem)).name)).len == (*Token)(unsafe.Pointer(tok)).len && !(libc.Xstrncmp(tls, (*Token)(unsafe.Pointer((*Member)(unsafe.Pointer(mem)).name)).loc, (*Token)(unsafe.Pointer(tok)).loc, uint64((*Token)(unsafe.Pointer(tok)).len)) != 0) {
				*(*uintptr)(unsafe.Pointer(rest)) = (*Token)(unsafe.Pointer(tok)).next
				return mem
			}
		}
	}

	error_tok(tls, tok, ts+9110, 0)
	return uintptr(0)
}

// designation = ("[" const-expr "]" | "." ident)* "="? initializer
func designation(tls *libc.TLS, rest uintptr, tok uintptr, init1 uintptr) { /* parse.c:1026:13: */
	bp := tls.Alloc(24)
	defer tls.Free(24)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8831) != 0 {
		if (*Type)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).ty)).kind != TY_ARRAY {
			error_tok(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+9136, 0)
		}
		// var begin int32 at bp+8, 4

		// var end int32 at bp+12, 4

		array_designator(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)), (*Initializer)(unsafe.Pointer(init1)).ty, bp+8, bp+12)
		// var tok2 uintptr at bp+16, 8

		{
			var i int32 = *(*int32)(unsafe.Pointer(bp + 8 /* begin */))
			for ; i <= *(*int32)(unsafe.Pointer(bp + 12)); i++ {
				designation(tls, bp+16, *(*uintptr)(unsafe.Pointer(bp /* tok */)), *(*uintptr)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).children + uintptr(i)*8)))
			}
		}
		array_initializer2(tls, rest, *(*uintptr)(unsafe.Pointer(bp + 16 /* tok2 */)), init1, *(*int32)(unsafe.Pointer(bp + 8))+1)
		return
	}

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+6708) != 0 && (*Type)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).ty)).kind == TY_STRUCT {
		var mem uintptr = struct_designator(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)), (*Initializer)(unsafe.Pointer(init1)).ty)
		designation(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)), *(*uintptr)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).children + uintptr((*Member)(unsafe.Pointer(mem)).idx)*8)))
		(*Initializer)(unsafe.Pointer(init1)).expr = uintptr(0)
		struct_initializer2(tls, rest, *(*uintptr)(unsafe.Pointer(bp /* tok */)), init1, (*Member)(unsafe.Pointer(mem)).next)
		return
	}

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+6708) != 0 && (*Type)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).ty)).kind == TY_UNION {
		var mem uintptr = struct_designator(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)), (*Initializer)(unsafe.Pointer(init1)).ty)
		(*Initializer)(unsafe.Pointer(init1)).mem = mem
		designation(tls, rest, *(*uintptr)(unsafe.Pointer(bp /* tok */)), *(*uintptr)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).children + uintptr((*Member)(unsafe.Pointer(mem)).idx)*8)))
		return
	}

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+6708) != 0 {
		error_tok(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+9173, 0)
	}

	if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8873) != 0 {
		*(*uintptr)(unsafe.Pointer(bp /* tok */)) = (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next
	}
	initializer2(tls, rest, *(*uintptr)(unsafe.Pointer(bp /* tok */)), init1)
}

// An array length can be omitted if an array has an initializer
// (e.g. `int x[] = {1,2,3}`). If it's omitted, count the number
// of initializer elements.
func count_array_init_elements(tls *libc.TLS, tok uintptr, ty uintptr) int32 { /* parse.c:1067:12: */
	bp := tls.Alloc(8)
	defer tls.Free(8)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	var first uint8 = uint8(1)
	var dummy uintptr = new_initializer(tls, (*Type)(unsafe.Pointer(ty)).base, uint8(1))

	var i int32 = 0
	var max int32 = 0

	for !(consume_end(tls, bp, *(*uintptr)(unsafe.Pointer(bp))) != 0) {
		if !(first != 0) {
			*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8331)
		}
		first = uint8(0)

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8831) != 0 {
			i = int32(const_expr(tls, bp, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next))
			if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8825) != 0 {
				i = int32(const_expr(tls, bp, (*Token)(unsafe.Pointer(*(*uintptr)(unsafe.Pointer(bp /* tok */)))).next))
			}
			*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8829)
			designation(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)), dummy)
		} else {
			initializer2(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)), dummy)
		}

		i++
		max = func() int32 {
			if max < i {
				return i
			}
			return max
		}()
	}
	return max
}

// array-initializer1 = "{" initializer ("," initializer)* ","? "}"
func array_initializer1(tls *libc.TLS, rest uintptr, tok uintptr, init1 uintptr) { /* parse.c:1095:13: */
	bp := tls.Alloc(24)
	defer tls.Free(24)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8837)

	if (*Initializer)(unsafe.Pointer(init1)).is_flexible != 0 {
		var len int32 = count_array_init_elements(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), (*Initializer)(unsafe.Pointer(init1)).ty)
		*(*Initializer)(unsafe.Pointer(init1)) = *(*Initializer)(unsafe.Pointer(new_initializer(tls, array_of(tls, (*Type)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).ty)).base, len), uint8(0))))
	}

	var first uint8 = uint8(1)

	if (*Initializer)(unsafe.Pointer(init1)).is_flexible != 0 {
		var len int32 = count_array_init_elements(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), (*Initializer)(unsafe.Pointer(init1)).ty)
		*(*Initializer)(unsafe.Pointer(init1)) = *(*Initializer)(unsafe.Pointer(new_initializer(tls, array_of(tls, (*Type)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).ty)).base, len), uint8(0))))
	}

	{
		var i int32 = 0
		for ; !(consume_end(tls, rest, *(*uintptr)(unsafe.Pointer(bp))) != 0); i++ {
			if !(first != 0) {
				*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8331)
			}
			first = uint8(0)

			if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8831) != 0 {
				// var begin int32 at bp+8, 4

				// var end int32 at bp+12, 4

				array_designator(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)), (*Initializer)(unsafe.Pointer(init1)).ty, bp+8, bp+12)
				// var tok2 uintptr at bp+16, 8

				{
					var j int32 = *(*int32)(unsafe.Pointer(bp + 8 /* begin */))
					for ; j <= *(*int32)(unsafe.Pointer(bp + 12)); j++ {
						designation(tls, bp+16, *(*uintptr)(unsafe.Pointer(bp /* tok */)), *(*uintptr)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).children + uintptr(j)*8)))
					}
				}
				*(*uintptr)(unsafe.Pointer(bp /* tok */)) = *(*uintptr)(unsafe.Pointer(bp + 16 /* tok2 */))
				i = *(*int32)(unsafe.Pointer(bp + 12 /* end */))
				continue
			}

			if i < (*Type)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).ty)).array_len {
				initializer2(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)), *(*uintptr)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).children + uintptr(i)*8)))
			} else {
				*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip_excess_element(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)))
			}
		}
	}
}

// array-initializer2 = initializer ("," initializer)*
func array_initializer2(tls *libc.TLS, rest uintptr, tok uintptr, init1 uintptr, i int32) { /* parse.c:1135:13: */
	bp := tls.Alloc(8)
	defer tls.Free(8)
	*(*uintptr)(unsafe.Pointer(bp)) = tok

	if (*Initializer)(unsafe.Pointer(init1)).is_flexible != 0 {
		var len int32 = count_array_init_elements(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), (*Initializer)(unsafe.Pointer(init1)).ty)
		*(*Initializer)(unsafe.Pointer(init1)) = *(*Initializer)(unsafe.Pointer(new_initializer(tls, array_of(tls, (*Type)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).ty)).base, len), uint8(0))))
	}

	for ; i < (*Type)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).ty)).array_len && !(is_end(tls, *(*uintptr)(unsafe.Pointer(bp))) != 0); i++ {
		var start uintptr = *(*uintptr)(unsafe.Pointer(bp /* tok */))
		if i > 0 {
			*(*uintptr)(unsafe.Pointer(bp /* tok */)) = skip(tls, *(*uintptr)(unsafe.Pointer(bp /* tok */)), ts+8331)
		}

		if equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+8831) != 0 || equal(tls, *(*uintptr)(unsafe.Pointer(bp)), ts+6708) != 0 {
			*(*uintptr)(unsafe.Pointer(rest)) = start
			return
		}

		initializer2(tls, bp, *(*uintptr)(unsafe.Pointer(bp /* tok */)), *(*uintptr)(unsafe.Pointer((*Initializer)(unsafe.Pointer(init1)).children + uintptr(i)*8)))
	}
	*(*uintptr)(unsafe.Pointer(rest)) = *(*uintptr)(unsafe.Pointer(bp /* tok */))
}

// struct-initializer1 = "{" initializer ("," initializer)* ","? "}"
