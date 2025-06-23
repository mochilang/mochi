package zigcode

func (c *Compiler) writeBuiltins() {
	if c.needsAvgInt {
		c.writeln("fn _avg_int(v: []const i32) f64 {")
		c.indent++
		c.writeln("if (v.len == 0) return 0;")
		c.writeln("var sum: f64 = 0;")
		c.writeln("for (v) |it| { sum += @floatFromInt(it); }")
		c.writeln("return sum / @as(f64, @floatFromInt(v.len));")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsAvgFloat {
		c.writeln("fn _avg_float(v: []const f64) f64 {")
		c.indent++
		c.writeln("if (v.len == 0) return 0;")
		c.writeln("var sum: f64 = 0;")
		c.writeln("for (v) |it| { sum += it; }")
		c.writeln("return sum / @as(f64, @floatFromInt(v.len));")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsInListInt {
		c.writeln("fn _contains_list_int(v: []const i32, item: i32) bool {")
		c.indent++
		c.writeln("for (v) |it| { if (it == item) return true; }")
		c.writeln("return false;")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsInListString {
		c.writeln("fn _contains_list_string(v: []const []const u8, item: []const u8) bool {")
		c.indent++
		c.writeln("for (v) |it| { if (std.mem.eql(u8, it, item)) return true; }")
		c.writeln("return false;")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsSetOps {
		c.writeln("fn _contains(comptime T: type, v: []const T, item: T) bool {")
		c.indent++
		c.writeln("for (v) |it| { if (std.meta.eql(it, item)) return true; }")
		c.writeln("return false;")
		c.indent--
		c.writeln("}")
		c.writeln("")

		c.writeln("fn _union_all(comptime T: type, a: []const T, b: []const T) []T {")
		c.indent++
		c.writeln("var res = std.ArrayList(T).init(std.heap.page_allocator);")
		c.writeln("defer res.deinit();")
		c.writeln("for (a) |it| { res.append(it) catch unreachable; }")
		c.writeln("for (b) |it| { res.append(it) catch unreachable; }")
		c.writeln("return res.toOwnedSlice() catch unreachable;")
		c.indent--
		c.writeln("}")
		c.writeln("")

		c.writeln("fn _union(comptime T: type, a: []const T, b: []const T) []T {")
		c.indent++
		c.writeln("var res = std.ArrayList(T).init(std.heap.page_allocator);")
		c.writeln("defer res.deinit();")
		c.writeln("for (a) |it| { res.append(it) catch unreachable; }")
		c.writeln("for (b) |it| { if (!_contains(T, res.items, it)) res.append(it) catch unreachable; }")
		c.writeln("return res.toOwnedSlice() catch unreachable;")
		c.indent--
		c.writeln("}")
		c.writeln("")

		c.writeln("fn _except(comptime T: type, a: []const T, b: []const T) []T {")
		c.indent++
		c.writeln("var res = std.ArrayList(T).init(std.heap.page_allocator);")
		c.writeln("defer res.deinit();")
		c.writeln("for (a) |it| { if (!_contains(T, b, it)) res.append(it) catch unreachable; }")
		c.writeln("return res.toOwnedSlice() catch unreachable;")
		c.indent--
		c.writeln("}")
		c.writeln("")

		c.writeln("fn _intersect(comptime T: type, a: []const T, b: []const T) []T {")
		c.indent++
		c.writeln("var res = std.ArrayList(T).init(std.heap.page_allocator);")
		c.writeln("defer res.deinit();")
		c.writeln("for (a) |it| { if (_contains(T, b, it) and !_contains(T, res.items, it)) res.append(it) catch unreachable; }")
		c.writeln("return res.toOwnedSlice() catch unreachable;")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsJSON {
		c.writeln("fn _json(v: anytype) void {")
		c.indent++
		c.writeln("var buf = std.ArrayList(u8).init(std.heap.page_allocator);")
		c.writeln("defer buf.deinit();")
		c.writeln("std.json.stringify(v, .{}, buf.writer()) catch unreachable;")
		c.writeln("std.debug.print(\"{s}\\n\", .{buf.items});")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsIndex {
		c.writeln("fn _index_list(comptime T: type, v: []const T, i: i32) T {")
		c.indent++
		c.writeln("var idx = i;")
		c.writeln("const n: i32 = @as(i32, @intCast(v.len));")
		c.writeln("if (idx < 0) idx += n;")
		c.writeln("if (idx < 0 or idx >= n) @panic(\"index out of range\");")
		c.writeln("return v[@as(usize, @intCast(idx))];")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsIndexString {
		c.writeln("fn _index_string(s: []const u8, i: i32) []const u8 {")
		c.indent++
		c.writeln("var idx = i;")
		c.writeln("const n: i32 = @as(i32, @intCast(s.len));")
		c.writeln("if (idx < 0) idx += n;")
		c.writeln("if (idx < 0 or idx >= n) @panic(\"index out of range\");")
		c.writeln("const u = @as(usize, @intCast(idx));")
		c.writeln("return s[u..u+1];")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsSlice {
		c.writeln("fn _slice_list(comptime T: type, v: []const T, start: i32, end: i32) []const T {")
		c.indent++
		c.writeln("var s = start;")
		c.writeln("var e = end;")
		c.writeln("const n: i32 = @as(i32, @intCast(v.len));")
		c.writeln("if (s < 0) s += n;")
		c.writeln("if (e < 0) e += n;")
		c.writeln("if (s < 0) s = 0;")
		c.writeln("if (e > n) e = n;")
		c.writeln("if (e < s) e = s;")
		c.writeln("return v[@as(usize, @intCast(s))..@as(usize, @intCast(e))];")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsSliceString {
		c.writeln("fn _slice_string(s: []const u8, start: i32, end: i32) []const u8 {")
		c.indent++
		c.writeln("var sidx = start;")
		c.writeln("var eidx = end;")
		c.writeln("const n: i32 = @as(i32, @intCast(s.len));")
		c.writeln("if (sidx < 0) sidx += n;")
		c.writeln("if (eidx < 0) eidx += n;")
		c.writeln("if (sidx < 0) sidx = 0;")
		c.writeln("if (eidx > n) eidx = n;")
		c.writeln("if (eidx < sidx) eidx = sidx;")
		c.writeln("return s[@as(usize, @intCast(sidx))..@as(usize, @intCast(eidx))];")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsReduce {
		c.writeln("fn _reduce(comptime T: type, v: []const T, init: T, f: fn (T, T) T) T {")
		c.indent++
		c.writeln("var acc: T = init;")
		c.writeln("for (v) |it| {")
		c.indent++
		c.writeln("acc = f(acc, it);")
		c.indent--
		c.writeln("}")
		c.writeln("return acc;")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsEqual {
		c.writeln("fn _equal(a: anytype, b: anytype) bool {")
		c.indent++
		c.writeln("if (@TypeOf(a) != @TypeOf(b)) return false;")
		c.writeln("return switch (@typeInfo(@TypeOf(a))) {")
		c.indent++
		c.writeln(".Struct, .Union, .Array, .Vector, .Pointer, .Slice => std.meta.eql(a, b),")
		c.writeln("else => a == b,")
		c.indent--
		c.writeln("};")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
}
