//go:build slow

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
	if c.needsSumInt {
		c.writeln("fn _sum_int(v: []const i32) i32 {")
		c.indent++
		c.writeln("var sum: i32 = 0;")
		c.writeln("for (v) |it| { sum += it; }")
		c.writeln("return sum;")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsSumFloat {
		c.writeln("fn _sum_float(v: []const f64) f64 {")
		c.indent++
		c.writeln("var sum: f64 = 0;")
		c.writeln("for (v) |it| { sum += it; }")
		c.writeln("return sum;")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsMinInt {
		c.writeln("fn _min_int(v: []const i32) i32 {")
		c.indent++
		c.writeln("if (v.len == 0) return 0;")
		c.writeln("var m: i32 = v[0];")
		c.writeln("for (v[1..]) |it| { if (it < m) m = it; }")
		c.writeln("return m;")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsMinFloat {
		c.writeln("fn _min_float(v: []const f64) f64 {")
		c.indent++
		c.writeln("if (v.len == 0) return 0.0;")
		c.writeln("var m: f64 = v[0];")
		c.writeln("for (v[1..]) |it| { if (it < m) m = it; }")
		c.writeln("return m;")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsMinString {
		c.writeln("fn _min_string(v: []const []const u8) []const u8 {")
		c.indent++
		c.writeln("if (v.len == 0) return \"\";")
		c.writeln("var m: []const u8 = v[0];")
		c.writeln("for (v[1..]) |it| { if (std.mem.lessThan(u8, it, m)) m = it; }")
		c.writeln("return m;")
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
	if c.needsLoadJSON || c.needsSaveJSON {
		c.writeln("fn _read_input(path: ?[]const u8) []const u8 {")
		c.indent++
		c.writeln("const alloc = std.heap.page_allocator;")
		c.writeln("if (path == null or std.mem.eql(u8, path.?, \"-\")) {")
		c.indent++
		c.writeln("return std.io.getStdIn().readAllAlloc(alloc, 1 << 20) catch unreachable;")
		c.indent--
		c.writeln("} else {")
		c.indent++
		c.writeln("return std.fs.cwd().readFileAlloc(alloc, path.?, 1 << 20) catch unreachable;")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")
		c.writeln("")
		c.writeln("fn _write_output(path: ?[]const u8, data: []const u8) void {")
		c.indent++
		c.writeln("if (path == null or std.mem.eql(u8, path.?, \"-\")) {")
		c.indent++
		c.writeln("std.io.getStdOut().writeAll(data) catch unreachable;")
		c.indent--
		c.writeln("} else {")
		c.indent++
		c.writeln("std.fs.cwd().writeFile(path.?, data) catch unreachable;")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsLoadJSON {
		c.writeln("fn _load_json(comptime T: type, path: ?[]const u8) []T {")
		c.indent++
		c.writeln("const text = _read_input(path);")
		c.writeln("return std.json.parseFromSlice(T, std.heap.page_allocator, text, .{}).value;")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsSaveJSON {
		c.writeln("fn _save_json(rows: anytype, path: ?[]const u8) void {")
		c.indent++
		c.writeln("var buf = std.ArrayList(u8).init(std.heap.page_allocator);")
		c.writeln("defer buf.deinit();")
		c.writeln("if (rows.len == 1) {")
		c.indent++
		c.writeln("std.json.stringify(rows[0], .{}, buf.writer()) catch unreachable;")
		c.indent--
		c.writeln("} else {")
		c.indent++
		c.writeln("std.json.stringify(rows, .{}, buf.writer()) catch unreachable;")
		c.indent--
		c.writeln("}")
		c.writeln("_write_output(path, buf.items);")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsFetch {
		c.writeln("fn _fetch(url: []const u8, opts: anytype) []const u8 {")
		c.indent++
		c.writeln("_ = opts;")
		c.writeln("const alloc = std.heap.page_allocator;")
		c.writeln("if (std.mem.startsWith(u8, url, \"file://\")) {")
		c.indent++
		c.writeln("return std.fs.cwd().readFileAlloc(alloc, url[7..], 1 << 20) catch unreachable;")
		c.indent--
		c.writeln("}")
		c.writeln("var child = std.ChildProcess.init(&.{\"curl\", \"-s\", url}, alloc);")
		c.writeln("child.stdout_behavior = .Pipe;")
		c.writeln("child.spawn() catch unreachable;")
		c.writeln("defer { if (child.stdout) |s| { s.close(); } child.wait() catch unreachable; }")
		c.writeln("return child.stdout.?.readToEndAlloc(alloc, 1 << 20) catch unreachable;")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsFetchJSON {
		c.writeln("fn _fetch_json(comptime T: type, url: []const u8, opts: anytype) T {")
		c.indent++
		c.writeln("const data = _fetch(url, opts);")
		c.writeln("return std.json.parseFromSlice(T, std.heap.page_allocator, data, .{}).value;")
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
		c.writeln("fn _slice_list(comptime T: type, v: []const T, start: i32, end: i32, step: i32) []T {")
		c.indent++
		c.writeln("var s = start;")
		c.writeln("var e = end;")
		c.writeln("var st = step;")
		c.writeln("const n: i32 = @as(i32, @intCast(v.len));")
		c.writeln("if (s < 0) s += n;")
		c.writeln("if (e < 0) e += n;")
		c.writeln("if (st == 0) st = 1;")
		c.writeln("if (s < 0) s = 0;")
		c.writeln("if (e > n) e = n;")
		c.writeln("if (st > 0 and e < s) e = s;")
		c.writeln("if (st < 0 and e > s) e = s;")
		c.writeln("var res = std.ArrayList(T).init(std.heap.page_allocator);")
		c.writeln("defer res.deinit();")
		c.writeln("var i: i32 = s;")
		c.writeln("while ((st > 0 and i < e) or (st < 0 and i > e)) : (i += st) {")
		c.indent++
		c.writeln("res.append(v[@as(usize, @intCast(i))]) catch unreachable;")
		c.indent--
		c.writeln("}")
		c.writeln("return res.toOwnedSlice() catch unreachable;")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsSliceString {
		c.writeln("fn _slice_string(s: []const u8, start: i32, end: i32, step: i32) []const u8 {")
		c.indent++
		c.writeln("var sidx = start;")
		c.writeln("var eidx = end;")
		c.writeln("var stp = step;")
		c.writeln("const n: i32 = @as(i32, @intCast(s.len));")
		c.writeln("if (sidx < 0) sidx += n;")
		c.writeln("if (eidx < 0) eidx += n;")
		c.writeln("if (stp == 0) stp = 1;")
		c.writeln("if (sidx < 0) sidx = 0;")
		c.writeln("if (eidx > n) eidx = n;")
		c.writeln("if (stp > 0 and eidx < sidx) eidx = sidx;")
		c.writeln("if (stp < 0 and eidx > sidx) eidx = sidx;")
		c.writeln("var res = std.ArrayList(u8).init(std.heap.page_allocator);")
		c.writeln("defer res.deinit();")
		c.writeln("var i: i32 = sidx;")
		c.writeln("while ((stp > 0 and i < eidx) or (stp < 0 and i > eidx)) : (i += stp) {")
		c.indent++
		c.writeln("res.append(s[@as(usize, @intCast(i))]) catch unreachable;")
		c.indent--
		c.writeln("}")
		c.writeln("return res.toOwnedSlice() catch unreachable;")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsConcatList {
		c.writeln("fn _concat_list(comptime T: type, a: []const T, b: []const T) []T {")
		c.indent++
		c.writeln("var res = std.ArrayList(T).init(std.heap.page_allocator);")
		c.writeln("defer res.deinit();")
		c.writeln("for (a) |it| { res.append(it) catch unreachable; }")
		c.writeln("for (b) |it| { res.append(it) catch unreachable; }")
		c.writeln("return res.toOwnedSlice() catch unreachable;")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsConcatString {
		c.writeln("fn _concat_string(a: []const u8, b: []const u8) []const u8 {")
		c.indent++
		c.writeln("var res = std.ArrayList(u8).init(std.heap.page_allocator);")
		c.writeln("defer res.deinit();")
		c.writeln("res.appendSlice(a) catch unreachable;")
		c.writeln("res.appendSlice(b) catch unreachable;")
		c.writeln("return res.toOwnedSlice() catch unreachable;")
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
	if c.needsMapKeys {
		c.writeln("fn _map_keys(comptime K: type, comptime V: type, m: std.AutoHashMap(K, V)) []K {")
		c.indent++
		c.writeln("var res = std.ArrayList(K).init(std.heap.page_allocator);")
		c.writeln("defer res.deinit();")
		c.writeln("var it = m.keyIterator();")
		c.writeln("while (it.next()) |k_ptr| { res.append(k_ptr.*) catch unreachable; }")
		c.writeln("return res.toOwnedSlice() catch unreachable;")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsMapValues {
		c.writeln("fn _map_values(comptime K: type, comptime V: type, m: std.AutoHashMap(K, V)) []V {")
		c.indent++
		c.writeln("var res = std.ArrayList(V).init(std.heap.page_allocator);")
		c.writeln("defer res.deinit();")
		c.writeln("var it = m.valueIterator();")
		c.writeln("while (it.next()) |v_ptr| { res.append(v_ptr.*) catch unreachable; }")
		c.writeln("return res.toOwnedSlice() catch unreachable;")
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

func (c *Compiler) writeExpectFunc() {
	if !c.needsExpect {
		return
	}
	c.writeln("fn expect(cond: bool) void {")
	c.indent++
	c.writeln("if (!cond) @panic(\"expect failed\");")
	c.indent--
	c.writeln("}")
	c.writeln("")
}
