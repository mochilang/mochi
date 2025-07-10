const std = @import("std");

fn _sum_int(v: []const i32) i32 {
    var sum: i32 = 0;
    for (v) |it| { sum += it; }
    return sum;
}

fn _print_list(comptime T: type, v: []const T) void {
    for (v, 0..) |it, i| {
        if (i > 0) std.debug.print(" ", .{});
        std.debug.print("{any}", .{it});
    }
    std.debug.print("\n", .{});
}

fn _equal(a: anytype, b: anytype) bool {
    if (@TypeOf(a) != @TypeOf(b)) return false;
    return switch (@typeInfo(@TypeOf(a))) {
        .Struct, .Union, .Array, .Vector, .Pointer, .Slice => std.meta.eql(a, b),
        else => a == b,
    };
}

const items = (blk0: { const _tmp0 = struct { cat: []const u8, val: i32, flag: bool, }; const _arr = &[_]_tmp0{_tmp0{ .cat = "a", .val = 10, .flag = true }, _tmp0{ .cat = "a", .val = 5, .flag = false }, _tmp0{ .cat = "b", .val = 20, .flag = true }}; break :blk0 _arr; });
const result = blk3: { var _tmp5 = std.ArrayList(struct { key: []const u8, Items: std.ArrayList(struct { cat: []const u8, val: i32, flag: bool, }) }).init(std.heap.page_allocator); var _tmp6 = std.AutoHashMap([]const u8, usize).init(std.heap.page_allocator); for (items) |i| { const _tmp7 = i.cat; if (_tmp6.get(_tmp7)) |idx| { _tmp5.items[idx].Items.append(i) catch unreachable; } else { var g = struct { key: []const u8, Items: std.ArrayList(struct { cat: []const u8, val: i32, flag: bool, }) }{ .key = _tmp7, .Items = std.ArrayList(struct { cat: []const u8, val: i32, flag: bool, }).init(std.heap.page_allocator) }; g.Items.append(i) catch unreachable; _tmp5.append(g) catch unreachable; _tmp6.put(_tmp7, _tmp5.items.len - 1) catch unreachable; } } var _tmp8 = std.ArrayList(struct { key: []const u8, Items: std.ArrayList(struct { cat: []const u8, val: i32, flag: bool, }) }).init(std.heap.page_allocator);for (_tmp5.items) |g| { _tmp8.append(g) catch unreachable; } var _tmp9 = std.ArrayList(struct { item: struct { key: []const u8, Items: std.ArrayList(struct { cat: []const u8, val: i32, flag: bool, }) }, key: i32 }).init(std.heap.page_allocator);for (_tmp8.items) |g| { _tmp9.append(.{ .item = g, .key = g.key }) catch unreachable; } for (0.._tmp9.items.len) |i| { for (i+1.._tmp9.items.len) |j| { if (_tmp9.items[j].key < _tmp9.items[i].key) { const t = _tmp9.items[i]; _tmp9.items[i] = _tmp9.items[j]; _tmp9.items[j] = t; } } } var _tmp10 = std.ArrayList(struct { key: []const u8, Items: std.ArrayList(struct { cat: []const u8, val: i32, flag: bool, }) }).init(std.heap.page_allocator);for (_tmp9.items) |p| { _tmp10.append(p.item) catch unreachable; } var _tmp11 = std.ArrayList(struct { cat: i32, share: f64, }).init(std.heap.page_allocator);for (_tmp10.items) |g| { _tmp11.append(struct { cat: i32, share: f64, }{ .cat = g.key, .share = (_sum_int(blk1: { var _tmp1 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp1.append(if (x.flag) (x.val) else (0)) catch unreachable; } const _tmp2 = _tmp1.toOwnedSlice() catch unreachable; break :blk1 _tmp2; }) / _sum_int(blk2: { var _tmp3 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp3.append(x.val) catch unreachable; } const _tmp4 = _tmp3.toOwnedSlice() catch unreachable; break :blk2 _tmp4; })) }) catch unreachable; } const _tmp11Slice = _tmp11.toOwnedSlice() catch unreachable; break :blk3 _tmp11Slice; };

pub fn main() void {
    _print_list(std.StringHashMap(i32), result);
}
