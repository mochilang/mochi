const std = @import("std");

fn _append(comptime T: type, v: []const T, x: T) []T {
    var res = std.ArrayList(T).init(std.heap.page_allocator);
    defer res.deinit();
    for (v) |it| { res.append(it) catch unreachable; }
    res.append(x) catch unreachable;
    return res.toOwnedSlice() catch unreachable;
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

const data = (blk0: { const _tmp0 = struct { tag: []const u8, val: i32, }; const _arr = &[_]_tmp0{_tmp0{ .tag = "a", .val = 1 }, _tmp0{ .tag = "a", .val = 2 }, _tmp0{ .tag = "b", .val = 3 }}; break :blk0 _arr; });
const groups = blk1: { var _tmp1 = std.ArrayList(struct { key: []const u8, Items: std.ArrayList(struct { tag: []const u8, val: i32, }) }).init(std.heap.page_allocator); var _tmp2 = std.AutoHashMap([]const u8, usize).init(std.heap.page_allocator); for (data) |d| { const _tmp3 = d.tag; if (_tmp2.get(_tmp3)) |idx| { _tmp1.items[idx].Items.append(d) catch unreachable; } else { var g = struct { key: []const u8, Items: std.ArrayList(struct { tag: []const u8, val: i32, }) }{ .key = _tmp3, .Items = std.ArrayList(struct { tag: []const u8, val: i32, }).init(std.heap.page_allocator) }; g.Items.append(d) catch unreachable; _tmp1.append(g) catch unreachable; _tmp2.put(_tmp3, _tmp1.items.len - 1) catch unreachable; } } var _tmp4 = std.ArrayList(struct { key: []const u8, Items: std.ArrayList(struct { tag: []const u8, val: i32, }) }).init(std.heap.page_allocator);for (_tmp1.items) |g| { _tmp4.append(g) catch unreachable; } var _tmp5 = std.ArrayList(i32).init(std.heap.page_allocator);for (_tmp4.items) |g| { _tmp5.append(g) catch unreachable; } const _tmp5Slice = _tmp5.toOwnedSlice() catch unreachable; break :blk1 _tmp5Slice; };
var tmp = &[]i32{};
const result = blk2: { var _tmp6 = std.ArrayList(struct { item: i32, key: i32 }).init(std.heap.page_allocator); for (tmp) |r| { _tmp6.append(.{ .item = r, .key = r.tag }) catch unreachable; } for (0.._tmp6.items.len) |i| { for (i+1.._tmp6.items.len) |j| { if (_tmp6.items[j].key < _tmp6.items[i].key) { const t = _tmp6.items[i]; _tmp6.items[i] = _tmp6.items[j]; _tmp6.items[j] = t; } } } var _tmp7 = std.ArrayList(i32).init(std.heap.page_allocator);for (_tmp6.items) |p| { _tmp7.append(p.item) catch unreachable; } const _tmp8 = _tmp7.toOwnedSlice() catch unreachable; break :blk2 _tmp8; };

pub fn main() void {
    for (groups) |g| {
        var total = 0;
        for (g.items) |x| {
            total = (total + x.val);
        }
        tmp = _append(i32, tmp, struct { tag: i32, total: i32, }{ .tag = g.key, .total = total });
    }
    _print_list(i32, result);
}
