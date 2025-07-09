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

const items = (blk0: { const _tmp0 = struct { cat: []const u8, val: i32, }; const _arr = &[_]_tmp0{_tmp0{ .cat = "a", .val = 3 }, _tmp0{ .cat = "a", .val = 1 }, _tmp0{ .cat = "b", .val = 5 }, _tmp0{ .cat = "b", .val = 2 }}; break :blk0 _arr; });
const grouped = blk2: { var _tmp3 = std.ArrayList(struct { key: []const u8, Items: std.ArrayList(struct { cat: []const u8 val: i32 }) }).init(std.heap.page_allocator); var _tmp4 = std.AutoHashMap([]const u8, usize).init(std.heap.page_allocator); for (items) |i| { const _tmp5 = i.cat; if (_tmp4.get(_tmp5)) |idx| { _tmp3.items[idx].Items.append(i) catch unreachable; } else { var g = struct { key: []const u8, Items: std.ArrayList(struct { cat: []const u8 val: i32 }) }{ .key = _tmp5, .Items = std.ArrayList(struct { cat: []const u8 val: i32 }).init(std.heap.page_allocator) }; g.Items.append(i) catch unreachable; _tmp3.append(g) catch unreachable; _tmp4.put(_tmp5, _tmp3.items.len - 1) catch unreachable; } } var _tmp6 = std.ArrayList(struct { cat: i32 total: f64 }).init(std.heap.page_allocator);for (_tmp3.items) |g| { _tmp6.append(struct { cat: i32, total: f64, }{ .cat = g.key, .total = _sum_int(blk1: { var _tmp1 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp1.append(x.val) catch unreachable; } const _tmp2 = _tmp1.toOwnedSlice() catch unreachable; break :blk1 _tmp2; }) }) catch unreachable; } break :blk2 _tmp6.toOwnedSlice() catch unreachable; };

pub fn main() void {
    _print_list(std.StringHashMap(i32), grouped);
}
