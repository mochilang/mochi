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

var items: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: []const std.AutoHashMap([]const u8, i32) = undefined;

pub fn main() void {
    items = &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("cat", "a") catch unreachable; m.put("val", 10) catch unreachable; m.put("flag", true) catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("cat", "a") catch unreachable; m.put("val", 5) catch unreachable; m.put("flag", false) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("cat", "b") catch unreachable; m.put("val", 20) catch unreachable; m.put("flag", true) catch unreachable; break :blk2 m; }};
    result = blk6: { var _tmp4 = std.ArrayList(struct { key: i32, Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp5 = std.AutoHashMap(i32, usize).init(std.heap.page_allocator); for (items) |i| { const _tmp6 = i.cat; if (_tmp5.get(_tmp6)) |idx| { _tmp4.items[idx].Items.append(i) catch unreachable; } else { var g = struct { key: i32, Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp6, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(i) catch unreachable; _tmp4.append(g) catch unreachable; _tmp5.put(_tmp6, _tmp4.items.len - 1) catch unreachable; } } var _tmp7 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp4.items) |g| { _tmp7.append(blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cat", g.key) catch unreachable; m.put("share", (_sum_int(blk4: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp0.append(if (x.flag) (x.val) else (0)) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk4 _tmp1; }) / _sum_int(blk5: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp2.append(x.val) catch unreachable; } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk5 _tmp3; }))) catch unreachable; break :blk3 m; }) catch unreachable; } break :blk6 _tmp7.toOwnedSlice() catch unreachable; };
    _print_list(std.AutoHashMap([]const u8, i32), result);
}
