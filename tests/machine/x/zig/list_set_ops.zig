const std = @import("std");

fn _contains(comptime T: type, v: []const T, item: T) bool {
    for (v) |it| { if (std.meta.eql(it, item)) return true; }
    return false;
}

fn _union_all(comptime T: type, a: []const T, b: []const T) []T {
    var res = std.ArrayList(T).init(std.heap.page_allocator);
    defer res.deinit();
    for (a) |it| { res.append(it) catch unreachable; }
    for (b) |it| { res.append(it) catch unreachable; }
    return res.toOwnedSlice() catch unreachable;
}

fn _union(comptime T: type, a: []const T, b: []const T) []T {
    var res = std.ArrayList(T).init(std.heap.page_allocator);
    defer res.deinit();
    for (a) |it| { res.append(it) catch unreachable; }
    for (b) |it| { if (!_contains(T, res.items, it)) res.append(it) catch unreachable; }
    return res.toOwnedSlice() catch unreachable;
}

fn _except(comptime T: type, a: []const T, b: []const T) []T {
    var res = std.ArrayList(T).init(std.heap.page_allocator);
    defer res.deinit();
    for (a) |it| { if (!_contains(T, b, it)) res.append(it) catch unreachable; }
    return res.toOwnedSlice() catch unreachable;
}

fn _intersect(comptime T: type, a: []const T, b: []const T) []T {
    var res = std.ArrayList(T).init(std.heap.page_allocator);
    defer res.deinit();
    for (a) |it| { if (_contains(T, b, it) and !_contains(T, res.items, it)) res.append(it) catch unreachable; }
    return res.toOwnedSlice() catch unreachable;
}

fn _print_list(comptime T: type, v: []const T) void {
    for (v, 0..) |it, i| {
        if (i > 0) std.debug.print(" ", .{});
        std.debug.print("{any}", .{it});
    }
    std.debug.print("\n", .{});
}

pub fn main() void {
    _print_list(i32, _union(i32, &[_]i32{
    1,
    2,
}, &[_]i32{
    2,
    3,
}));
    _print_list(i32, _except(i32, &[_]i32{
    1,
    2,
    3,
}, &[_]i32{2}));
    _print_list(i32, _intersect(i32, &[_]i32{
    1,
    2,
    3,
}, &[_]i32{
    2,
    4,
}));
    std.debug.print("{d}\n", .{(_union_all(i32, &[_]i32{
    1,
    2,
}, &[_]i32{
    2,
    3,
})).len});
}
