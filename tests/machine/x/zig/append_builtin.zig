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

var a: []const i32 = undefined;

pub fn main() void {
    a = &[_]i32{1, 2};
    _print_list(i32, _append(i32, a, 3));
}
