// Generated by Mochi compiler v0.10.27 on 2025-07-17T17:59:22Z
const std = @import("std");

fn _print_list(comptime T: type, v: []const T) void {
    for (v, 0..) |it, i| {
        if (i > 0) std.debug.print(" ", .{});
        std.debug.print("{any}", .{it});
    }
    std.debug.print("\n", .{});
}

pub fn main() void {
    _print_list(i32, &[_]i32{
    1,
    2,
    3,
}[1..3]);
    _print_list(i32, &[_]i32{
    1,
    2,
    3,
}[0..2]);
    std.debug.print("{s}\n", .{"hello"[1..4]});
}
