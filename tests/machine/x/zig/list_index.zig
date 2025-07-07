const std = @import("std");

var xs: []const i32 = undefined;

pub fn main() void {
    xs = &[_]i32{ @as(i32, @intCast(10)), @as(i32, @intCast(20)), @as(i32, @intCast(30)) };
    std.debug.print("{any}\n", .{xs[@as(i32, @intCast(1))]});
}
