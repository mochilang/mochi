const std = @import("std");

var matrix: []const []const i32 = undefined;

pub fn main() void {
    matrix = &[_][]const i32{ &[_]i32{ @as(i32, @intCast(1)), @as(i32, @intCast(2)) }, &[_]i32{ @as(i32, @intCast(3)), @as(i32, @intCast(4)) } };
    matrix.items[@as(i32, @intCast(1))][@as(i32, @intCast(0))] = @as(i32, @intCast(5));
    std.debug.print("{any}\n", .{matrix[@as(i32, @intCast(1))][@as(i32, @intCast(0))]});
}
