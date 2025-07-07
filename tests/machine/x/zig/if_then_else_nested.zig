const std = @import("std");

var x: i32 = undefined;
var msg: []const u8 = undefined;

pub fn main() void {
    x = @as(i32, @intCast(8));
    msg = if ((x > @as(i32, @intCast(10)))) ("big") else (if ((x > @as(i32, @intCast(5)))) ("medium") else ("small"));
    std.debug.print("{s}\n", .{msg});
}
