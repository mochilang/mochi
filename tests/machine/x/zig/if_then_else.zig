const std = @import("std");

var x: i32 = undefined;
var msg: []const u8 = undefined;

pub fn main() void {
    x = @as(i32, @intCast(12));
    msg = if ((x > @as(i32, @intCast(10)))) ("yes") else ("no");
    std.debug.print("{s}\n", .{msg});
}
