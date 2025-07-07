const std = @import("std");

var x: i32 = undefined;

pub fn main() void {
    x = @as(i32, @intCast(1));
    x = @as(i32, @intCast(2));
    std.debug.print("{any}\n", .{x});
}
