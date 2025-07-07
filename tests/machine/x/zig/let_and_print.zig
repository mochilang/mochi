const std = @import("std");

var a: i32 = undefined;
var b: i32 = undefined;

pub fn main() void {
    a = @as(i32, @intCast(10));
    b = @as(i32, @intCast(20));
    std.debug.print("{any}\n", .{(a + b)});
}
