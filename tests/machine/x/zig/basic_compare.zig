const std = @import("std");

var a: i32 = undefined;
var b: i32 = undefined;

pub fn main() void {
    a = (@as(i32,@intCast(10)) - @as(i32,@intCast(3)));
    b = (@as(i32,@intCast(2)) + @as(i32,@intCast(2)));
    std.debug.print("{any}\n", .{a});
    std.debug.print("{any}\n", .{(a == @as(i32,@intCast(7)))});
    std.debug.print("{any}\n", .{(b < @as(i32,@intCast(5)))});
}
