const std = @import("std");

var a: []const i32 = undefined;

pub fn main() void {
    a = &[_]i32{@as(i32,@intCast(1)), @as(i32,@intCast(2))};
    std.debug.print("{any}\n", .{append(a, @as(i32,@intCast(3)))});
}
