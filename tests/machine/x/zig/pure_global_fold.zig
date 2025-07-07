const std = @import("std");

var k: i32 = undefined;

fn inc(x: i32) i32 {
    return (x + k);
}

pub fn main() void {
    k = @as(i32, @intCast(2));
    std.debug.print("{any}\n", .{inc(@as(i32, @intCast(3)))});
}
