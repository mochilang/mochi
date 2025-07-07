const std = @import("std");

fn sum3(a: i32, b: i32, c: i32) i32 {
    return ((a + b) + c);
}

pub fn main() void {
    std.debug.print("{any}\n", .{sum3(@as(i32, @intCast(1)), @as(i32, @intCast(2)), @as(i32, @intCast(3)))});
}
