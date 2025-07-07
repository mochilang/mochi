const std = @import("std");

fn sum_rec(n: i32, acc: i32) i32 {
    if ((n == @as(i32, @intCast(0)))) {
        return acc;
    }
    return sum_rec((n - @as(i32, @intCast(1))), (acc + n));
}

pub fn main() void {
    std.debug.print("{any}\n", .{sum_rec(@as(i32, @intCast(10)), @as(i32, @intCast(0)))});
}
