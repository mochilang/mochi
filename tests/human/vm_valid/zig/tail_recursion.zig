const std = @import("std");

fn sum_rec(n: i32, acc: i32) i32 {
    if (n == 0) return acc;
    return sum_rec(n - 1, acc + n);
}

pub fn main() void {
    std.debug.print("{d}\n", .{sum_rec(10, 0)});
}
