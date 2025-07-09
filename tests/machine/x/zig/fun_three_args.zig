const std = @import("std");

fn sum3(a: i32, b: i32, c: i32) i32 {
    return ((a + b) + c);
}

pub fn main() void {
    std.debug.print("{d}\n", .{sum3(1, 2, 3)});
}
