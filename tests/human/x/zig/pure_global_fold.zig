const std = @import("std");

const k: i32 = 2;

fn inc(x: i32) i32 {
    return x + k;
}

pub fn main() void {
    std.debug.print("{d}\n", .{inc(3)});
}
