const std = @import("std");

fn add(a: i32, b: i32) i32 {
    return a + b;
}

pub fn main() void {
    const add5 = fn (b: i32) i32 { return add(5, b); };
    std.debug.print("{d}\n", .{add5(3)});
}
