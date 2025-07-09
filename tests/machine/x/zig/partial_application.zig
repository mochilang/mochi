const std = @import("std");

const add5 = add(5);

fn add(a: i32, b: i32) i32 {
    return (a + b);
}

pub fn main() void {
    std.debug.print("{d}\n", .{add5(3)});
}
