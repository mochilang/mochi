const std = @import("std");

fn triple(x: i32) i32 {
    return (x * 3);
}

pub fn main() void {
    std.debug.print("{d}\n", .{triple((1 + 2))});
}
