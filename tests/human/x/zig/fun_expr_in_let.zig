const std = @import("std");

pub fn main() void {
    const square = fn (x: i32) i32 { return x * x; };
    std.debug.print("{d}\n", .{square(6)});
}
