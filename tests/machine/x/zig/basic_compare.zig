const std = @import("std");

const a = (10 - 3); // i32
const b = (2 + 2); // i32

pub fn main() void {
    std.debug.print("{d}\n", .{a});
    std.debug.print("{}\n", .{(a == 7)});
    std.debug.print("{}\n", .{(b < 5)});
}
