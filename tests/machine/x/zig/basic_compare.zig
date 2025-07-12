const std = @import("std");

const a = 7; // i32
const b = 4; // i32

pub fn main() void {
    std.debug.print("{d}\n", .{a});
    std.debug.print("{}\n", .{(a == 7)});
    std.debug.print("{}\n", .{(b < 5)});
}
