const std = @import("std");

pub fn main() void {
    std.debug.print("{d}\n", .{(1 + (2 * 3))});
    std.debug.print("{d}\n", .{(((1 + 2)) * 3)});
    std.debug.print("{d}\n", .{((2 * 3) + 1)});
    std.debug.print("{d}\n", .{(2 * ((3 + 1)))});
}
