const std = @import("std");

pub fn main() void {
    std.debug.print("{d}\n", .{7});
    std.debug.print("{d}\n", .{((3) * 3)});
    std.debug.print("{d}\n", .{7});
    std.debug.print("{d}\n", .{(2 * (4))});
}
