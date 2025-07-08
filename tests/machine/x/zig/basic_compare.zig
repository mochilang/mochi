const std = @import("std");

const a = (10 - 3);
const b = (2 + 2);

pub fn main() void {
    std.debug.print("{d}\n", .{a});
    std.debug.print("{}\n", .{(a == 7)});
    std.debug.print("{}\n", .{(b < 5)});
}
