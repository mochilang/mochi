const std = @import("std");

pub fn main() void {
    std.debug.print("{s}\n", .{substring("mochi", 1, 4)});
}
