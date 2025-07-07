const std = @import("std");

pub fn main() void {
    const s = "mochi";
    std.debug.print("{d}\n", .{s.len});
}
