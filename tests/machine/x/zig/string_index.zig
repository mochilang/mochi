const std = @import("std");

const s = "mochi"; // []const u8

pub fn main() void {
    std.debug.print("{s}\n", .{s[1]});
}
