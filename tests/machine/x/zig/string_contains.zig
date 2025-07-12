const std = @import("std");

const s = "catch"; // []const u8

pub fn main() void {
    std.debug.print("{}\n", .{s.contains("cat")});
    std.debug.print("{}\n", .{s.contains("dog")});
}
