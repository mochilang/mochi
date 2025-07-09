const std = @import("std");

const s = "catch";

pub fn main() void {
    std.debug.print("{}\n", .{s.contains("cat")});
    std.debug.print("{}\n", .{s.contains("dog")});
}
