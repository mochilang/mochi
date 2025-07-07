const std = @import("std");

pub fn main() void {
    const s = "catch";
    std.debug.print("{}\n", .{std.mem.indexOf(u8, s, "cat") != null});
    std.debug.print("{}\n", .{std.mem.indexOf(u8, s, "dog") != null});
}
