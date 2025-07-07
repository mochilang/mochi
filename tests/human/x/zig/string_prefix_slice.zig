const std = @import("std");

pub fn main() void {
    const prefix = "fore";
    const s1 = "forest";
    const s2 = "desert";

    std.debug.print("{}\n", .{std.mem.eql(u8, s1[0..prefix.len], prefix)});
    std.debug.print("{}\n", .{std.mem.eql(u8, s2[0..prefix.len], prefix)});
}
