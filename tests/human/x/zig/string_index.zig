const std = @import("std");

pub fn main() void {
    const s = "mochi";
    const ch = s[1];
    std.debug.print("{c}\n", .{ch});
}
