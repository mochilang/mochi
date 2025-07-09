const std = @import("std");

const s = "mochi";

pub fn main() void {
    std.debug.print("{s}\n", .{s[1]});
}
