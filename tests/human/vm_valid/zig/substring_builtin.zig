const std = @import("std");

pub fn main() void {
    const s = "mochi";
    const sub = s[1..4];
    std.debug.print("{s}\n", .{sub});
}
