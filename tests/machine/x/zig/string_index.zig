const std = @import("std");

var s: []const u8 = undefined;

pub fn main() void {
    s = "mochi";
    std.debug.print("{any}\n", .{s[@as(i32, @intCast(1))]});
}
