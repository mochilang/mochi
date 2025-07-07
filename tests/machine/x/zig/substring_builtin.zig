const std = @import("std");

pub fn main() void {
    std.debug.print("{any}\n", .{substring("mochi", @as(i32, @intCast(1)), @as(i32, @intCast(4)))});
}
