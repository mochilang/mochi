const std = @import("std");

var s: []const u8 = undefined;

pub fn main() void {
    s = "catch";
    std.debug.print("{any}\n", .{s.contains("cat")});
    std.debug.print("{any}\n", .{s.contains("dog")});
}
