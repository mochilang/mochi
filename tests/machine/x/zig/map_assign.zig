const std = @import("std");

var scores: std.StringHashMap(i32) = undefined;

pub fn main() void {
    _ = scores.put("bob", 2) catch unreachable;
    std.debug.print("{d}\n", .{scores["bob"]});
}
