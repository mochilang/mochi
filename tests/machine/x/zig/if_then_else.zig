const std = @import("std");

var x: i32 = undefined;
var msg: []const u8 = undefined;

pub fn main() void {
    x = 12;
    msg = if ((x > 10)) ("yes") else ("no");
    std.debug.print("{s}\n", .{msg});
}
