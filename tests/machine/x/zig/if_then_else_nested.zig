const std = @import("std");

var x: i32 = undefined;
var msg: []const u8 = undefined;

pub fn main() void {
    x = 8;
    msg = if ((x > 10)) ("big") else (if ((x > 5)) ("medium") else ("small"));
    std.debug.print("{s}\n", .{msg});
}
