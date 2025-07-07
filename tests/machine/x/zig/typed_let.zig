const std = @import("std");

var y: i32 = undefined;

pub fn main() void {
    y = 0;
    std.debug.print("{any}\n", .{y});
}
