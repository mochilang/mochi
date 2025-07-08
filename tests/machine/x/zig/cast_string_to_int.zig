const std = @import("std");

pub fn main() void {
    std.debug.print("{any}\n", .{std.fmt.parseInt(i32, "1995", 10) catch unreachable});
}
