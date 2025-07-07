const std = @import("std");

pub fn main() !void {
    const value = try std.fmt.parseInt(i32, "1995", 10);
    std.debug.print("{d}\n", .{value});
}
