const std = @import("std");

pub fn main() !void {
    const xs = [_]i32{1, 2, 3};
    const has2 = std.mem.indexOfScalar(i32, &xs, 2) != null;
    const has5 = std.mem.indexOfScalar(i32, &xs, 5) != null;
    std.debug.print("{}\n", .{has2});
    std.debug.print("{}\n", .{!has5});
}
