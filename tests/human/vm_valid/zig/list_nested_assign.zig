const std = @import("std");

pub fn main() void {
    var matrix = [2][2]i32{ .{1,2}, .{3,4} };
    matrix[1][0] = 5;
    std.debug.print("{d}\n", .{matrix[1][0]});
}
