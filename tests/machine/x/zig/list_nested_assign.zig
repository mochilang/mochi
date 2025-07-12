const std = @import("std");

var matrix = &[_][]const i32{
    &[_]i32{
    1,
    2,
},
    &[_]i32{
    3,
    4,
},
}; // []const []const i32

pub fn main() void {
    matrix.items[1][0] = 5;
    std.debug.print("{d}\n", .{matrix[1][0]});
}
