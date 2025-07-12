const std = @import("std");

const M = struct {
    a: i32,
    b: i32,
};
const m = M{
    .a = 1,
    .b = 2,
}; // M

pub fn main() void {
    std.debug.print("{any}\n", .{m["b"]});
}
