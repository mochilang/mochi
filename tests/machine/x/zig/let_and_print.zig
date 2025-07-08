const std = @import("std");

var a: i32 = undefined;
var b: i32 = undefined;

pub fn main() void {
    a = 10;
    b = 20;
    std.debug.print("{any}\n", .{(a + b)});
}
