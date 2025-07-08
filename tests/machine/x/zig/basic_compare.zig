const std = @import("std");

var a: i32 = undefined;
var b: i32 = undefined;

pub fn main() void {
    a = (10 - 3);
    b = (2 + 2);
    std.debug.print("{any}\n", .{a});
    std.debug.print("{any}\n", .{(a == 7)});
    std.debug.print("{any}\n", .{(b < 5)});
}
