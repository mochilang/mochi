const std = @import("std");

var x = 3;
var y = 4;
var m: struct { a: i32 b: i32 } = undefined;

pub fn main() void {
    std.debug.print("{any} {any}\n", .{m["a"], m["b"]});
}
