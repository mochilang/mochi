const std = @import("std");

var x = 3; // i32
var y = 4; // i32
var m: std.StringHashMap(i32) = undefined; // std.StringHashMap(i32)

pub fn main() void {
    std.debug.print("{d} {d}\n", .{m["a"], m["b"]});
}
