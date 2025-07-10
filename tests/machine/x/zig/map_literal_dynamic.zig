const std = @import("std");

var x = 3;
var y = 4;
var m: std.StringHashMap(i32) = undefined;

pub fn main() void {
    std.debug.print("{d} {d}\n", .{m["a"], m["b"]});
}
