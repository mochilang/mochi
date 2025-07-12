const std = @import("std");

fn handleError(err: anyerror) noreturn {
    std.debug.panic("{any}", .{err});
}

var x = 3; // i32
var y = 4; // i32
var m: std.StringHashMap(i32) = undefined; // std.StringHashMap(i32)

pub fn main() void {
    std.debug.print("{d} {d}\n", .{m["a"], m["b"]});
}
