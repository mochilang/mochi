const std = @import("std");

var m: struct { a: i32, b: i32, } = undefined;

pub fn main() void {
    for (m) |k| {
        std.debug.print("{any}\n", .{k});
    }
}
