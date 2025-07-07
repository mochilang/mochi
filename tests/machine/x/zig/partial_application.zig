const std = @import("std");

var add5: fn (i32) i32 = undefined;

fn add(a: i32, b: i32) i32 {
    return (a + b);
}

pub fn main() void {
    add5 = add(@as(i32, @intCast(5)));
    std.debug.print("{any}\n", .{add5(@as(i32, @intCast(3)))});
}
