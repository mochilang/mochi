// Generated by Mochi compiler v0.10.27 on 2025-07-17T17:59:22Z
const std = @import("std");

fn _sum_int(v: []const i32) i32 {
    var sum: i32 = 0;
    for (v) |it| { sum += it; }
    return sum;
}

pub fn main() void {
    std.debug.print("{d:.1}\n", .{_sum_int(&[_]i32{
    1,
    2,
    3,
})});
}
