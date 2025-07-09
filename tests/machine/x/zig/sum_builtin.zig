const std = @import("std");

fn _sum_int(v: []const i32) i32 {
    var sum: i32 = 0;
    for (v) |it| { sum += it; }
    return sum;
}

pub fn main() void {
    std.debug.print("{any}\n", .{_sum_int(&[_]i32{1, 2, 3})});
}
