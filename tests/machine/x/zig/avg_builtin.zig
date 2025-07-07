const std = @import("std");

fn _avg_int(v: []const i32) i32 {
    if (v.len == 0) return 0;
    var sum: i32 = 0;
    for (v) |it| { sum += it; }
    return @divTrunc(sum, @as(i32, @intCast(v.len)));
}

pub fn main() void {
    std.debug.print("{any}\n", .{_avg_int(&[_]i32{@as(i32,@intCast(1)), @as(i32,@intCast(2)), @as(i32,@intCast(3))})});
}
