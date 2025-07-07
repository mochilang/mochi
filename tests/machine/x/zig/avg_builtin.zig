const std = @import("std");

fn _avg_int(v: []const i32) f64 {
    if (v.len == 0) return 0;
    var sum: f64 = 0;
    for (v) |it| {
        sum += @floatFromInt(it);
    }
    return sum / @as(f64, @floatFromInt(v.len));
}

pub fn main() void {
    std.debug.print("{any}\n", .{_avg_int(&[_]i32{ @as(i32, @intCast(1)), @as(i32, @intCast(2)), @as(i32, @intCast(3)) })});
}
