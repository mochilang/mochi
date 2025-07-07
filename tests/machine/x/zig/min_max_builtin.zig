const std = @import("std");

fn _min_int(v: []const i32) i32 {
    if (v.len == 0) return 0;
    var m: i32 = v[0];
    for (v[1..]) |it| {
        if (it < m) m = it;
    }
    return m;
}

var nums: []const i32 = undefined;

pub fn main() void {
    nums = &[_]i32{ @as(i32, @intCast(3)), @as(i32, @intCast(1)), @as(i32, @intCast(4)) };
    std.debug.print("{any}\n", .{_min_int(nums)});
    std.debug.print("{any}\n", .{max(nums)});
}
