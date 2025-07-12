const std = @import("std");

fn _min_int(v: []const i32) i32 {
    if (v.len == 0) return 0;
    var m: i32 = v[0];
    for (v[1..]) |it| { if (it < m) m = it; }
    return m;
}

fn _max_int(v: []const i32) i32 {
    if (v.len == 0) return 0;
    var m: i32 = v[0];
    for (v[1..]) |it| { if (it > m) m = it; }
    return m;
}

const nums = &[_]i32{
    3,
    1,
    4,
}; // []const i32

pub fn main() void {
    std.debug.print("{any}\n", .{_min_int(nums)});
    std.debug.print("{any}\n", .{_max_int(nums)});
}
