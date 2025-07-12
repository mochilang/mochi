const std = @import("std");

fn _contains_list_int(v: []const i32, item: i32) bool {
    for (v) |it| { if (it == item) return true; }
    return false;
}

const xs = &[_]i32{
    1,
    2,
    3,
}; // []const i32

pub fn main() void {
    std.debug.print("{}\n", .{_contains_list_int(xs, 2)});
    std.debug.print("{}\n", .{!(_contains_list_int(xs, 5))});
}
