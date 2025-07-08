const std = @import("std");

fn _contains_list_int(v: []const i32, item: i32) bool {
    for (v) |it| { if (it == item) return true; }
    return false;
}

var xs: []const i32 = undefined;

pub fn main() void {
    xs = &[_]i32{1, 2, 3};
    std.debug.print("{any}\n", .{_contains_list_int(xs, 2)});
    std.debug.print("{any}\n", .{!(_contains_list_int(xs, 5))});
}
