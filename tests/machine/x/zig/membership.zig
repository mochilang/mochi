const std = @import("std");

fn _contains_list_int(v: []const i32, item: i32) bool {
    for (v) |it| { if (it == item) return true; }
    return false;
}

const nums = &[_]i32{
    1,
    2,
    3,
};

pub fn main() void {
    std.debug.print("{}\n", .{_contains_list_int(nums, 2)});
    std.debug.print("{}\n", .{_contains_list_int(nums, 4)});
}
