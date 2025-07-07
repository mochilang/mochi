const std = @import("std");

fn _contains_list_int(v: []const i32, item: i32) bool {
    for (v) |it| {
        if (it == item) return true;
    }
    return false;
}

var nums: []const i32 = undefined;

pub fn main() void {
    nums = &[_]i32{ @as(i32, @intCast(1)), @as(i32, @intCast(2)), @as(i32, @intCast(3)) };
    std.debug.print("{any}\n", .{_contains_list_int(nums, @as(i32, @intCast(2)))});
    std.debug.print("{any}\n", .{_contains_list_int(nums, @as(i32, @intCast(4)))});
}
