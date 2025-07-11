const std = @import("std");

fn _contains_list_int(v: []const i32, item: i32) bool {
    for (v) |it| { if (it == item) return true; }
    return false;
}

const M = struct {
    a: i32,
    b: i32,
};
const m = M{
    .a = 1,
    .b = 2,
}; // M

pub fn main() void {
    std.debug.print("{any}\n", .{_contains_list_int(m, "a")});
    std.debug.print("{any}\n", .{_contains_list_int(m, "c")});
}
