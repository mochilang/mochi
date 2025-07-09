const std = @import("std");

fn _contains_list_int(v: []const i32, item: i32) bool {
    for (v) |it| { if (it == item) return true; }
    return false;
}

const s = "catch";

pub fn main() void {
    std.debug.print("{}\n", .{_contains_list_int(s, "cat")});
    std.debug.print("{}\n", .{_contains_list_int(s, "dog")});
}
