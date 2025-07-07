const std = @import("std");

fn _contains_list_int(v: []const i32, item: i32) bool {
    for (v) |it| {
        if (it == item) return true;
    }
    return false;
}

var s: []const u8 = undefined;

pub fn main() void {
    s = "catch";
    std.debug.print("{s}\n", .{_contains_list_int(s, "cat")});
    std.debug.print("{s}\n", .{_contains_list_int(s, "dog")});
}
