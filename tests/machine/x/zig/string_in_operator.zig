// Generated by Mochi compiler v0.10.27 on 2025-07-17T17:59:22Z
const std = @import("std");

fn _contains_list_int(v: []const i32, item: i32) bool {
    for (v) |it| { if (it == item) return true; }
    return false;
}

const s = "catch"; // []const u8

pub fn main() void {
    std.debug.print("{}\n", .{_contains_list_int(s, "cat")});
    std.debug.print("{}\n", .{_contains_list_int(s, "dog")});
}
