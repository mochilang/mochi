const std = @import("std");

fn _contains_list_int(v: []const i32, item: i32) bool {
    for (v) |it| { if (it == item) return true; }
    return false;
}

const m = (blk0: { var _map0 = std.StringHashMap(i32).init(std.heap.page_allocator); _map0.put("a", 1) catch unreachable; _map0.put("b", 2) catch unreachable; break :blk0 _map0; });

pub fn main() void {
    std.debug.print("{any}\n", .{_contains_list_int(m, "a")});
    std.debug.print("{any}\n", .{_contains_list_int(m, "c")});
}
