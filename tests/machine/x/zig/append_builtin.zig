const std = @import("std");

fn _append(comptime T: type, v: []const T, x: T) []T {
    var res = std.ArrayList(T).init(std.heap.page_allocator);
    defer res.deinit();
    res.appendSlice(v) catch unreachable;
    res.append(x) catch unreachable;
    return res.toOwnedSlice() catch unreachable;
}

const a = &[_]i32{
    1,
    2,
};

pub fn main() void {
    std.debug.print("{any}\n", .{_append(i32, a, 3)});
}
