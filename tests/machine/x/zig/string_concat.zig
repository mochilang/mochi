const std = @import("std");

fn _concat_string(a: []const u8, b: []const u8) []const u8 {
    var res = std.ArrayList(u8).init(std.heap.page_allocator);
    defer res.deinit();
    res.appendSlice(a) catch unreachable;
    res.appendSlice(b) catch unreachable;
    return res.toOwnedSlice() catch unreachable;
}

pub fn main() void {
    std.debug.print("{s}\n", .{_concat_string("hello ", "world")});
}
