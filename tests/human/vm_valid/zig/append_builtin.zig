const std = @import("std");

pub fn main() !void {
    var list = std.ArrayList(i32).init(std.heap.page_allocator);
    defer list.deinit();
    try list.appendSlice(&[_]i32{1, 2});
    try list.append(3);
    std.debug.print("{any}\n", .{list.items});
}
