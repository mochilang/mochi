const std = @import("std");

pub fn main() void {
    std.debug.print("{s}\n", .{std.fmt.allocPrint(std.heap.page_allocator, "{d}", .{123}) catch unreachable});
}
