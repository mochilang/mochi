const std = @import("std");

pub fn main() void {
    std.debug.print("{any}\n", .{std.fmt.allocPrint(std.heap.page_allocator, "{d}", .{@as(i32, @intCast(123))}) catch unreachable});
}
