const std = @import("std");

pub fn main() void {
    std.debug.print("{}\n", .{std.mem.order(u8, "a", "b") == .lt});
    std.debug.print("{}\n", .{std.mem.order(u8, "a", "a") != .gt});
    std.debug.print("{}\n", .{std.mem.order(u8, "b", "a") == .gt});
    std.debug.print("{}\n", .{std.mem.order(u8, "b", "b") != .lt});
}
