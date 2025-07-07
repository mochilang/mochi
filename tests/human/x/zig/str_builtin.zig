const std = @import("std");

pub fn main() !void {
    var buf: [32]u8 = undefined;
    const s = try std.fmt.bufPrint(&buf, "{d}", .{123});
    std.debug.print("{s}\n", .{s});
}
