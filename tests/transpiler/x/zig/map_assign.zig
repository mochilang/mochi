// Generated by Mochi Zig transpiler on 2025-07-21 10:57 +0000
const std = @import("std");

pub fn main() void {
    var scores = blk: { var m = std.StringHashMap(i64).init(std.heap.page_allocator); m.put("alice", 1) catch unreachable; break :blk m; };
    scores.put("bob", 2) catch unreachable;
    try std.io.getStdOut().writer().print("{s}\n", .{scores.get("bob").?});
}
