// Generated by Mochi Zig transpiler on 2025-07-21 10:57 +0000
const std = @import("std");

pub fn main() void {
    const x = 3;
    const y = 4;
    const m = blk: { var m = std.StringHashMap(i64).init(std.heap.page_allocator); m.put("a", x) catch unreachable; m.put("b", y) catch unreachable; break :blk m; };
    try std.io.getStdOut().writer().print("{s} {s}\n", .{m.get("a").?, m.get("b").?});
}
