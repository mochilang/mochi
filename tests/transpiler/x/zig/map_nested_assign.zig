// Generated by Mochi Zig transpiler on 2025-07-21 10:57 +0000
const std = @import("std");

pub fn main() void {
    var data = blk: { var m = std.StringHashMap(std.StringHashMap(i64)).init(std.heap.page_allocator); m.put("outer", blk: { var m = std.StringHashMap(i64).init(std.heap.page_allocator); m.put("inner", 1) catch unreachable; break :blk m; }) catch unreachable; break :blk m; };
    data.get("inner").?.put("inner", 2) catch unreachable;
    try std.io.getStdOut().writer().print("{s}\n", .{data.get("outer").?.get("inner").?});
}
