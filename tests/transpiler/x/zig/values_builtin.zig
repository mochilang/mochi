// Generated by Mochi Zig transpiler on 2025-07-21 10:58 +0000
const std = @import("std");

pub fn main() void {
    const m = blk: { var m = std.StringHashMap(i64).init(std.heap.page_allocator); m.put("a", 1) catch unreachable; m.put("b", 2) catch unreachable; m.put("c", 3) catch unreachable; break :blk m; };
    try std.io.getStdOut().writer().print("{any}\n", .{blk: { var it = m.iterator(); var arr = std.ArrayList(i64).init(std.heap.page_allocator); while (it.next()) |kv| { arr.append(kv.value) catch unreachable; } break :blk arr.toOwnedSlice(); }});
}
