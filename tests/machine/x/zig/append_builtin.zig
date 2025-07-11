const std = @import("std");

const a = &[_]i32{
    1,
    2,
};

pub fn main() void {
    std.debug.print("{any}\n", .{blk0: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); defer _tmp0.deinit(); _tmp0.appendSlice(a) catch unreachable; _tmp0.append(3) catch unreachable; break :blk0 _tmp0.toOwnedSlice() catch unreachable; }});
}
