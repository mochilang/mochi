const std = @import("std");

var data: []const i32 = undefined;
var flag: bool = undefined;

pub fn main() void {
    data = &[_]i32{ @as(i32, @intCast(1)), @as(i32, @intCast(2)) };
    flag = exists(blk0: {
        var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator);
        for (data) |x| {
            if (!((x == @as(i32, @intCast(1))))) continue;
            _tmp0.append(x) catch unreachable;
        }
        const _tmp1 = _tmp0.toOwnedSlice() catch unreachable;
        break :blk0 _tmp1;
    });
    std.debug.print("{any}\n", .{flag});
}
