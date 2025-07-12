const std = @import("std");

fn handleError(err: anyerror) noreturn {
    std.debug.panic("{any}", .{err});
}

const data = &[_]i32{
    1,
    2,
}; // []const i32
const flag = (blk0: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (data) |x| { if (!((x == 1))) continue; _tmp0.append(x) catch |err| handleError(err); } const _tmp1 = _tmp0.toOwnedSlice() catch |err| handleError(err); break :blk0 _tmp1; }).len != 0; // bool

pub fn main() void {
    std.debug.print("{}\n", .{flag});
}
