const std = @import("std");

fn handleError(err: anyerror) noreturn {
    std.debug.panic("{any}", .{err});
}

const a = &[_]i32{
    1,
    2,
}; // []const i32

pub fn main() void {
    std.debug.print("{any}\n", .{blk0: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); defer _tmp0.deinit(); _tmp0.appendSlice(a) catch |err| handleError(err); _tmp0.append(3) catch |err| handleError(err); break :blk0 _tmp0.items; }});
}
