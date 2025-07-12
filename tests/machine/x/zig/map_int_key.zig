const std = @import("std");

fn handleError(err: anyerror) noreturn {
    std.debug.panic("{any}", .{err});
}

const m = (blk0: { var _map0 = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); _map0.put(1, "a") catch |err| handleError(err); _map0.put(2, "b") catch |err| handleError(err); break :blk0 _map0; }); // std.AutoHashMap(i32, []const u8)

pub fn main() void {
    std.debug.print("{s}\n", .{m[1]});
}
