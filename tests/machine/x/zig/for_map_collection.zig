const std = @import("std");

fn handleError(err: anyerror) noreturn {
    std.debug.panic("{any}", .{err});
}

var m: std.StringHashMap(i32) = undefined; // std.StringHashMap(i32)

pub fn main() void {
    var _tmp1 = m.keyIterator();
    while (_tmp1.next()) |k_ptr| {
        const k = k_ptr.*;
        std.debug.print("{s}\n", .{k});
    }
}
