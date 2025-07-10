const std = @import("std");

var m: std.StringHashMap(i32) = undefined;

pub fn main() void {
    var _tmp1 = m.keyIterator();
    while (_tmp1.next()) |k_ptr| {
        const k = k_ptr.*;
        std.debug.print("{s}\n", .{k});
    }
}
