const std = @import("std");

fn _map_values(comptime K: type, comptime V: type, m: std.AutoHashMap(K, V)) []V {
    var res = std.ArrayList(V).init(std.heap.page_allocator);
    defer res.deinit();
    var it = m.valueIterator();
    while (it.next()) |v_ptr| {
        res.append(v_ptr.*) catch unreachable;
    }
    return res.toOwnedSlice() catch unreachable;
}

var m: std.AutoHashMap([]const u8, i32) = undefined;

pub fn main() void {
    m = blk0: {
        var m = std.AutoHashMap([]const u8, i32).init(std.heap.page_allocator);
        m.put("a", @as(i32, @intCast(1))) catch unreachable;
        m.put("b", @as(i32, @intCast(2))) catch unreachable;
        m.put("c", @as(i32, @intCast(3))) catch unreachable;
        break :blk0 m;
    };
    std.debug.print("{any}\n", .{_map_values([]const u8, i32, m)});
}
