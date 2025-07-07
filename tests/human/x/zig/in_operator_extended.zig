const std = @import("std");

pub fn main() !void {
    const xs = [_]i32{1, 2, 3};
    var odd: [3]i32 = undefined;
    var idx: usize = 0;
    for (xs) |x| {
        if (x % 2 == 1) {
            odd[idx] = x;
            idx += 1;
        }
    }
    const ys = odd[0..idx];
    const has1 = std.mem.indexOfScalar(i32, ys, 1) != null;
    const has2 = std.mem.indexOfScalar(i32, ys, 2) != null;
    std.debug.print("{}\n", .{has1});
    std.debug.print("{}\n", .{has2});

    var map = std.StringHashMap(i32).init(std.heap.page_allocator);
    defer map.deinit();
    try map.put("a", 1);
    std.debug.print("{}\n", .{map.contains("a")});
    std.debug.print("{}\n", .{map.contains("b")});

    const s = "hello";
    std.debug.print("{}\n", .{std.mem.contains(u8, s, "ell")});
    std.debug.print("{}\n", .{std.mem.contains(u8, s, "foo")});
}
