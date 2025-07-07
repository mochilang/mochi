const std = @import("std");

const Item = struct {
    n: i32,
    v: []const u8,
};

pub fn main() !void {
    var items = [_]Item{
        .{ .n = 1, .v = "a" },
        .{ .n = 1, .v = "b" },
        .{ .n = 2, .v = "c" },
    };

    // simple stable bubble sort by `n`
    var i: usize = 0;
    while (i < items.len) : (i += 1) {
        var j: usize = 0;
        while (j + 1 < items.len - i) : (j += 1) {
            if (items[j + 1].n < items[j].n) {
                const tmp = items[j];
                items[j] = items[j + 1];
                items[j + 1] = tmp;
            }
        }
    }

    var result: [3][]const u8 = undefined;
    for (items) |it, idx| {
        result[idx] = it.v;
    }
    std.debug.print("{any}\n", .{result});
}
