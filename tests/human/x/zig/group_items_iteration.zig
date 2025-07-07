const std = @import("std");

const Entry = struct {
    tag: []const u8,
    val: i32,
};

pub fn main() !void {
    const data = [_]Entry{
        .{ .tag = "a", .val = 1 },
        .{ .tag = "a", .val = 2 },
        .{ .tag = "b", .val = 3 },
    };

    var totals = std.StringHashMap(i32).init(std.heap.page_allocator);
    defer totals.deinit();

    for (data) |d| {
        if (totals.getPtr(d.tag)) |p| {
            p.* += d.val;
        } else {
            try totals.put(d.tag, d.val);
        }
    }

    var groups = std.ArrayList(Entry).init(std.heap.page_allocator);
    defer groups.deinit();

    var it = totals.iterator();
    while (it.next()) |kv| {
        try groups.append(.{ .tag = kv.key, .val = kv.value });
    }

    std.sort.sort(Entry, groups.items, {}, struct {
        fn lessThan(ctx: void, a: Entry, b: Entry) bool {
            _ = ctx;
            return std.mem.lessThan(u8, a.tag, b.tag);
        }
    }.lessThan);

    for (groups.items) |g| {
        std.debug.print("{s}: {d}\n", .{ g.tag, g.val });
    }
}
