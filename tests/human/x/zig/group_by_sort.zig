const std = @import("std");

const Item = struct { cat: []const u8, val: i32 };
const Res = struct { cat: []const u8, total: i32 };

pub fn main() !void {
    const items = [_]Item{
        .{ .cat = "a", .val = 3 },
        .{ .cat = "a", .val = 1 },
        .{ .cat = "b", .val = 5 },
        .{ .cat = "b", .val = 2 },
    };

    var totals = std.StringHashMap(i32).init(std.heap.page_allocator);
    defer totals.deinit();

    for (items) |it| {
        if (totals.getPtr(it.cat)) |t| {
            t.* += it.val;
        } else {
            try totals.put(it.cat, it.val);
        }
    }

    var arr = std.ArrayList(Res).init(std.heap.page_allocator);
    defer arr.deinit();
    var it = totals.iterator();
    while (it.next()) |kv| {
        try arr.append(.{ .cat = kv.key, .total = kv.value });
    }

    std.sort.sort(Res, arr.items, {}, struct {
        fn lt(ctx: void, a: Res, b: Res) bool { _ = ctx; return a.total > b.total; }
    }.lt);

    for (arr.items) |r| {
        std.debug.print("{s}: {d}\n", .{ r.cat, r.total });
    }
}
