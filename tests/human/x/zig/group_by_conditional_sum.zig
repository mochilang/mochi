const std = @import("std");

const Item = struct { cat: []const u8, val: i32, flag: bool };
const Res = struct { cat: []const u8, share: f64 };

pub fn main() !void {
    const items = [_]Item{
        .{ .cat = "a", .val = 10, .flag = true },
        .{ .cat = "a", .val = 5, .flag = false },
        .{ .cat = "b", .val = 20, .flag = true },
    };

    var groups = std.StringHashMap(struct { pos: i32, total: i32 }).init(std.heap.page_allocator);
    defer groups.deinit();

    for (items) |it| {
        if (groups.getPtr(it.cat)) |g| {
            if (it.flag) g.pos += it.val;
            g.total += it.val;
        } else {
            try groups.put(it.cat, .{ .pos = it.flag ? it.val : 0, .total = it.val });
        }
    }

    var res = std.ArrayList(Res).init(std.heap.page_allocator);
    defer res.deinit();

    var it = groups.iterator();
    while (it.next()) |kv| {
        const share = @as(f64, @floatFromInt(kv.value.pos)) / @as(f64, @floatFromInt(kv.value.total));
        try res.append(.{ .cat = kv.key, .share = share });
    }

    std.sort.sort(Res, res.items, {}, struct {
        fn lt(ctx: void, a: Res, b: Res) bool { _ = ctx; return std.mem.lessThan(u8, a.cat, b.cat); }
    }.lt);

    for (res.items) |r| {
        std.debug.print("{s}: {d:.3}\n", .{ r.cat, r.share });
    }
}
