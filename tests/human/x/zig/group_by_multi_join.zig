const std = @import("std");

const Nation = struct { id: i32, name: []const u8 };
const Supplier = struct { id: i32, nation: i32 };
const PartSupp = struct { part: i32, supplier: i32, cost: f64, qty: i32 };

pub fn main() !void {
    const nations = [_]Nation{ .{ .id = 1, .name = "A" }, .{ .id = 2, .name = "B" } };
    const suppliers = [_]Supplier{ .{ .id = 1, .nation = 1 }, .{ .id = 2, .nation = 2 } };
    const partsupp = [_]PartSupp{
        .{ .part = 100, .supplier = 1, .cost = 10.0, .qty = 2 },
        .{ .part = 100, .supplier = 2, .cost = 20.0, .qty = 1 },
        .{ .part = 200, .supplier = 1, .cost = 5.0, .qty = 3 },
    };

    var totals = std.AutoHashMap(i32, f64).init(std.heap.page_allocator);
    defer totals.deinit();

    for (partsupp) |ps| {
        var nation_name: ?[]const u8 = null;
        for (suppliers) |s| if (s.id == ps.supplier) {
            for (nations) |n| if (n.id == s.nation and std.mem.eql(u8, n.name, "A")) {
                nation_name = n.name;
            }
        }
        if (nation_name) |_| {
            const val = ps.cost * @as(f64, @floatFromInt(ps.qty));
            if (totals.getPtr(ps.part)) |t| {
                t.* += val;
            } else {
                try totals.put(ps.part, val);
            }
        }
    }

    var it = totals.iterator();
    while (it.next()) |kv| {
        std.debug.print("{d}: total {d}\n", .{ kv.key, kv.value });
    }
}
