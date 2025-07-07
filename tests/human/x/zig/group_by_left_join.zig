const std = @import("std");

const Customer = struct { id: i32, name: []const u8 };
const Order = struct { id: i32, customerId: i32 };

pub fn main() !void {
    const customers = [_]Customer{
        .{ .id = 1, .name = "Alice" },
        .{ .id = 2, .name = "Bob" },
        .{ .id = 3, .name = "Charlie" },
    };
    const orders = [_]Order{
        .{ .id = 100, .customerId = 1 },
        .{ .id = 101, .customerId = 1 },
        .{ .id = 102, .customerId = 2 },
    };

    var counts = std.StringHashMap(i32).init(std.heap.page_allocator);
    defer counts.deinit();

    for (customers) |c| {
        var cnt: i32 = 0;
        for (orders) |o| if (o.customerId == c.id) cnt += 1;
        try counts.put(c.name, cnt);
    }

    std.debug.print("--- Group Left Join ---\n", .{});
    var it = counts.iterator();
    while (it.next()) |kv| {
        std.debug.print("{s} orders: {d}\n", .{ kv.key, kv.value });
    }
}
