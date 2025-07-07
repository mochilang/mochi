const std = @import("std");

const Customer = struct { id: i32, name: []const u8 };
const Order = struct { id: i32, customerId: i32 };

pub fn main() !void {
    const customers = [_]Customer{
        .{ .id = 1, .name = "Alice" },
        .{ .id = 2, .name = "Bob" },
    };
    const orders = [_]Order{
        .{ .id = 100, .customerId = 1 },
        .{ .id = 101, .customerId = 1 },
        .{ .id = 102, .customerId = 2 },
    };

    var counts = std.StringHashMap(i32).init(std.heap.page_allocator);
    defer counts.deinit();

    for (orders) |o| {
        for (customers) |c| {
            if (c.id == o.customerId) {
                if (counts.getPtr(c.name)) |p| {
                    p.* += 1;
                } else {
                    try counts.put(c.name, 1);
                }
            }
        }
    }

    std.debug.print("--- Orders per customer ---\n", .{});
    var it = counts.iterator();
    while (it.next()) |kv| {
        std.debug.print("{s} orders: {d}\n", .{ kv.key, kv.value });
    }
}
