const std = @import("std");

const Customer = struct {
    id: i32,
    name: []const u8,
};

const Order = struct {
    id: i32,
    customerId: i32,
    total: i32,
};

pub fn main() !void {
    const customers = [_]Customer{
        .{ .id = 1, .name = "Alice" },
        .{ .id = 2, .name = "Bob" },
        .{ .id = 3, .name = "Charlie" },
    };

    const orders = [_]Order{
        .{ .id = 100, .customerId = 1, .total = 250 },
        .{ .id = 101, .customerId = 2, .total = 125 },
        .{ .id = 102, .customerId = 1, .total = 300 },
        .{ .id = 103, .customerId = 4, .total = 80 },
    };

    std.debug.print("--- Orders with customer info ---\n", .{});
    for (orders) |o| {
        var cust: ?Customer = null;
        for (customers) |c| {
            if (c.id == o.customerId) {
                cust = c;
                break;
            }
        }
        if (cust) |c| {
            std.debug.print(
                "Order {d} by {s} - ${d}\n",
                .{ o.id, c.name, o.total },
            );
        }
    }
}
