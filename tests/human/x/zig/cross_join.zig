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

const Entry = struct {
    orderId: i32,
    orderCustomerId: i32,
    pairedCustomerName: []const u8,
    orderTotal: i32,
};

pub fn main() !void {
    var customers = [_]Customer{
        .{ .id = 1, .name = "Alice" },
        .{ .id = 2, .name = "Bob" },
        .{ .id = 3, .name = "Charlie" },
    };

    var orders = [_]Order{
        .{ .id = 100, .customerId = 1, .total = 250 },
        .{ .id = 101, .customerId = 2, .total = 125 },
        .{ .id = 102, .customerId = 1, .total = 300 },
    };

    var result = std.ArrayList(Entry).init(std.heap.page_allocator);
    defer result.deinit();

    for (orders) |o| {
        for (customers) |c| {
            try result.append(.{
                .orderId = o.id,
                .orderCustomerId = o.customerId,
                .pairedCustomerName = c.name,
                .orderTotal = o.total,
            });
        }
    }

    std.debug.print("--- Cross Join: All order-customer pairs ---\n", .{});
    for (result.items) |entry| {
        std.debug.print(
            "Order {d} (customerId: {d}, total: ${d}) paired with {s}\n",
            .{ entry.orderId, entry.orderCustomerId, entry.orderTotal, entry.pairedCustomerName },
        );
    }
}

