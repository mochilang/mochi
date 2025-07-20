const std = @import("std");

const Customer = struct {
    id: i32,
    name: []const u8,
};

const Order = struct {
    id: i32,
    customer_id: i32,
    total: i32,
};

const Entry = struct {
    order_id: i32,
    order_customer_id: i32,
    paired_customer_name: []const u8,
    order_total: i32,
};

pub fn main() !void {
    const customers = [_]Customer{
        .{ .id = 1, .name = "Alice" },
        .{ .id = 2, .name = "Bob" },
        .{ .id = 3, .name = "Charlie" },
    };

    const orders = [_]Order{
        .{ .id = 100, .customer_id = 1, .total = 250 },
        .{ .id = 101, .customer_id = 2, .total = 125 },
        .{ .id = 102, .customer_id = 1, .total = 300 },
    };

    const result = blk: {
        var arr = std.ArrayList(Entry).init(std.heap.page_allocator);
        for (orders) |o| {
            for (customers) |c| {
                arr.append(.{
                    .order_id = o.id,
                    .order_customer_id = o.customer_id,
                    .paired_customer_name = c.name,
                    .order_total = o.total,
                }) catch unreachable;
            }
        }
        const tmp = arr.toOwnedSlice() catch unreachable;
        break :blk tmp;
    };

    try std.io.getStdOut().writer().print("--- Cross Join: All order-customer pairs ---\n", .{});

    for (result) |entry| {
        try std.io.getStdOut().writer().print(
            "Order {d} (customerId: {d}, total: ${d}) paired with {s}\n",
            .{ entry.order_id, entry.order_customer_id, entry.order_total, entry.paired_customer_name },
        );
    }
}
