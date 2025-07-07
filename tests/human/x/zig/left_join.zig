const std = @import("std");

const Customer = struct { id: i32, name: []const u8 };
const Order = struct { id: i32, customerId: i32, total: i32 };

pub fn main() !void {
    const customers = [_]Customer{
        .{ .id = 1, .name = "Alice" },
        .{ .id = 2, .name = "Bob" },
    };
    const orders = [_]Order{
        .{ .id = 100, .customerId = 1, .total = 250 },
        .{ .id = 101, .customerId = 3, .total = 80 },
    };

    std.debug.print("--- Left Join ---\n", .{});
    for (orders) |o| {
        var cust: ?Customer = null;
        for (customers) |c| if (c.id == o.customerId) { cust = c; break; }
        std.debug.print("Order {d} customer {s} total {d}\n",
            .{ o.id, if (cust) |c| c.name else "null", o.total });
    }
}
