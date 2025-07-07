const std = @import("std");

const Customer = struct { id: i32, name: []const u8 };
const Order = struct { id: i32, customerId: i32 };
const Item = struct { orderId: i32, sku: []const u8 };

pub fn main() !void {
    const customers = [_]Customer{
        .{ .id = 1, .name = "Alice" },
        .{ .id = 2, .name = "Bob" },
    };
    const orders = [_]Order{
        .{ .id = 100, .customerId = 1 },
        .{ .id = 101, .customerId = 2 },
    };
    const items = [_]Item{ .{ .orderId = 100, .sku = "a" } };

    std.debug.print("--- Left Join Multi ---\n", .{});
    for (orders) |o| {
        var cust: ?Customer = null;
        for (customers) |c| if (c.id == o.customerId) { cust = c; break; }
        var item: ?Item = null;
        for (items) |it| if (it.orderId == o.id) { item = it; break; }
        std.debug.print("{d} {s} {s}\n",
            .{ o.id, cust.?.name, if (item) |it| it.sku else "null" });
    }
}
