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
    const items = [_]Item{
        .{ .orderId = 100, .sku = "a" },
        .{ .orderId = 101, .sku = "b" },
    };

    std.debug.print("--- Multi Join ---\n", .{});
    for (orders) |o| {
        var cust: ?Customer = null;
        for (customers) |c| if (c.id == o.customerId) { cust = c; break; }
        if (cust == null) continue;
        for (items) |it| {
            if (it.orderId == o.id) {
                std.debug.print("{s} bought item {s}\n", .{ cust.?.name, it.sku });
            }
        }
    }
}
