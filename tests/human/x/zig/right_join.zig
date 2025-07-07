const std = @import("std");

const Customer = struct { id: i32, name: []const u8 };
const Order = struct { id: i32, customerId: i32, total: i32 };

pub fn main() !void {
    const customers = [_]Customer{
        .{ .id = 1, .name = "Alice" },
        .{ .id = 2, .name = "Bob" },
        .{ .id = 3, .name = "Charlie" },
        .{ .id = 4, .name = "Diana" },
    };
    const orders = [_]Order{
        .{ .id = 100, .customerId = 1, .total = 250 },
        .{ .id = 101, .customerId = 2, .total = 125 },
        .{ .id = 102, .customerId = 1, .total = 300 },
    };

    std.debug.print("--- Right Join using syntax ---\n", .{});
    for (customers) |c| {
        var has_order = false;
        for (orders) |o| {
            if (o.customerId == c.id) {
                has_order = true;
                std.debug.print("Customer {s} has order {d} - ${d}\n", .{ c.name, o.id, o.total });
            }
        }
        if (!has_order) {
            std.debug.print("Customer {s} has no orders\n", .{ c.name });
        }
    }
}
