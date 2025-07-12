const std = @import("std");

const CustomersItem = struct {
    id: i32,
    name: []const u8,
};
const customers = &[_]CustomersItem{
    CustomersItem{
    .id = 1,
    .name = "Alice",
},
    CustomersItem{
    .id = 2,
    .name = "Bob",
},
    CustomersItem{
    .id = 3,
    .name = "Charlie",
},
    CustomersItem{
    .id = 4,
    .name = "Diana",
},
}; // []const CustomersItem
const OrdersItem = struct {
    id: i32,
    customerId: i32,
    total: i32,
};
const orders = &[_]OrdersItem{
    OrdersItem{
    .id = 100,
    .customerId = 1,
    .total = 250,
},
    OrdersItem{
    .id = 101,
    .customerId = 2,
    .total = 125,
},
    OrdersItem{
    .id = 102,
    .customerId = 1,
    .total = 300,
},
}; // []const OrdersItem
const result = blk0: { var _tmp0 = std.ArrayList(struct {
    customerName: []const u8,
    order: OrdersItem,
}).init(std.heap.page_allocator); for (orders) |o| { var matched = false; for (customers) |c| { if (!((o.customerId == c.id))) continue; matched = true; _tmp0.append(struct {
    customerName: []const u8,
    order: OrdersItem,
}{
    .customerName = c.name,
    .order = o,
}) catch unreachable; } if (!matched) { const c: ?CustomersItem = null; _tmp0.append(struct {
    customerName: []const u8,
    order: OrdersItem,
}{
    .customerName = c.name,
    .order = o,
}) catch unreachable; } } const res = _tmp0.toOwnedSlice() catch unreachable; break :blk0 res; }; // []const std.StringHashMap(i32)

pub fn main() void {
    std.debug.print("--- Right Join using syntax ---\n", .{});
    for (result) |entry| {
        if (entry.order) {
            std.debug.print("Customer {any} has order {any} - $ {any}\n", .{entry.customerName, entry.order.id, entry.order.total});
        } else {
            std.debug.print("Customer {any} has no orders\n", .{entry.customerName});
        }
    }
}
